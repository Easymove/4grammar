(in-package #:4grammar)

;;; ----------------------------------------------------------------------------
;;; Base classes
;;; ----------------------------------------------------------------------------
(defclass query ()
  ((grammar :accessor query-grammar
            :initarg :grammar
            :type string)))

(defclass response ()
  ())


(declaim (ftype (function (query) t) compute-or-get-query))
(defun compute-or-get-query (query)
  (multiple-value-bind (val found)
      (get-from-storage (list (class-of query) (query-grammar query)))
    (if found
        val
        (execute query))))


(declaim (ftype (function (string) (or grammar nil)) get-grammar))
(defun get-grammar (string)
  (multiple-value-bind (val found)
      (get-from-storage string)
    (if found
        val
        (let ((res (parse-grammar string)))
          (set-to-storage string res)
          res))))

;;; ----------------------------------------------------------------------------
;;; Unused and missing definitions query
;;; ----------------------------------------------------------------------------
(defclass unused-definitions (query)
  ())

(defclass unused-definitions-response (response)
  ((unused-definitions :accessor response-unused-definitions
                       :initarg :unused-definitions
                       :type list)))


(defmethod execute ((query unused-definitions))
  (let ((grammar (get-grammar (query-grammar query))))
    (multiple-value-bind (defined used) (grammar-defined/used-definitions grammar)
      (make-instance 'unused-definitions-response
                     :unused-definitions (set-difference defined used :test #'string=)))))


(defclass missing-definitions (query)
  ())

(defclass missing-definitions-response (response)
  ((missing-definitions :accessor response-missing-definitions
                        :initarg :missing-definitions
                        :type list)))


(defmethod execute ((query missing-definitions))
  (let ((grammar (get-grammar (query-grammar query))))
    (multiple-value-bind (defined used) (grammar-defined/used-definitions grammar)
      (make-instance 'missing-definitions-response
                     :missing-definitions (set-difference used defined :test #'string=)))))

(defparameter +predefined-tokens+ (list "EOF"))

(declaim (ftype (function (grammar) (values list list)) grammar-defined/used-definitions))
(defun grammar-defined/used-definitions (grammar)
  (let ((defined) (used)
        (first-rule t))
    (traverse grammar
              (lambda (el)
                (typecase el
                  (rule
                   (if (not first-rule)
                       (pushnew (rule-name el) defined :test #'equal)
                       (setf first-rule nil))
                   (when (rule-command el)
                     (pushnew (rule-name el) used :test #'equal)))
                  (non-terminal
                   (unless (member (grammar-symbol-value el) +predefined-tokens+ :test #'equal)
                     (pushnew (grammar-symbol-value el) used :test #'equal))))))
    (values defined used)))

;;; ----------------------------------------------------------------------------
;;; Visualize grammar query
;;; ----------------------------------------------------------------------------
(defclass draw-grammar (query)
  ())

(defclass draw-grammar-response (response)
  ((dot :accessor response-dot
        :initarg :dot
        :type string)))


(defmethod execute ((query draw-grammar))
  (let ((grammar (get-grammar (query-grammar query)))
        (graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed)))
    (labels ((%name (el)
               (etypecase el
                 (rule (rule-name el))
                 (grammar-symbol (grammar-symbol-value el))))
             (%draw-children (rule)
               (aif (cl-graph:find-vertex-if
                     graph
                     (curry #'equal (%name rule))
                     :key (compose #'%name #'cl-graph:element))
                    it
                    (prog1
                        (cl-graph:add-vertex graph rule)
                      (when (typep rule 'rule)
                        (traverse rule (lambda (el)
                                         (typecase el
                                           (non-terminal
                                            (let ((next-rule (lookup (grammar-symbol-value el) grammar)))
                                              (when next-rule
                                                (%draw-children next-rule))
                                              (cl-graph:add-edge-between-vertexes
                                               graph rule (or next-rule el))))
                                           (terminal
                                            (cl-graph:add-edge-between-vertexes
                                             graph rule (%draw-children el)))))))))))
      (mapc #'%draw-children (grammar-rules grammar))
      (make-instance 'draw-grammar-response
                     :dot (with-output-to-string (str)
                            ;; TODO: extend with more customizations
                            (cl-graph:graph->dot graph str
                                                 :edge-labeler nil
                                                 :vertex-labeler
                                                 (lambda (v stream)
                                                   (format stream "~a" (%name (cl-graph:element v))))
                                                 :vertex-formatter
                                                 (lambda (v stream)
                                                   (format stream "color=~A"
                                                           (awhen (cl-graph:element v)
                                                             (etypecase it
                                                               (rule
                                                                (if (= (length (cl-graph:parent-vertexes v)) 0)
                                                                    "blue"
                                                                    "black"))
                                                               (grammar-symbol
                                                                (if (member (grammar-symbol-value it)
                                                                            +predefined-tokens+ :test #'equal)
                                                                    "black"
                                                                    "red"))))))))))))


(declaim (ftype (function (string grammar &optional symbol) t) lookup))
(defun lookup (name grammar &optional (type 'rule))
  (traverse grammar
            (lambda (el)
              (when (and (typep el type)
                         (equal (rule-name el) name))
                (return-from lookup el))))
  nil)

;;; ----------------------------------------------------------------------------
;;; First/follow sets queries
;;; ----------------------------------------------------------------------------

(defclass rule-first-set (query)
  ((rule-name :accessor query-rule-name
              :initarg :rule-name
              :type string)))

(defclass rule-first-set-response (response)
  ((rule-first-set :accessor response-rule-first-set
                   :initarg :rule-first-set
                   :type list)))


(defmethod execute ((query rule-first-set))
  (let ((grammar (get-grammar (query-grammar query))))
    (make-instance 'rule-first-set-response
                   :rule-first-set (first-set (query-rule-name query) grammar))))

(defgeneric first-set (target grammar))

(defmethod first-set ((target null) (grammar grammar))
  nil)

(defmethod first-set ((target string) (grammar grammar))
  (first-set (lookup target grammar) grammar))

(defun first-set-for-terminal (el)
  (list el))

(defmethod first-set ((target terminal) (grammar grammar))
  (first-set-for-terminal target))

(defmethod first-set ((target token-name) (grammar grammar))
  (first-set-for-terminal target))

(defmethod first-set ((target range-entity) (grammar grammar))
  (first-set-for-terminal target))

(defmethod first-set ((target set-entity) (grammar grammar))
  (first-set-for-terminal target))

(defmethod first-set ((target rule-name) (grammar grammar))
  (let ((name (grammar-symbol-value target)))
    (values (first-set name grammar) (has-empty-command? name grammar))))

(defmethod first-set ((target grammar-definition) (grammar grammar))
  (let ((empty-p t) (acc nil))
    (traverse
     target
     (lambda (el)
       (unless (eq el target)
         (if empty-p
             (multiple-value-bind (res has-eps?) (first-set el grammar)
               (setf empty-p has-eps?)
               (setf acc (union acc res :test #'grammar-symbol-equal)))
             (return-from first-set (values acc empty-p))))))))


(defgeneric has-empty-command? (target grammar))

(defmethod has-empty-command? ((target null) (grammar grammar))
  nil)

(defmethod has-empty-command? ((target string) (grammar grammar))
  (has-empty-command? (lookup target grammar) grammar))

(defmethod has-empty-command? ((target rule) (grammar grammar))
  (traverse target
            (lambda (el)
              (when (or (and (typep el 'entity-with-mod-base)
                             (or (eq (entity-mod el) #\?)
                                 (eq (entity-mod el) #\*)))
                        (and (typep el 'alternative)
                             (null (alternative-entities el))))
                (return-from has-empty-command? t))))
  nil)



(defclass rule-follow-set (query)
  ((rule-name :accessor query-rule-name
              :initarg :rule-name
              :type string)))

(defclass rule-follow-set-response (response)
  ((rule-follow-set :accessor response-rule-follow-set
                    :initarg :rule-follow-set
                    :type list)))


(defmethod execute ((query rule-follow-set))
  (let ((grammar (get-grammar (query-grammar query))))
    (make-instance 'rule-follow-set-response
                   :rule-follow-set (follow-set (query-rule-name query) grammar))))

(defvar *found* nil)

(defgeneric follow-set (target grammar &optional visited))

(defmethod follow-set ((target null) (grammar grammar) &optional (visited (make-hash-table)))
  (declare (ignore visited))
  nil)

(defmethod follow-set ((target string) (grammar grammar) &optional (visited (make-hash-table)))
  (follow-set (lookup target grammar) grammar visited))

(defmethod follow-set ((target grammar-symbol) (grammar grammar) &optional (visited (make-hash-table)))
  (follow-set (grammar-symbol-value target) grammar visited))

;;; TODO: Optimize me somehow
;;; TODO: Also take care of complex entities repetition
(defmethod follow-set ((target rule) (grammar grammar) &optional (visited (make-hash-table)))
  (let ((acc))
    (unless (gethash target visited)
      (setf (gethash target visited) t)
      (traverse
       grammar
       (lambda (rule)
         (when (typep rule 'rule)
           (traverse
            rule
            (lambda (alt)
              (when (typep alt 'alternative)
                (let ((found))
                  (traverse
                   alt
                   (lambda (el)
                     (when (typep el 'simple-entity)
                       (let ((sym (entity-value el)))
                         (if found
                             (progn
                               (setf acc (union acc (first-set sym grammar)
                                                :test #'grammar-symbol-equal))
                               (setf found (has-empty-command? (grammar-symbol-value sym) grammar)))
                             (when (equal (grammar-symbol-value sym)
                                          (rule-name target))
                               (when (or (eq (entity-mod el) #\+)
                                         (eq (entity-mod el) #\*))
                                 (setf acc (union acc (first-set sym grammar)
                                                  :test #'grammar-symbol-equal)))
                               (setf acc (union acc (follow-set rule grammar visited)
                                                :test #'grammar-symbol-equal))
                               (setf found t)))))))))))))))
    acc))
