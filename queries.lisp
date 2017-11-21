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
    (labels ((%lookup (name)
               (traverse grammar
                         (lambda (el)
                           (when (and (typep el 'rule)
                                      (equal (rule-name el) name))
                             (return-from %lookup el))))
               nil)
             (%name (el)
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
                                            (let ((next-rule (%lookup (grammar-symbol-value el))))
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
