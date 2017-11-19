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


(defparameter +hidden-channels+ (list "HIDDEN" "skip"))
(defparameter +predefined-tokens+ (list "EOF"))

(declaim (ftype (function (grammar) (values list list)) grammar-defined/used-definitions))
(defun grammar-defined/used-definitions (grammar)
  (let ((defined) (used)
        (first-rule t))
    (traverse grammar
              (lambda (el)
                (typecase el
                  (rule
                   (if (not (or (some (curry #'equal (rule-channel el)) +hidden-channels+)
                                first-rule))
                       (pushnew (rule-name el) defined :test #'equal)
                       (setf first-rule nil)))
                  (non-terminal
                   (unless (member (grammar-symbol-value el) +predefined-tokens+ :test #'equal)
                     (pushnew (grammar-symbol-value el) used :test #'equal))))))
    (values defined used)))
