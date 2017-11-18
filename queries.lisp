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
  (let ((grammar (parse-grammar (query-grammar query))))
    (multiple-value-bind (defined used) (grammar-defined/used-definitions grammar)
      (make-instance 'unused-definitions-response
                     :unused-definitions (set-difference defined used :test #'string=)))))


(defclass missing-definitions (query)
  ())

(defclass missing-definitions-response (query)
  ((missing-definitions :accessor response-missing-definitions
                        :initarg :missing-definitions
                        :type list)))


(defmethod execute ((query missing-definitions))
  (let ((grammar (parse-grammar (query-grammar query))))
    (multiple-value-bind (defined used) (grammar-defined/used-definitions grammar)
      (make-instance 'missing-definitions-response
                     :missing-definitions (set-difference used defined :test #'string=)))))


(declaim (ftype (function (grammar) (values list list)) grammar-defined/used-definitions))
(defun grammar-defined/used-definitions (grammar)
  (let ((defined) (used))
    (traverse grammar
              (lambda (el)
                (typecase el
                  (rule (pushnew (rule-name el) defined :test #'equal))
                  (non-terminal (pushnew (grammar-symbol-value el) used :test #'equal)))))
    (values defined used)))
