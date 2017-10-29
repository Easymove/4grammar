(in-package :4grammar)

(defclass grammar ()
  ((rules :accessor grammar-rules
          :initarg :rules
          :type list)))

(defclass rule ()
  ((name :accessor rule-name
         :initarg :name
         :type non-terminal)
   (commands :accessor rule-commands
             :initarg :commands
             :type list)))

(defclass command ()
  ((entities :accessor command-entities
             :initarg :entities
             :type list)))

(defclass entity-base ()
  ((mod :accessor entity-mod
        :type keyword
        :initarg :mod)))

(defclass simple-entity (entity-base)
  ((value :accessor entity-value
          :initarg :value
          :type grammar-symbol)))

(defclass complex-entity (entity-base)
  ((value :accessor entity-value
          :initarg :value
          :type list)))

(defclass grammar-symbol ()
  ((value :accessor grammar-symbol-value
          :initarg :value
          :type string)))

(defclass terminal (grammar-symbol)
  ())

(defclass non-terminal (grammar-symbol)
  ())

;;; -----------------------------------------------------------------------------
;;; printers
;;; -----------------------------------------------------------------------------
(defmethod print-object ((term grammar-symbol) stream)
  (format stream "#<~A: ~A>" (type-of term) (grammar-symbol-value term)))

(defmethod print-object ((term entity-base) stream)
  (format stream "#<~A~@[~A~]: ~A>" (type-of term) (entity-mod term) (entity-value term)))

(defmethod print-object ((term command) stream)
  (format stream "#<~A: ~A>" (type-of term) (command-entities term)))

(defmethod print-object ((term rule) stream)
  (format stream "#<~A ~A: ~A>" (type-of term) (rule-name term) (rule-commands term)))

(defmethod print-object ((term grammar) stream)
  (format stream "#<~A: ~A>" (type-of term) (grammar-rules term)))
