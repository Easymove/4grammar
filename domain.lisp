(in-package :4grammar)

(defclass grammar-definition ()
  ())

(defclass grammar (grammar-definition)
  ((name :accessor grammar-name
         :initarg :name
         :type non-terminal)
   (rules :accessor grammar-rules
          :initarg :rules
          :type list)))

(defclass rule (grammar-definition)
  ((name :accessor rule-name
         :initarg :name
         :type non-terminal)
   (alternatives :accessor rule-alternatives
                 :initarg :alternatives
                 :type list)
   (channel :accessor rule-channel
            :initarg :channel
            :type keyword)))

(defclass alias (rule)
  ())

(defclass alternative (grammar-definition)
  ((entities :accessor alternative-entities
             :initarg :entities
             :type list)))

(defclass entity-base (grammar-definition)
  ())

(defclass entity-with-mod-base (entity-base)
  ((mod :accessor entity-mod
        :type keyword
        :initarg :mod)))

(defclass negatable-entity-base (entity-with-mod-base)
  ((negated? :accessor entity-negated?
             :initarg :negated?
             :type boolean
             :initform nil)))

(defclass simple-entity (negatable-entity-base)
  ((value :accessor entity-value
          :initarg :value
          :type grammar-symbol)))

(defclass set-entity (negatable-entity-base)
  ((set :accessor entity-set
        :initarg :set
        :type list)))

(defclass wildcard-entity (entity-with-mod-base)
  ())

(defclass island-entity-base (entity-base)
  ((island :accessor entity-island
           :initarg :island
           :type string)))

(defclass action-entity (island-entity-base)
  ())

(defclass predicate-entity (island-entity-base)
  ())

(defclass range-entity (negatable-entity-base)
  ((from :accessor range-from
         :initarg :from
         :type char)
   (to :accessor range-to
       :initarg :to
       :type char)
   (set :accessor range-set
        :initarg :set
        :type list)))

(defclass complex-entity (negatable-entity-base)
  ((value :accessor entity-value
          :initarg :value
          :type list)))

(defclass grammar-symbol (grammar-definition)
  ((value :accessor grammar-symbol-value
          :initarg :value
          :type string)))

(defclass terminal (grammar-symbol)
  ())

(defclass non-terminal (grammar-symbol)
  ())

;;; ----------------------------------------------------------------------------
;;; printers
;;; ----------------------------------------------------------------------------
(defmethod print-object ((term terminal) stream)
  (format stream "'~A'" (grammar-symbol-value term)))

(defmethod print-object ((term non-terminal) stream)
  (format stream "~A" (grammar-symbol-value term)))

(defmethod print-object ((term entity-with-mod-base) stream)
  (format stream "#<~A~@[~A~]: ~A>" (type-of term) (entity-mod term) (entity-value term)))

(defmethod print-object ((term negatable-entity-base) stream)
  (format stream "#<~A~@[~A~]: ~@[~A~] ~A>" (type-of term) (entity-mod term) (entity-negated? term) (entity-value term)))

(defmethod print-object ((term set-entity) stream)
  (format stream "#<~A~@[~A~]: ~@[~A~] ~A>" (type-of term) (entity-mod term) (entity-negated? term) (entity-set term)))

(defmethod print-object ((term wildcard-entity) stream)
  (format stream "#<~A~@[~A~]>" (type-of term) (entity-mod term)))

(defmethod print-object ((term island-entity-base) stream)
  (format stream "#<~A: ~A>" (type-of term) (entity-island term)))

(defmethod print-object ((term range-entity) stream)
  (format stream "#<~A~@[~A~]: '~A' to '~A'>" (type-of term) (entity-mod term) (range-from term) (range-to term)))

(defmethod print-object ((term alternative) stream)
  (format stream "#<~A: ~A>" (type-of term) (alternative-entities term)))

(defmethod print-object ((term rule) stream)
  (format stream "#<~A ~A: ~A>" (type-of term) (rule-name term) (rule-alternatives term)))

(defmethod print-object ((term grammar) stream)
  (format stream "#<~A: ~A>" (type-of term) (grammar-rules term)))

;;; ----------------------------------------------------------------------------
;;; Maps
;;; ----------------------------------------------------------------------------
(defgeneric map-members (node function)
  (:documentation "Maps over the children of the given grammar construction."))

(defmethod map-members ((node grammar) function)
  (mapc function (grammar-rules node)))

(defmethod map-members ((node rule) function)
  (mapc function (rule-alternatives node)))

(defmethod map-members ((node alternative) function)
  (mapc function (alternative-entities node)))

(defmethod map-members ((node complex-entity) function)
  (mapc function (entity-value node)))

(defmethod map-members ((node simple-entity) function)
  (funcall function (entity-value node)))

(defgeneric traverse (node function)
  (:documentation "Traverse all nodes of the given grammar or any other grammar definition."))

(defmethod traverse ((node grammar-definition) function)
  (funcall function node)
  (map-members node (rcurry #'traverse function)))

(defmethod traverse ((node grammar-symbol) function)
  (funcall function node))

(defmethod traverse ((node range-entity) function)
  (funcall function node))

(defmethod traverse ((node set-entity) function)
  (funcall function node))

(defmethod traverse ((node island-entity-base) function)
  (funcall function node))

(defmethod traverse ((node wildcard-entity) function)
  (funcall function node))
