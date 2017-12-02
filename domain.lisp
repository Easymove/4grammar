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
   (command :accessor rule-command
            :initarg :command
            :type keyword)
   (args :accessor rule-args
         :initarg :args
         :type (or string nil)
         :initform nil)
   (returns :accessor rule-returns
            :initarg :returns
            :type (or string nil)
            :initform nil)
   (locals :accessor rule-locals
           :initarg :locals
           :type (or string nil)
           :initform nil)
   (catchers :accessor rule-catchers
             :initarg :catchers
             :type list
             :initform nil)))

(defclass catcher (grammar-definition)
  ((args :accessor catcher-args
         :initarg :args
         :initform nil
         :type (or string nil))
   (island :initarg catcher-island
           :initarg :island
           :initform nil
           :type string)))

(defclass final-catcher (catcher)
  ())

(defclass alias (rule)
  ())

(defclass token (rule)
  ())

(defclass mode (grammar-definition)
  ((mode :accessor mode
         :initarg :mode
         :type string)))

(defclass command (grammar-definition)
  ((name :accessor command-name
         :initarg :name
         :type string)
   (arg :accessor command-arg
        :initarg :arg
        :type (or string nil)
        :initform nil)))

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
  ((alternatives :accessor entity-alternatives
                 :initarg :alternatives
                 :type list)))

(defclass grammar-symbol (grammar-definition)
  ((value :accessor grammar-symbol-value
          :initarg :value
          :type string)))

(defclass terminal (grammar-symbol)
  ())

(defclass non-terminal (grammar-symbol)
  ((operator :accessor symbol-asgn
             :initarg :operator
             :type (or string nil)
             :initform nil)
   (value :accessor symbol-asgn-value
          :initarg :operator-value
          :type (or string nil)
          :initform nil)
   (args :accessor non-term-args
         :initarg :args
         :type (or string nil)
         :initform nil)))

(defclass rule-name (non-terminal)
  ())

(defclass token-name (non-terminal)
  ())

;;; ----------------------------------------------------------------------------
;;; printers
;;; ----------------------------------------------------------------------------
(defgeneric get-value (entity)
  (:documentation "Returns value of the given grammar entity. Used for printing."))

(defmethod get-value ((ent set-entity))
  (entity-set ent))

(defmethod get-value ((ent complex-entity))
  (entity-alternatives ent))

(defmethod get-value ((ent simple-entity))
  (entity-value ent))

(defmethod get-value ((ent range-entity))
  (format nil "'~A' to '~A'" (range-from ent) (range-to ent)))

(defmethod print-object ((term terminal) stream)
  (format stream "'~A'" (grammar-symbol-value term)))

(defmethod print-object ((term non-terminal) stream)
  (format stream "~A" (grammar-symbol-value term)))

(defmethod print-object ((term entity-with-mod-base) stream)
  (format stream "#<~A~@[~A~]: ~A>" (type-of term) (entity-mod term) (get-value term)))

(defmethod print-object ((term negatable-entity-base) stream)
  (format stream "#<~A~@[~A~]: ~@[~A~] ~A>" (type-of term) (entity-mod term) (entity-negated? term) (get-value term)))

(defmethod print-object ((term wildcard-entity) stream)
  (format stream "#<~A~@[~A~]>" (type-of term) (entity-mod term)))

(defmethod print-object ((term island-entity-base) stream)
  (format stream "#<~A: ~A>" (type-of term) (entity-island term)))

(defmethod print-object ((term alternative) stream)
  (format stream "#<~A: ~A>" (type-of term) (alternative-entities term)))

(defmethod print-object ((term rule) stream)
  (format stream "#<~A ~A: ~A>" (type-of term) (rule-name term) (rule-alternatives term)))

(defmethod print-object ((term grammar) stream)
  (format stream "#<~A ~A: ~A>" (type-of term) (grammar-name term) (grammar-rules term)))

;;; ----------------------------------------------------------------------------
;;; Maps
;;; ----------------------------------------------------------------------------
(defgeneric map-members (node function)
  (:documentation "Maps over the children of the given grammar construction."))

(defmethod map-members ((node grammar) function)
  (mapc function (grammar-rules node)))

(defmethod map-members ((node rule) function)
  (mapc function (rule-alternatives node))
  (awhen (rule-command node)
    (funcall function it)))

(defmethod map-members ((node alternative) function)
  (mapc function (alternative-entities node)))

(defmethod map-members ((node complex-entity) function)
  (mapc function (entity-alternatives node)))

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

(defmethod traverse ((node mode) function)
  (funcall function node))

(defmethod traverse ((node command) function)
  (funcall function node))


(defgeneric grammar-symbol-equal (s1 s2))

(defmethod grammar-symbol-equal ((s1 grammar-definition) (s2 grammar-definition))
  nil)

(defmethod grammar-symbol-equal ((s1 grammar-symbol) (s2 grammar-symbol))
  (and (eq (type-of s1) (type-of s2))
       (equal (grammar-symbol-value s1)
              (grammar-symbol-value s2))))

(defmethod grammar-symbol-equal ((s1 set-entity) (s2 set-entity))
  (equal (entity-set s1)
         (entity-set s2)))

(defmethod grammar-symbol-equal ((s1 range-entity) (s2 range-entity))
  (and (eq (range-from s1)
           (range-from s2))
       (eq (range-to s1)
           (range-to s2))))

(defmethod grammar-symbol-equal ((s1 Wildcard-entity) (s2 Wildcard-entity))
  t)
