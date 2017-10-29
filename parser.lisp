(in-package #:4grammar)

(defparameter *default-res-fn* #'list)

(defparameter +whitespace+ (list #\space #\newline #\tab))
(defparameter +literal-quote+ #\')
(defparameter +literal-escape+ #\\)
(defparameter +rule-delimiter+ ":")
(defparameter +rule-end+ ";")
(defparameter +alternative+ #\|)
(defparameter +complex-entity-open+ #\()
(defparameter +complex-entity-close+ #\))


;;; -----------------------------------------------------------------------------
;;; grammar specific functions
;;; -----------------------------------------------------------------------------
(defun word-constituent-p (ch)
  (or (alphanumericp ch)
      (eq ch #\_)
      (eq ch #\-)))

;;; -----------------------------------------------------------------------------
;;; auxiliary parsers
;;; -----------------------------------------------------------------------------
(defun .zero-or-more (parser &optional (res-fn *default-res-fn*))
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (apply res-fn x xs)))
         (.identity nil)))

(defun .one-or-more (parser &optional (res-fn *default-res-fn*))
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (apply res-fn x xs)))
         (.identity nil)))

(defun .whitespace (&optional result-type)
  (.first (.map result-type (.is (rcurry #'member +whitespace+)))))

(defun .identifier ()
  (.map 'string (.is #'word-constituent-p)))

(defun .literal ()
  (.let* ((_ (.char= +literal-quote+))
          (string
           (.map 'string
                 (.plus (.let* ((_ (.char= +literal-escape+)))
                          (.item))
                        (.is-not 'char= +literal-quote+))))
          (_ (.char= +literal-quote+)))
    (.identity string)))

;;; -----------------------------------------------------------------------------
;;; general parsers
;;; -----------------------------------------------------------------------------
(defun parse-grammar (string)
  (parse (.grammar) string))

(defun parse (parser string)
  (funcall parser string))

(defun .grammar ()
  (.first (.zero-or-more (.rule) (lambda (&rest rules) (make-instance 'grammar :rules rules)))))

(defun .rule ()
  (.let* ((_ (.optional (.whitespace)))
          (name (.non-terminal))
          (_ (.optional (.whitespace)))
          (_ (.string= +rule-delimiter+))
          (_ (.optional (.whitespace)))
          (commands (.commands-list))
          (_ (.optional (.whitespace)))
          (_ (.string= +rule-end+)))
    (.identity (make-instance 'rule
                              :name name
                              :commands commands))))

(defun .commands-list ()
  (.first
   (.let* ((x (.command))
           (xs (.plus (.and (.char= +alternative+)
                            (.commands-list))
                      (.identity nil))))
     (.identity (apply #'list x xs)))))

(defun .command ()
  (.first (.zero-or-more (.entity) (lambda (&rest entities) (make-instance 'command :entities entities)))))

(defun .entity ()
  (.or (.simple-entity)
       (.complex-entity)))

(defun .simple-entity ()
  (.let* ((_ (.optional (.whitespace)))
          (value (.or (.terminal)
                      (.non-terminal)))
          (_ (.optional (.whitespace)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'simple-entity
                              :value value
                              :mod mod))))

(defun .complex-entity ()
  (.let* ((_ (.optional (.whitespace)))
          (_ (.char= +complex-entity-open+))
          (entities (.zero-or-more (.entity)))
          (_ (.optional (.whitespace)))
          (_ (.char= +complex-entity-close+))
          (_ (.optional (.whitespace)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'complex-entity
                              :value entities
                              :mod mod))))

(defun .mod ()
  (.or (.char= #\?)
       (.char= #\+)
       (.char= #\*)))

(defun .terminal ()
  (.let* ((val (.literal)))
    (.identity (make-instance 'terminal
                              :value val))))

(defun .non-terminal ()
  (.let* ((val (.first (.identifier))))
    (.identity (make-instance 'non-terminal
                              :value val))))
