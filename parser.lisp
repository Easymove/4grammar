(in-package #:4grammar)

(defparameter +whitespace+ (list #\space #\newline #\tab))
(defparameter +literal-quote+ #\')
(defparameter +literal-escape+ #\\)
(defparameter +rule-delimiter+ #\:)
(defparameter +statement-end+ #\;)
(defparameter +alternative+ #\|)
(defparameter +complex-entity-open+ #\()
(defparameter +complex-entity-close+ #\))
(defparameter +set-entity-open+ #\[)
(defparameter +set-entity-close+ #\])
(defparameter +negation+ #\~)
(defparameter +action-open+ #\{)
(defparameter +action-close+ #\})
(defparameter +predicate-action+ #\?)
(defparameter +range-delimiter+ "..")
(defparameter +redirection-sign+ "->")
(defparameter +channel+ "channel")

(defparameter +line-comment-start+ "//")
(defparameter +block-comment-open+ "/*")
(defparameter +block-comment-close+ "*/")

(defparameter *default-res-fn* #'list)

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
  (.first
   (.plus (.let* ((x parser)
                  (xs (.zero-or-more parser)))
            (.identity (apply res-fn x xs)))
          (.identity nil))))

(defun .whitespace (&optional result-type)
  (.or (.first (.map result-type (.is (rcurry #'member +whitespace+))))
       (.comment)))

(defun .identifier ()
  (.and (.not (.string= +redirection-sign+))
        (.map 'string (.is #'word-constituent-p))))

(defun .literal ()
  (.let* ((_ (.char= +literal-quote+))
          (string
           (.map 'string
                 (.plus (.let* ((_ (.char= +literal-escape+)))
                          (.item))
                        (.is-not 'char= +literal-quote+))))
          (_ (.char= +literal-quote+)))
    (.identity string)))

(defun .with-ws (parser)
  (.let* ((_ (.zero-or-more (.whitespace)))
          (res parser))
    (.identity res)))

(defun token-name-p (string)
  (every #'upper-case-p string))

(defun .trash ()
  (.let* ((string (.with-ws
                   (.map 'string
                         (.and (.not (.char= +statement-end+))
                               (.item)))))
          (_ (.char= +statement-end+)))
    (warn "Can't parse:~%~A~%" string)
    (.identity nil)))

;;; -----------------------------------------------------------------------------
;;; general parsers
;;; -----------------------------------------------------------------------------
(defun parse-grammar (string)
  (parse (.grammar) string))

(defun parse (parser string)
  (funcall parser string))



(defun .grammar ()
  (.let* ((name (.grammar-statement))
          (rules (.zero-or-more (.statement)))
          (_ (.optional (.whitespace))))
    (.identity (make-instance 'grammar
                              :name name
                              :rules rules))))


(defun .grammar-statement ()
  (.let* ((_ (.with-ws (.string= "grammar")))
          (name (.with-ws (.identifier)))
          (_ (.with-ws (.char= +statement-end+))))
    (.identity name)))


(defun .statement ()
  (.let* ((stmt (.with-ws (.or (.alias)
                               (.rule-or-token))))
          (_ (.with-ws (.char= +statement-end+))))
    (.identity stmt)))


(defun .alias ()
  (.let* ((_ (.with-ws (.string= "fragment"))))
    (.rule-aux (lambda (name channel alternatives)
                 (make-instance 'alias
                                :name name
                                :channel channel
                                :alternatives alternatives)))))


(defun .rule-or-token ()
  (.rule-aux))


(defun .rule-aux (&optional (make-fn (lambda (name channel alternatives)
                                       (make-instance (if (token-name-p name)
                                                          'token
                                                          'rule)
                                                      :name name
                                                      :channel channel
                                                      :alternatives alternatives))))
  (.let* ((name (.with-ws (.identifier)))
          (_ (.with-ws (.char= +rule-delimiter+)))
          (alternatives (.alternatives-list))
          (channel (.optional (.channel))))
    (.identity (funcall make-fn name channel alternatives))))


(defun .channel ()
  (.let* ((_ (.with-ws (.string= +redirection-sign+)))
          (channel (.channel-name)))
    (.identity channel)))


(defun .channel-name ()
  (.or (.let* ((_ (.with-ws (.string= +channel+)))
               (_ (.with-ws (.char= +complex-entity-open+)))
               (channel (.with-ws (.identifier)))
               (_ (.with-ws (.char= +complex-entity-close+))))
         (.identity channel))
       (.with-ws (.identifier))))


(defun .alternatives-list ()
  (.let* ((x (.alternative))
          (xs (.plus (.and (.with-ws (.char= +alternative+))
                           (.alternatives-list))
                     (.identity nil))))
    (.identity (apply #'list x xs))))


(defun .alternative ()
  (.let* ((entities (.zero-or-more (.entity))))
    (.identity (make-instance 'alternative
                              :entities entities))))


(defun .entity ()
  (.or (.wildcard-entity)
       (.predicate-entity)
       (.action-entity)
       (.set-entity)
       (.complex-entity)
       (.range-entity)
       (.simple-entity)))


(defun .wildcard-entity ()
  (.let* ((_ (.with-ws (.char= #\.)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'wildcard-entity
                              :mod mod))))

(defun .predicate-entity ()
  (.let* ((_ (.with-ws (.char= +action-open+)))
          (island (.map 'string (.and (.not (.char= +action-close+))
                                      (.item))))
          (_ (.char= +action-close+))
          (_ (.char= +predicate-action+)))
    (.identity (make-instance 'predicate-entity
                              :island island))))

(defun .action-entity ()
  (.let* ((_ (.with-ws (.char= +action-open+)))
          (island (.map 'string (.and (.not (.char= +action-close+))
                                      (.item))))
          (_ (.char= +action-close+)))
    (.identity (make-instance 'action-entity
                              :island island))))

(defun .simple-entity ()
  (.let* ((negated? (.optional (.negation)))
          (value (.or (.terminal)
                      (.non-terminal)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'simple-entity
                              :value value
                              :mod mod
                              :negated? (not (null negated?))))))


(defun .complex-entity ()
  (.let* ((negated? (.optional (.negation)))
          (_ (.with-ws (.char= +complex-entity-open+)))
          (alternatives (.alternatives-list))
          (_ (.with-ws (.char= +complex-entity-close+)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'complex-entity
                              :mod mod
                              :negated? (not (null negated?))
                              :alternatives alternatives))))


(defun .range-entity ()
  (.let* ((negated? (.optional (.negation)))
          (range-from (.with-ws (.literal)))
          (_ (.with-ws (.string= +range-delimiter+)))
          (range-to (.with-ws (.literal)))
          (mod (.optional (.mod))))
    (.identity (make-instance 'range-entity
                              :from range-from
                              :to range-to
                              :set (expand-set range-from range-to)
                              :mod mod
                              :negated? (not (null negated?))))))


(defun .set-entity ()
  (.let* ((negated? (.optional (.negation)))
          (_ (.with-ws (.char= +set-entity-open+)))
          (set (.set))
          (_ (.char= +set-entity-close+))
          (mod (.optional (.mod))))
    (.identity (make-instance 'set-entity
                              :set set
                              :mod mod
                              :negated? (not (null negated?))))))


(defun .set ()
  (.zero-or-more
   (.and
    (.not (.char= +set-entity-close+))
    (.or (.range)
         (.let* ((ch1 (.char)))
           (.identity (list ch1)))))
   #'append))


(defun .range ()
  (.let* ((ch1 (.char))
          (_ (.char= #\-))
          (ch2 (.char)))
    (.identity (expand-set ch1 ch2))))


(defun expand-set (ch1 ch2)
  ;; expand only ascii ranges
  (if (and (= (length ch1) 1)
           (= (length ch2) 1))
      (loop
         :for i :from (char-code (char ch1 0)) :to (char-code (char ch2 0))
         :collect (coerce (list (code-char i)) 'string))
      (list ch1 ch2)))


(defun .char ()
  (.or (.quoted-char)
       (.let* ((ch1 (.item)))
         (.identity (coerce (list ch1) 'string)))))


(defun .quoted-char ()
  (.let* ((slash (.char= #\\))
          (ch1 (.item)))
    (.identity (coerce (list slash ch1) 'string))))


(defun .mod ()
  (.with-ws
   (.or (.char= #\?)
        (.char= #\+)
        (.char= #\*))))


(defun .negation ()
  (.with-ws (.char= +negation+)))


(defun .terminal ()
  (.let* ((val (.with-ws (.literal))))
    (.identity (make-instance 'terminal
                              :value val))))

(defun .non-terminal ()
  (.let* ((val (.with-ws (.first (.identifier)))))
    (.identity (make-instance (if (token-name-p val)
                                  'token-name
                                  'rule-name)
                              :value val))))


(defun .comment (&optional result-type)
  (.or (.line-comment result-type)
       (.block-comment result-type)))


(defun .line-comment (&optional result-type)
  (.let* ((_ (.string= +line-comment-start+))
          (content (.map result-type (.is-not (curry #'char= #\NewLine))))
          (_ (.char= #\Newline)))
    (.identity content)))


(defun .block-comment (&optional result-type)
  (.let* ((_ (.string= +block-comment-open+))
          (content (.map result-type
                         (.and (.not (.string= +block-comment-close+))
                               (.item))))
          (_ (.string= +block-comment-close+)))
    (.identity content)))
