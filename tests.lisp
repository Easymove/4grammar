(in-package :4grammar)


(defparameter *grammar* "
 grammar Abnf;

// Note: Whitespace handling not as strict as in the specification.

rulelist
   : rule_* EOF
   ;

rule_
   : ID ( '=' | '=/' ) elements
   ;

elements
   : alternation
   ;

alternation
   : concatenation ( '/' concatenation )*
   ;

concatenation
   : repetition ( repetition )*
   ;

repetition
   : repeat? element
   ;

repeat
   : INT | ( INT? '*' INT? )
   ;

element
   : ID | group | option | STRING | NumberValue | ProseValue
   ;

group
   : '(' alternation ')'
   ;

option
   : '[' alternation ']'
   ;


NumberValue
   : '%' ( BinaryValue | DecimalValue | HexValue )
   ;


fragment BinaryValue
   : 'b' BIT+ ( ( '.' BIT+ )+ | ( '-' BIT+ ) )?
   ;


fragment DecimalValue
   : 'd' DIGIT+ ( ( '.' DIGIT+ )+ | ( '-' DIGIT+ ) )?
   ;


fragment HexValue
   : 'x' HEX_DIGIT+ ( ( '.' HEX_DIGIT+ )+ | ( '-' HEX_DIGIT+ ) )?
   ;


ProseValue
   : '<' ( ~ '>' )* '>'
   ;


ID
   : ( 'a' .. 'z' | 'A' .. 'Z' ) ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' )*
   ;


INT
   : '0' .. '9'+
   ;

STRING
   : ( '%s' | '%i' )? '\"' ( ~ '\"' )* '\"'
   ;


fragment BIT
   : '0' .. '1'
   ;


fragment DIGIT
   : '0' .. '9'
   ;


// Note: from the RFC errata (http://www.rfc-editor.org/errata_search.php?rfc=5234&eid=4040):
// > ABNF strings are case insensitive and the character set for these strings is US-ASCII.
// > So the definition of HEXDIG already allows for both upper and lower case (or a mixture).
fragment HEX_DIGIT
   : ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
;
")

(defparameter *tests* nil)

(defmacro define-parse-test (name (str parser) (parsed) &body body)
  `(progn (defun ,name ()
            (let ((,parsed (caar (parse (.first ,parser) ,str))))
              (progn (assert ,@body) t)))
          (push ',name *tests*)))

(defun run-tests ()
  (loop
     :for test :in *tests*
     :do (format t "~A"
                 (if (handler-case (funcall test)
                       (error (e) (declare (ignorable e)) nil))
                     "."
                     "x"))))

(defgeneric object-equal (obj1 obj2))

(defmethod object-equal ((obj1 t) (obj2 t))
  (or (eq obj1 obj2)
      (and (equal (class-of obj1) (class-of obj2))
           (loop
              :for slot :in (sb-mop:class-slots (class-of obj1))
              :always (object-equal (slot-value obj1 (sb-mop:slot-definition-name slot))
                                    (slot-value obj2 (sb-mop:slot-definition-name slot)))))))

(defmethod object-equal ((obj1 number) (obj2 number))
  (= obj1 obj2))

(defmethod object-equal ((obj1 string) (obj2 string))
  (string= obj1 obj2))

(defmethod object-equal ((obj1 list) (obj2 list))
  (and (= (length obj1)
          (length obj2))
       (loop
          :for x :in obj1
          :for y :in obj2
          :always (object-equal x y))))

(defmethod object-equal ((obj1 vector) (obj2 vector))
  (and (= (length obj1)
          (length obj2))
       (loop
          :for x :across obj1
          :for y :across obj2
          :always (object-equal x y))))


(define-parse-test test.parse.simple-entity.1
    ("'hey'+" (.simple-entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod #\+
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.line-comment.1
    ("// asd asd asd asd
'hey'" (.simple-entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod nil
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.block-comment.1
    ("/*asd */ 'hey'" (.simple-entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod nil
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.complex-entity.1
    ("( 'hey'? yo* )?" (.complex-entity))
    (res)
  (object-equal (make-instance 'complex-entity
                               :negated? nil
                               :mod #\?
                               :value (list
                                       (make-instance
                                        'alternative
                                        :entities
                                        (list (make-instance
                                               'simple-entity
                                               :negated? nil
                                               :mod #\?
                                               :value (make-instance 'terminal
                                                                     :value "hey"))
                                              (make-instance
                                               'simple-entity
                                               :negated? nil
                                               :mod #\*
                                               :value (make-instance 'non-terminal
                                                                     :value "yo"))))))
                res))

(define-parse-test test.parse.complex-entity.2
    ("( 'hey' | yo )+" (.complex-entity))
    (res)
  (object-equal (make-instance 'complex-entity
                               :negated? nil
                               :mod #\+
                               :value (list
                                       (make-instance
                                        'alternative
                                        :entities
                                        (list (make-instance
                                               'simple-entity
                                               :negated? nil
                                               :mod nil
                                               :value (make-instance 'terminal
                                                                     :value "hey"))))
                                       (make-instance
                                        'alternative
                                        :entities
                                        (list (make-instance
                                               'simple-entity
                                               :negated? nil
                                               :mod nil
                                               :value (make-instance 'non-terminal
                                                                     :value "yo"))))))
                res))

(define-parse-test test.parse.set-entity.1
    ("[abc]" (.set-entity))
    (res)
  (object-equal (make-instance 'set-entity
                               :negated? nil
                               :mod nil
                               :set (list #\a #\b #\c))
                res))


(define-parse-test test.parse.range-entity.1
    ("A .. B" (.range-entity))
    (res)
  (object-equal (make-instance 'range-entity
                               :negated? nil
                               :mod nil
                               :from #\A
                               :to #\B)
                res))
