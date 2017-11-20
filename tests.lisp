(in-package :4grammar)

(defparameter *tests* nil)

(defmacro define-parse-test (name (str parser) (parsed) &body body)
  `(progn (defun ,name ()
            (let ((,parsed (caar (parse (.first ,parser) ,str))))
              (progn (assert (progn ,@body)) t)))
          (push ',name *tests*)))

(defun run-tests ()
  (let ((failed-tests))
    (loop
       :for test :in *tests*
       :do (format t "~A"
                   (if (handler-case (funcall test)
                         (error (e) (declare (ignorable e)) nil))
                       "."
                       (progn
                         (push test failed-tests)
                         "x"))))
    (format t "~%~@[Failed tests:~%~{~S~%~}~]" failed-tests)))

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
    ("'hey'+" (.entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod #\+
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.simple-entity.2
    ("~ 'a'" (.entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? t
                               :mod nil
                               :value (make-instance 'terminal
                                                     :value "a"))
                res))

(define-parse-test test.parse.line-comment.1
    ("// asd asd asd asd
'hey'" (.entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod nil
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.block-comment.1
    ("/*asd */ 'hey'" (.entity))
    (res)
  (object-equal (make-instance 'simple-entity
                               :negated? nil
                               :mod nil
                               :value (make-instance 'terminal
                                                     :value "hey"))
                res))

(define-parse-test test.parse.complex-entity.1
    ("( 'hey'? yo* )?" (.entity))
    (res)
  (object-equal (make-instance 'complex-entity
                               :negated? nil
                               :mod #\?
                               :alternatives (list
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
                                                      :value (make-instance 'rule-name
                                                                            :value "yo"))))))
                res))

(define-parse-test test.parse.complex-entity.2
    ("( 'hey' | yo )+" (.entity))
    (res)
  (object-equal (make-instance 'complex-entity
                               :negated? nil
                               :mod #\+
                               :alternatives (list
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
                                                      :value (make-instance 'rule-name
                                                                            :value "yo"))))))
                res))

(define-parse-test test.parse.complex-entity.3
    ("~( 'A' | 'B' )*" (.entity))
    (res)
  (object-equal (make-instance 'complex-entity
                               :negated? t
                               :mod #\*
                               :alternatives (list
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'terminal
                                                                            :value "A"))))
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'terminal
                                                                            :value "B"))))))
                res))

(define-parse-test test.parse.set-entity.1
    ("[abc]" (.entity))
    (res)
  (object-equal (make-instance 'set-entity
                               :negated? nil
                               :mod nil
                               :set (list "a" "b" "c"))
                res))


(define-parse-test test.parse.set-entity.2
    ("[a-c0-2]" (.entity))
    (res)
  (object-equal (make-instance 'set-entity
                               :negated? nil
                               :mod nil
                               :set (list "a" "b" "c" "0" "1" "2"))
                res))

(define-parse-test test.parse.set-entity.3
    ("[\\n\\r]" (.entity))
    (res)
  (object-equal (make-instance 'set-entity
                               :negated? nil
                               :mod nil
                               :set (list "\\n" "\\r"))
                res))

(define-parse-test test.parse.set-entity.4
    ("~[\\n\\r]+" (.entity))
    (res)
  (object-equal (make-instance 'set-entity
                               :negated? t
                               :mod #\+
                               :set (list "\\n" "\\r"))
                res))

(define-parse-test test.parse.range-entity.1
    ("'A' .. 'B'" (.entity))
    (res)
  (object-equal (make-instance 'range-entity
                               :negated? nil
                               :mod nil
                               :from "A"
                               :to "B"
                               :set (list "A" "B"))
                res))


(define-parse-test test.parse.wildcard-entity.1
    ("." (.entity))
    (res)
  (object-equal (make-instance 'Wildcard-entity
                               :mod nil)
                res))


(define-parse-test test.parse.action-entity.1
    ("{ SomeAction.here() }" (.entity))
    (res)
  (object-equal (make-instance 'action-entity
                               :island " SomeAction.here() ")
                res))

(define-parse-test test.parse.predicate-entity.1
    ("{ SomePredicate.here() }?" (.entity))
    (res)
  (object-equal (make-instance 'predicate-entity
                               :island " SomePredicate.here() ")
                res))

(define-parse-test test.parse.command.1
    ("a : b | c -> skip ;" (.statement))
    (res)
  (object-equal (make-instance 'rule
                               :command (make-instance 'command
                                                       :name "skip")
                               :alternatives (list
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'rule-name
                                                                            :value "b"))))
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'rule-name
                                                                            :value "c")))))
                               :name "a")
                res))

(define-parse-test test.parse.command.2
    ("A : B | C -> channel ( HIDDEN ) ;" (.statement))
    (res)
  (object-equal (make-instance 'token
                               :command (make-instance 'command
                                                       :name "channel"
                                                       :arg "HIDDEN")
                               :alternatives (list
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'token-name
                                                                            :value "B"))))
                                              (make-instance
                                               'alternative
                                               :entities
                                               (list (make-instance
                                                      'simple-entity
                                                      :negated? nil
                                                      :mod nil
                                                      :value (make-instance 'token-name
                                                                            :value "C")))))
                               :name "A")
                res))

;;; ----------------------------------------------------------------------------
;;; Queries tests
;;; ----------------------------------------------------------------------------

(defparameter *abnf-grammar* "
/*
BSD License
Copyright \(c\) 2013, Rainer Schuster
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Rainer Schuster nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES \(INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION\) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\(INCLUDING NEGLIGENCE OR OTHERWISE\) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
ABNF grammar derived from:
http://tools.ietf.org/html/rfc5234
Augmented BNF for Syntax Specifications: ABNF
January 2008
http://tools.ietf.org/html/rfc7405
Case-Sensitive String Support in ABNF
December 2014
Terminal rules mainly created by ANTLRWorks 1.5 sample code.
*/
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


COMMENT
   : ';' ~ ( '\\n' | '\\r' )* '\\r'? '\\n' -> channel ( HIDDEN )
   ;


WS
   : ( ' ' | '\\t' | '\\r' | '\\n' ) -> channel ( HIDDEN )
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

//unused and missing definitions for testing purposes

fragment UNUSED : '0' .. '7' ;

UNUSED2 : ~( TOKEN1 | TOKEN2 ) ;

unused-rule : ( missing-rule '==' ( DIGIT | missing-rule2 )+ )* ;

// Note: from the RFC errata (http://www.rfc-editor.org/errata_search.php?rfc=5234&eid=4040):
// > ABNF strings are case insensitive and the character set for these strings is US-ASCII.
// > So the definition of HEXDIG already allows for both upper and lower case (or a mixture).
fragment HEX_DIGIT
   : ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
   ;"

  "From: https://github.com/antlr/grammars-v4/blob/master/abnf/Abnf.g4")

(defmacro define-query-test (name (req) (response) &body body)
  `(progn
     (defun ,name ()
       (let ((,response (compute-or-get-query ,req)))
         (progn (assert (progn ,@body)) t)))
     (push ',name *tests*)))


(define-query-test unused-definitions.1
    ((make-instance 'unused-definitions
                    :grammar *abnf-grammar*))
    (response)
  (object-equal (make-instance 'unused-definitions-response
                               :unused-definitions
                               (list "UNUSED" "UNUSED2" "unused-rule"))
                response))

(define-query-test missing-definitions.1
    ((make-instance 'missing-definitions
                    :grammar *abnf-grammar*))
    (response)
  (object-equal (make-instance 'missing-definitions-response
                               :missing-definitions
                               (list "TOKEN1" "TOKEN2" "missing-rule" "missing-rule2"))
                response))
