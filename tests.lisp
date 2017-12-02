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

(defvar *ignore-fields* nil)

(defmethod object-equal ((obj1 t) (obj2 t))
  (or (eq obj1 obj2)
      (and (equal (class-of obj1) (class-of obj2))
           (loop
              :for slot :in (sb-mop:class-slots (class-of obj1))
              :always (or (member (sb-mop:slot-definition-name slot) *ignore-fields*)
                          (object-equal (slot-value obj1 (sb-mop:slot-definition-name slot))
                                        (slot-value obj2 (sb-mop:slot-definition-name slot))))))))

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


(define-query-test draw-grammar.1
    ((make-instance 'draw-grammar
                    :grammar *abnf-grammar*))
    (response)
  (object-equal (make-instance 'draw-grammar-response
                               :dot "digraph G {
graph [];

0 [label=\"rulelist\", color=blue];
1 [label=\"rule_\", color=black];
2 [label=\"ID\", color=black];
3 [label=\"-\", color=red];
4 [label=\"=\", color=red];
5 [label=\"=/\", color=red];
6 [label=\"elements\", color=black];
7 [label=\"alternation\", color=black];
8 [label=\"concatenation\", color=black];
9 [label=\"repetition\", color=black];
10 [label=\"repeat\", color=black];
11 [label=\"INT\", color=black];
12 [label=\"*\", color=red];
13 [label=\"element\", color=black];
14 [label=\"group\", color=black];
15 [label=\"(\", color=red];
16 [label=\")\", color=red];
17 [label=\"option\", color=black];
18 [label=\"[\", color=red];
19 [label=\"]\", color=red];
20 [label=\"STRING\", color=black];
21 [label=\"%s\", color=red];
22 [label=\"%i\", color=red];
23 [label=\"\"\", color=red];
24 [label=\"NumberValue\", color=black];
25 [label=\"%\", color=red];
26 [label=\"BinaryValue\", color=black];
27 [label=\"b\", color=red];
28 [label=\"BIT\", color=black];
29 [label=\".\", color=red];
30 [label=\"DecimalValue\", color=black];
31 [label=\"d\", color=red];
32 [label=\"DIGIT\", color=black];
33 [label=\"HexValue\", color=black];
34 [label=\"x\", color=red];
35 [label=\"HEX_DIGIT\", color=black];
36 [label=\"ProseValue\", color=black];
37 [label=\"<\", color=red];
38 [label=\">\", color=red];
39 [label=\"/\", color=red];
40 [label=\"EOF\", color=black];
41 [label=\"COMMENT\", color=blue];
42 [label=\";\", color=red];
43 [label=\"n\", color=red];
44 [label=\"r\", color=red];
45 [label=\"WS\", color=blue];
46 [label=\" \", color=red];
47 [label=\"t\", color=red];
48 [label=\"UNUSED\", color=blue];
49 [label=\"UNUSED2\", color=blue];
50 [label=\"TOKEN1\", color=red];
51 [label=\"TOKEN2\", color=red];
52 [label=\"unused-rule\", color=blue];
53 [label=\"missing-rule\", color=red];
54 [label=\"==\", color=red];
55 [label=\"missing-rule2\", color=red];
0->1 [];
1->2 [];
13->2 [];
2->3 [];
26->3 [];
30->3 [];
33->3 [];
1->4 [];
1->5 [];
1->6 [];
14->7 [];
17->7 [];
6->7 [];
7->8 [];
8->9 [];
9->10 [];
10->11 [];
10->12 [];
9->13 [];
13->14 [];
14->15 [];
14->16 [];
13->17 [];
17->18 [];
17->19 [];
13->20 [];
20->21 [];
20->22 [];
20->23 [];
13->24 [];
24->25 [];
24->26 [];
26->27 [];
26->28 [];
26->29 [];
30->29 [];
33->29 [];
24->30 [];
30->31 [];
30->32 [];
52->32 [];
24->33 [];
33->34 [];
33->35 [];
13->36 [];
36->37 [];
36->38 [];
7->39 [];
0->40 [];
41->42 [];
41->43 [];
45->43 [];
41->44 [];
45->44 [];
45->46 [];
45->47 [];
49->50 [];
49->51 [];
52->53 [];
52->54 [];
52->55 [];
}")
                response))


(define-query-test first-set.1
    ((make-instance 'rule-first-set
                    :grammar *abnf-grammar*
                    :rule-name "rule_"))
    (response)
  (object-equal (make-instance 'rule-first-set-response
                               :rule-first-set (list (make-instance 'token-name
                                                                    :value "ID")))
                response))

(define-query-test first-set.2
    ((make-instance 'rule-first-set
                    :grammar *abnf-grammar*
                    :rule-name "ID"))
    (response)
  (object-equal (make-instance
                 'rule-first-set-response
                 :rule-first-set
                 (list (make-instance
                        'range-entity
                        :from "a"
                        :to "z"
                        :set '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
                        :mod nil)))
                response))

(define-query-test follow-set.1
    ((make-instance 'rule-follow-set
                    :grammar *abnf-grammar*
                    :rule-name "rule_"))
    (response)
  (object-equal (make-instance 'rule-follow-set-response
                               :rule-follow-set (list (make-instance 'token-name
                                                                     :value "ID")
                                                      (make-instance 'token-name
                                                                     :value "EOF")))
                response))

(define-query-test follow-set.2
    ((make-instance 'rule-follow-set
                    :grammar *abnf-grammar*
                    :rule-name "ID"))
    (response)
  (object-equal (make-instance 'rule-follow-set-response
                               :rule-follow-set (list (make-instance 'token-name
                                                                     :value "INT")
                                                      (make-instance 'terminal
                                                                     :value "/")
                                                      (make-instance 'terminal
                                                                     :value "]")
                                                      (make-instance 'terminal
                                                                     :value ")")
                                                      (make-instance 'token-name
                                                                     :value "ID")
                                                      (make-instance 'token-name
                                                                     :value "EOF")
                                                      (make-instance 'terminal
                                                                     :value "=")))
                response))


(define-query-test double-l-one-check.1
    ((make-instance 'double-l-one-check
                    :grammar *abnf-grammar*))
    (response)
  (let ((*ignore-fields* '(rule alt1 alt2)))
    (object-equal (make-instance 'double-l-one-check-response
                                 :double-l-one-check nil
                                 :conflicts (list
                                             (make-instance 'conflict
                                                            :rule nil
                                                            :alt1 nil
                                                            :alt2 nil
                                                            :set (list (make-instance 'token-name
                                                                                      :value "INT")))
                                             (make-instance 'conflict
                                                            :rule nil
                                                            :alt1 nil
                                                            :alt2 nil
                                                            :set (list (make-instance 'token-name
                                                                                      :value "INT")))
                                             (make-instance 'conflict
                                                            :rule nil
                                                            :alt1 nil
                                                            :alt2 nil
                                                            :set (list (make-instance 'token-name
                                                                                      :value "INT")))))
                  response)))
