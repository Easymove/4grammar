;;;; 4grammar.asd

(asdf:defsystem #:4grammar
  :description "Yet another little and smooth common lisp server for a sort of
                parsing/analyzing grammars. Originally intended to work with
                ANTLR4 grammars but can be easily extended to work with any other
                grammar described in the BNF (see the top of the parse.lisp)."
  :author "Alex Easymove <alex.yegorka@gmail.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:smug
               #:anaphora
               #:alexandria
               #:drakma
               #:yason
               #:cl-graph)
  :serial t
  :components ((:file "package")
               (:file "storage")
               (:file "server" :depends-on ("storage"))
               (:file "domain" :depends-on ("package"))
               (:file "parser" :depends-on ("domain"))
               (:file "queries" :depends-on ("parser" "storage"))
               (:file "4grammar" :depends-on ("queries"))
               (:file "tests" :depends-on ("4grammar"))))

