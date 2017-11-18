;;;; 4grammar.asd

(asdf:defsystem #:4grammar
  :description "Describe 4grammar here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:smug
               #:anaphora
               #:alexandria
               #:drakma
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "server")
               (:file "domain" :depends-on ("package"))
               (:file "parser" :depends-on ("domain"))
               (:file "queries" :depends-on ("parser"))
               (:file "4grammar" :depends-on ("queries"))
               (:file "tests" :depends-on ("4grammar"))))

