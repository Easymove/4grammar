;;;; 4grammar.asd

(asdf:defsystem #:4grammar
  :description "Describe 4grammar here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:smug
               #:anaphora
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "domain" :depends-on ("package"))
               (:file "parser" :depends-on ("domain"))
               (:file "4grammar" :depends-on ("parser"))))

