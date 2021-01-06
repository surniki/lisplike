;;;; lisplike.asd

(asdf:defsystem #:lisplike
  :description "A programming language for physics and more."
  :author "Scott Urnikis <surniki@ilstu.edu>"
  :license  "3-clause BSD license"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "environment")
               (:file "mellon")))
