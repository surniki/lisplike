;;;; package.lisp

(defpackage #:lisplike
  (:use #:cl)
  (:documentation "Defines the interpreter")
  (:export :repl))

(defpackage #:environment
  (:use #:cl)
  (:documentation "Contains the different default environments for the interpreter.")
  (:export :create
	   :store
	   :lookup
	   :reset
	   :initial
	   :global
	   :define-global))
