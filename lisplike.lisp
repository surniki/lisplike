;;;; lisplike.lisp

(in-package #:lisplike)

(defstruct lisplike-func
  (expression)
  (environment)
  (arguments)
  (id))

(defstruct cl-func
  (arguments)
  (name)
  (function))

(let ((counter 0))
  (defun create-lisplike-func (&key expr env args)
    (make-lisplike-func :expression expr :environment env :arguments args :id (incf counter))))

(defparameter *the-false-value* (gensym))
(defparameter *the-true-value* (gensym))
(defparameter *type-set-symbol* (gensym))

(environment:define-global
    (true *the-true-value*)
    (false *the-false-value*)
    (test 42)
    (first (make-cl-func :arguments '(list) :name 'car :function #'car))
    (rest (make-cl-func :arguments '(list) :name 'cdr :function #'cdr))
    (+ (make-cl-func :arguments '(x y) :name '+ :function #'+))
    (- (make-cl-func :arguments '(x y) :name '- :function #'-))
    (* (make-cl-func :arguments '(x y) :name '* :function #'*))
    (/ (make-cl-func :arguments '(x y) :name '/ :function #'/))
    (= (make-cl-func :arguments '(x y) :name '= :function
		     (lambda (x y) (if (= x y) *the-true-value* *the-false-value*))))
    (display (make-cl-func :arguments '(x) :name 'print :function
			   (lambda (x) (format t "~a~%" x) 'done))))

(defun literalp (datum)
  (or
   (numberp datum)
   (characterp datum)
   (stringp datum)
   (vectorp datum)))

(defun value->string (value)
  "Generates a string representation for the Lisplike value given."
  (cond ((eq value *the-false-value*) "false")
	((eq value *the-true-value*) "true")
	((lisplike-func-p value) (format nil "<<lisplike function #~a>>" (lisplike-func-id value)))
	((cl-func-p value) (format nil "<<common lisp function [~a]>>" (cl-func-name value)))
	(t (format nil "~a" value))))

(defun read-string (string)
  "Reads a string and converts it into a list of symbols."
  (read-as-string string))

(defun evaluate-n (exprs env)
  (labels ((evaluate-n-aux (exprs env value)
	     (if (null exprs)
		 (values value env)
		 (multiple-value-bind (new-value new-env) (evaluate (car exprs) env)
		   (evaluate-n-aux (cdr exprs) new-env new-value)))))
    (evaluate-n-aux exprs env nil)))
      
(defun environment-with-args (names values env)
  (if (null names)
      env
      (environment-with-args (cdr names) (cdr values) (environment:store (car names) (car values) env))))

(defun evaluate (expr env)
  "Evaluates a Lisplike expression EXPR in the context of the current environment ENV.
Returns two values: the result of the expression and the resulting environment."
  (if (atom expr)
      (cond ((literalp expr) (values expr env))
	    ((symbolp expr) (values (environment:lookup expr env) env))
	    (t (error "Attempted to evaluate an atom that is not a literal or a symbol.")))
      (case (first expr)
	(quote (values (second expr) env))
	(if (if (eq *the-true-value* (evaluate (second expr) env))
		(evaluate (third expr) env)
		(evaluate (fourth expr) env)))
	(define (values (second expr) (environment:store (second expr) (evaluate (third expr) env) env)))
	(set! (values (second expr) (environment:reset (second expr) (evaluate (third expr) env) env)))
	(do (evaluate-n (cdr expr) env))
	(lambda (values (create-lisplike-func :args (second expr) :expr (third expr) :env env) env))
	(otherwise (let ((applied-function (evaluate (car expr) env))
			 (arguments (mapcar (lambda (expr) (evaluate expr env)) (cdr expr)))
			 (given-arg-count (length (cdr expr))))
		     (cond ((lisplike-func-p applied-function)
			    (let ((expected-arg-count (length (lisplike-func-arguments applied-function))))
			      (when (not (= given-arg-count expected-arg-count))
				(error
				 (format nil "Arity mismatch: Expected ~a arguments but was given ~a arguments."
					 expected-arg-count given-arg-count)))
			      (values (evaluate (lisplike-func-expression applied-function)
						(environment-with-args (lisplike-func-arguments applied-function)
								       arguments
								       (lisplike-func-environment applied-function)))
				      env)))
			   ((cl-func-p applied-function)
			    (let ((expected-arg-count (length (cl-func-arguments applied-function))))
			      (when (not (= given-arg-count expected-arg-count))
				(error
				 (format nil "Arity mismatch: Expected ~a arguments but was given ~a arguments."
					 expected-arg-count given-arg-count)))
			      (values (apply (cl-func-function applied-function) arguments) env)))
			   (t (error (format nil "Attempted to use [~a] as a function." (car expr))))))))))

(defun repl ()
  (labels ((lisplike-repl-aux (prompt goodbye env)
	     (format t "~a" prompt)
	     (let ((expr (read)))
	       (if (or (equal expr '(quit)) (equal expr '(exit)))
		   (format t "~a~%" goodbye)
		   (multiple-value-bind (result new-env) (evaluate expr env)
		     (format t "~a~%" (value->string result))
		     (lisplike-repl-aux prompt goodbye new-env))))))
    (lisplike-repl-aux "lisplike> "
		     "Go in peace! I will not say: do not weep; for not all tears are an evil."
		     environment:global)))

