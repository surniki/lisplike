
(in-package #:environment)

(defparameter initial nil)

(defun create (&optional base-env)
  "Constructs a new environment starting from the definitions in ENVIRONMENT:INITIAL.
Optionally, if given an environment BASE-ENV as an argument, it will construct a new environment
starting from the definitions in BASE-ENV."
  (if base-env
      (copy-list base-env)
      (copy-list initial)))

(defun store (symbol value env)
  (cons (cons symbol value) env))

(defun lookup (symbol env)
  (cdr (assoc symbol env)))

(defun reset (symbol new-value env)
  (labels ((reset-aux (symbol new-value env new-env)
	     (if (null env)
		 new-env
		 (let ((current-symbol (caar env))
		       (current-value (cdar env)))
		   (if (eq current-symbol symbol)
		       (reset-aux symbol new-value (cdr env) (cons (cons current-symbol new-value) new-env))
		       (reset-aux symbol new-value (cdr env) (cons (cons current-symbol current-value) new-env)))))))
    (reset-aux symbol new-value env nil)))

(defmacro define-global (&body definitions)
  `(defparameter global ,(cons 'list
			       (loop :for definition :in definitions :collect
				    `(cons (quote ,(first definition)) ,(second definition))))))
