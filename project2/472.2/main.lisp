(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"2c.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () (tests))
