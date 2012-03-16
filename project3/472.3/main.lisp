(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"system.lisp"
		"pick.lisp"
		"gade.lisp"
		"de.lisp"
		"ga.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () (tests))
