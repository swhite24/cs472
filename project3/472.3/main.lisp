(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"unused/system.lisp"
		"unused/pick.lisp"
		"gade.lisp"
		"de.lisp"
		"ga.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () (tests))
