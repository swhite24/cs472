(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"system.lisp"
		"pick.lisp"
		"de.lisp"
		"de_orig.lisp"
		"evolution.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () (tests))
