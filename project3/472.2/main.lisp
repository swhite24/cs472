(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"system.lisp"
		"wizards_game.lisp"
		"2a.lisp"
		"2b.lisp"
		"2c.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () (tests))
