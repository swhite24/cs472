(load "main.lisp")

(format t "~4:@<~a~>~6:@<~a~>~11:@<~a~>~11:@<~a~>~11:@<~a~>~11:@<~a~>~%"
	'b 'n 'dfs 'dfid_dfs 'beam 'dfid_beam)

(let ((b_vals '(2 4 8 16 24 48))
      (n_vals '(40 80 160 325 650 1250 2500 5000)))
  (dolist (b b_vals)
    (dolist (n n_vals)
      (format t "~4:@<~a~>" b)
      (format t "~6:@<~a~>" n)
      (time-of-1000 (!test_dfs n b))
      (time-of-1000 (!test_dfid_dfs n b))
      (time-of-1000 (!test_beam n b))
      (time-of-1000 (!test_dfid_beam n b))
      (format t "~%"))))