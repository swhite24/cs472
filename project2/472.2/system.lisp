(defun shell->output (com &rest args)
  (with-open-stream  (stream (shell com args)) 
    (stream->list stream)))

(defun shell (com args) 
  (sb-ext:process-output 
   (sb-ext:run-program  com args :output :stream)))

(defun stream->list (str)
  (let ((line (read-line str nil nil)))
    (if line
	(cons line (stream->list str)))))

(deftest !shell->output ()
  (test 
   '("boot.lisp" "city.dot" "city.dot.png" "go" "go_bash" "go_lisp"
     "graph-util.lisp" "known-city.dot" "known-city.dot.png" "tricks.lisp"
     "wumpus.lisp")
   (shell->output "/bin/ls" ".")))

