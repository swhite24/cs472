;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used to generate runtimes for each algorithm with ;;;
;; a varying number of generations ;;;;;;;;;;;;;;;;;;;;;

(load "main")

(format t "~8:@<~a~>~11:@<~a~>~11:@<~a~>~11:@<~a~>~11:@<~a~>~%"
	'n 'ga_single 'de_single 'ga_double 'de_double)

(let ((start_time (get-internal-run-time))
      (n          1))
  (dolist (pop_size '(1000 5000 10000 25000 50000 75000 100000))
    (format t "~8:@<~a~>" pop_size)
    (format t "~11:@<~f~>" (time-it n (run_ga :gens pop_size
					      :n 1)))
    (format t "~11:@<~f~>" (time-it n (run_de :gens pop_size
					      :n 1)))
    (format t "~11:@<~f~>" (time-it n (run_ga :gens pop_size
					      :n 1
					      :obj_func #'two_obj)))
    (format t "~11:@<~f~>~%" (time-it n (run_de :gens pop_size
						:n 1
						:obj_func #'two_obj))))
  (format t "~%~%Total time for all executions: ~f~%"
	  (/ (- (get-internal-run-time) start_time)
	     internal-time-units-per-second)))