;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used to generate average gene for single and ;;;;;;;
;; multi-objective runs. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "main.lisp")

(labels ((header (name size) 
	   (format t "~11:@<~a~>~8:@<~a~>" name size)))
    (dolist (pop_size '(1000 5000 10000 25000 50000
			75000 100000))
      (header 'ga_single pop_size)
      (run_ga :gens pop_size :n 1)
      (header 'de_single pop_size)
      (run_de :gens pop_size :n 1)
      (header 'ga_double pop_size)
      (run_ga :gens pop_size :n 1 :obj_func #'two_obj)
      (header 'de_double pop_size)
      (run_de :gens pop_size :n 1 :obj_func #'two_obj)))