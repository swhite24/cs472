;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used to generate average gene with constant number ;;
;; of generations and varying scale factor ;;;;;;;;;;;;;

(load "main.lisp")

(let ((pop_size 100000))
  (labels ((header (name size) 
	     (format t "~11:@<~a~>~8:@<~a~>" name size)))
    (dolist (sf '(0.1 0.3 0.5 0.7 0.9))
      (header 'ga_single sf)
      (run_ga :gens pop_size :n 1 :scale_fact sf)
      (header 'de_single sf)
      (run_de :gens pop_size :n 1 :scale_fact sf)
      (header 'ga_double sf)
      (run_ga :gens pop_size :n 1 :scale_fact sf :obj_func #'two_obj)
      (header 'de_double sf)
      (run_de :gens pop_size :n 1 :scale_fact sf :obj_func #'two_obj))))