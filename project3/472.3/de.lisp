;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DE specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run_de (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) 
	       (n 10) (summarize_freq 10) (np 10000)
	       (obj_func #'single_obj)
	       (compare_func #'get_parent)
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact)))
  (run_alg #'de_candidate c_freq scale_fact gens n np
	   summarize_freq obj_func compare_func mem))

(defun run_de_dec (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) 
		   (n 10) (summarize_freq 10)
		   (obj_func #'single_obj)
		   (mem (make-rat_mem :c_freq c_freq
				      :scale_fact scale_fact)))
  (run_alg #'de_candidate c_freq scale_fact gens n 10
	   summarize_freq obj_func #'closest_dec mem))

(defun run_de_obj (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) 
	       (n 10) (summarize_freq 10)
	       (obj_func #'single_obj)
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact)))
  (run_alg #'de_candidate c_freq scale_fact gens n 10
	   summarize_freq obj_func #'closest_obj mem))

(defun de_candidate (mem parent)
  "produces a child gene based on parent and 3 others"
  (labels ((candidate1 (x y z)
	     (min 10 
		  (max 1 (round (+ x (* (rat_mem-scale_fact mem)
					(- y z)))))))
	   (cross-over (parent child)
	     (if (<= (randf 1.0) (rat_mem-c_freq mem))
		 parent child)))
    (mapcar #'cross-over (rat-genes parent)
	    (mapcar #'candidate1
		    (rat-genes (any_rat mem)) 
		    (rat-genes (any_rat mem))
		    (rat-genes (any_rat mem))))))


