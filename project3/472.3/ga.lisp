;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GA specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run_ga (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) 
	       (n 10) (summarize_freq 10)
	       (obj_func #'single_obj)
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact)))
  (run_alg #'ga_candidate c_freq scale_fact gens n 10
	   summarize_freq obj_func mem))

(defun ga_candidate (mem parent)
  "produce a child gene with mutation"
  (let* ((which (randi 8))
	 (plus_minus (randi 2))
	 (temp_genes (copy-list (rat-genes parent)))
	 (gene_to_fiddle (elt temp_genes which)))
    (setf (elt temp_genes which)
	  (min 1000 (max 1
		       (if (= plus_minus 0)
			   (- gene_to_fiddle
			      (* gene_to_fiddle
				 (rat_mem-scale_fact mem)))
			   (+ gene_to_fiddle
			      (* gene_to_fiddle
				 (rat_mem-scale_fact mem)))))))
    temp_genes))
    