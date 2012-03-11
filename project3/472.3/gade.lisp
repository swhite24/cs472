(defun de-step (n runs best best-score d parent_func cand_func)
  "for each member of the population, perhaps
    replace it with a mutated child"
  (let* ((id (mod n (d-np d)))
	 (parent (funcall parent_func id d))
	 (child  (funcall cand_func d parent))
	 (parent-score (closest_l parent d))
	 (child-score (closest child d))
	 (winner parent)
	 (winner-score parent-score))
    (when (> child-score parent-score)
      (setf winner child
	    winner-score child-score
	    (gethash id (d-all d)) child))
    (if (<= winner-score best-score)
	(de-run n runs best best-score d)
	(progn 
	  (format t "~&~6d [~3d%]~3d% ~5,2f (~{~3d~})~%" 
		  n
		  (round (* 100 (/ n runs)))
		  (round 
		   (* 100 (/ (- winner-score best-score)
			     best-score)))
		  winner-score winner)
	  (de-run n runs winner winner-score d)))))