(defun ga_parents (id d)
  (list (gethash id (d-all d) (any d))
	(gethash id (d-all d) (any d))))

(defun de_parent (id d)
  (list (gethash id (d-all d) (any d))))

(defun de_candidate (d parent)
  "The DE mutator candidate = any + f*(another - yetAnother)"
  (labels ((candidate1 (x y z r &aux (f (d-f d)))
	     (trim r (round (+ x (* f (- y z))))))
	   (cross-over (parent c)
	     (if (<= (randf 1.0) (d-cf d)) parent c)))
    (mapcar #'cross-over (car parent)
	    (mapcar #'candidate1 
		    (any d) (any d) (any d) (d-dd d)))))

(defun ga_candidate (d parent)
  "The DE mutator candidate = any + f*(another - yetAnother)"
  (labels ((candidate1 (x y z r &aux (f (d-f d)))
	     (trim r (round (+ x (* f (- y z))))))
	   (cross-over (parent c)
	     (if (<= (randf 1.0) (d-cf d)) parent c)))
    (mapcar #'cross-over parent 
	    (mapcar #'candidate1 
		    (any d) (any d) (any d) (d-dd d)))))
  