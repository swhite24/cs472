(defparameter *width* 1000)
(defparameter *height* 1000)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant_energy* 80)

(defun rat_de (&key (c_freq 0.3) (scale_fact 0.7) (pop_size 10) 
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact
				  :pop_size pop_size)))
  (dotimes (i 5)
    (add_rat mem)
    (add_plants mem))
  (print (rat_mem-all_rats mem))
  (maphash #'(lambda (key val)
	       (print `(key -  ,key   val -  ,val)))
	   (rat_mem-all_plants mem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory of all rats & plants ;;;;;;;;;;;;;;;;;;;
(defstruct rat_mem
  (c_freq 0.5)
  (scale_fact 0.3)
  (pop_size 10)
  (all_rats '())
  (all_plants (make-hash-table :test #'equal)))

(defmethod any_rat ((mem rat_mem))
  (nth (randi (length (rat_mem-all_rats mem)))
       (rat_mem-all_rats mem)))

(defmethod add_rat ((mem rat_mem))
  (with-slots (all_rats) mem
    (push (make-rat) all_rats)))

(defmethod random_plant ((mem rat_mem) left top width height)
  (with-slots (all_plants) mem
    (let ((pos (cons (+ left (random width)) (+ top (random height)))))
      (setf (gethash pos all_plants) t))))

(defmethod add_plants ((mem rat_mem))
  (apply #'random_plant (cons mem *jungle*))
  (random_plant mem 0 0 *width* *height*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct for rat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct rat
  (x (ash *width* -1))
  (y (ash *height* -1))
  (energy 1000)
  (dir 0)
  (genes (loop for x from 1 to 8
	      collect (1+ (randi 10)))))

(defmethod move_rat ((r rat))
  (with-slots (dir x y energy) r
    (setf x (mod (+ x (cond ((and (>= dir 2) (< dir 5)) 1)
				    ((or (= dir 1) (= dir 5)) 0)
				    (t -1))
		    *width*) *width*)
	  y (mod (+ y (cond ((and (>= dir 0) (< dir 3)) -1)
			    ((and (>= dir 4) (< dir 7)) 1)
			    (t 0))
		    *height*) *height*))
    (decf energy)))

(defmethod turn_rat ((r rat))
  (with-slots (dir genes) r
    (let ((x (randi (apply #'+ genes))))
      (labels ((angle (genes x)
		 (let ((xnu (- x (car genes))))
		   (if (< xnu 0)
		       0
		       (1+ (angle (cdr genes) xnu))))))
	(setf dir
	      (mod (+ dir (angle genes x)) 8))))))

(defmethod eat_rat ((r rat) (mem rat_mem))
  (with-slots (x y energy) r
    (with-slots (all_plants) mem
      (let ((pos (cons x y)))
	(when (gethash pos all_plants)
	  (incf energy *plant_energy*)
	  (remhash pos all_plants))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun de_candidate (mem parent)
  (labels ((candidate1 (x y z)
	     (round (+ x (* (rat_mem-scale_fact mem)
			    (- y z)))))
	   (cross-over (parent child)
	     (if (<= (randf 1.0) (rat_mem-c_freq mem))
		 parent child)))
    (mapcar #'cross-over parent
	    (mapcar #'candidate1
		    (rat-genes (any_rat mem)) 
		    (rat-genes (any_rat mem))
		    (rat-genes (any_rat mem))))))
		    
