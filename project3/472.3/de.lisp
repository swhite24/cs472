;; size of landscape
(defparameter *width* 100)
(defparameter *height* 30)

;; oasis in landscape
(defparameter *jungle* '(45 10 10 10))

;; constants
(defparameter *plant_energy* 80)
(defparameter *reproduction-energy* 200)

;; counters
(defparameter *dead_rats* 0)
(defparameter *plants_eaten* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory of all rats & plants ;;;;;;;;;;;;;;;;;;;
(defstruct rat_mem
  (c_freq 0.5)
  (scale_fact 0.3)
  ;; unique id for future rats
  (current 0)
  ;; generation counter, used for aging rats
  (gen 0)
  (all_rats (make-hash-table :test #'equal))
  (all_plants (make-hash-table :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct for rat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct rat
  (x (ash *width* -1))
  (y (ash *height* -1))
  (energy 1000)
  (dir 0)
  ;; unique id of parent in all_rats
  (parent 0)
  ;; generation when rat was created
  (creation 0)
  (genes (loop for x from 1 to 8
	      collect (1+ (randi 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rat_mem methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add_rat ((mem rat_mem) new_rat)
  "add rat to list of living rats..if no rat is passed
   a default will be created"
  (with-slots (all_rats current) mem
    (setf (gethash current all_rats)
	  (if (null new_rat)
	      (make-rat)
	      new_rat))
    (incf current)))

(defmethod any_rat ((mem rat_mem))
  "select any currently living rat"
  (with-slots (all_rats current) mem
    (let ((rand (randi current)))
      (or (gethash rand all_rats)
	  (any_rat mem)))))

(defmethod random_plant ((mem rat_mem) left top width height)
  "create plant within given constraints"
  (with-slots (all_plants) mem
    (let ((pos (cons (+ left (random width)) (+ top (random height)))))
      (setf (gethash pos all_plants) t))))

(defmethod add_plants ((mem rat_mem))
  "add plant in jungle and somewhere else"
  (apply #'random_plant (cons mem *jungle*))
  (random_plant mem 0 0 *width* *height*))

(defmethod update_mem ((mem rat_mem))
  "update each rat, add plants, and remove dead rats"
  (with-slots (all_plants all_rats gen) mem
    (maphash #'(lambda (key r) (update_rat r mem)) all_rats)
    (add_plants mem)
    (let ((dead_rats '()))
      (maphash #'(lambda (key r)
		   (if (<= (rat-energy r) 0)
		       (push key dead_rats)))
	       all_rats)
      (dolist (key dead_rats)
	(incf *dead_rats*)
	(remhash key all_rats)))
    (incf gen)))

(defmethod results ((mem rat_mem))
  "print average gene for all living rats"
  (with-slots (all_rats) mem
    (format t "Average gene: ")
    (let ((total_gene (make-list 8 :initial-element 0)))      
      (maphash #'(lambda (key r)
		   (setf total_gene (mapcar #'+ total_gene (rat-genes r))))
	       all_rats)
      (mapc #'(lambda (x) (format t "~f " x))
	    (mapcar #'(lambda (x) (/ x (hash-table-count all_rats)))
		    total_gene)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rat methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod move_rat ((r rat))
  "move rat in direction indicated by dir, then decrement energy"
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
  "turn rat based on genes"
  (with-slots (dir genes) r
    (let ((x (randi (apply #'+ genes))))
      (labels ((angle (genes x)
		 (let ((xnu (- x (car genes))))
		   (if (< xnu 0)
		       0
		       (1+ (angle (cdr genes) xnu))))))
	(setf dir
	      (mod (+ dir (angle genes x)) 8))))))

(defmethod kill_rat ((r rat))
  "true if energy <= 0"
  (with-slots (energy) r
    (if (<= energy 0)
	(progn (incf *dead_rats*) t))))

(defmethod score_rat1 ((r rat))
  "scores rat based on percentage of values in either
   the front or back of gene, max of 1"
  (with-slots (genes) r
    (let ((genes_sum (apply #'+ genes))
	  (early_sum (apply #'+ (butlast genes 5)))
	  (late_sum (apply #'+ (last genes 3))))
      (max (/ early_sum genes_sum)
	   (/ late_sum genes_sum)))))

(defmethod score_rat ((r rat))
  "scores rat based on energy, max of 1"
  (with-slots (energy) r
    (/ energy 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod eat_rat ((r rat) (mem rat_mem))
  "check if rat is near plant, if so consume it and add
   plant_energy to rats curren energy"
  (with-slots (x y energy) r
    (with-slots (all_plants) mem
      (let ((pos (cons x y)))
	(when (gethash pos all_plants)
	  (incf *plants_eaten*)
	  (incf energy *plant_energy*)
	  (remhash pos all_plants))))))

(defmethod reproduce_rat ((r rat) (mem rat_mem))
  "if a rat has enough energy, create a child with genes
   from de_candidate, then check who survives"
  (with-slots (energy) r
    (when (>= energy *reproduction-energy*)
      (setf energy (ash energy -1))
      (let ((child (copy-structure r)))
	(setf (rat-genes child) (de_candidate mem r)
	      (rat-creation child) (rat_mem-gen mem)
	      (rat-parent child) (1+ (rat_mem-current mem)))
	(add_rat mem child)))))

(defmethod survival_of_fittest ((r rat) (mem rat_mem))
  "check if rat is old enough to be tested, then compare
   its score with parent score.  loser dies."
  (with-slots (energy creation parent) r
    (with-slots (all_rats gen) mem
      (if (and (>= (- gen creation) 50)
	       (gethash parent all_rats))
	  (if (> (score_rat r)
		 (score_rat (gethash parent all_rats)))
	      (setf energy 0)
	      (setf (rat-energy (gethash parent all_rats)) 0))))))
	       
(defmethod update_rat ((r rat) (mem rat_mem))
  "a day in the life of a rat"
  (turn_rat r)
  (move_rat r)
  (eat_rat r mem)
  (reproduce_rat r mem)
  (survival_of_fittest r mem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rat_de (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) (n 10)
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact)))
  (when (> n 0)
    (setf *dead_rats* 0)
    (setf *plants_eaten* 0)
    ;; initial population
    (dotimes (i 4)
      (add_rat mem nil)) 
    (dotimes (i gens)
      (update_mem mem))
    ;(mapc #'print (rat_mem-all_rats mem))
    (format t "Number of living rats: ~a~%" 
	    (hash-table-count (rat_mem-all_rats mem)))
    (format t "Number of dead rats: ~a~%" *dead_rats*)
    ;; print average gene
    (results mem)
    (format t "~%Number of plants eaten: ~a~%~%" *plants_eaten*)
    (rat_de :c_freq c_freq :scale_fact scale_fact
	    :gens gens :n (1- n) )))

(defun de_candidate (mem parent)
  "produces a child gene based on parent and 3 others"
  (labels ((candidate1 (x y z)
	     (max 1 (round (+ x (* (rat_mem-scale_fact mem)
				   (- y z))))))
	   (cross-over (parent child)
	     (if (<= (randf 1.0) (rat_mem-c_freq mem))
		 parent child)))
    (mapcar #'cross-over (rat-genes parent)
	    (mapcar #'candidate1
		    (rat-genes (any_rat mem)) 
		    (rat-genes (any_rat mem))
		    (rat-genes (any_rat mem))))))
