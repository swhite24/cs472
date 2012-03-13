(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant_energy* 80)
(defparameter *reproduction-energy* 200)
(defparameter *dead_rats* 0)
(defparameter *plants_eaten* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory of all rats & plants ;;;;;;;;;;;;;;;;;;;
(defstruct rat_mem
  (c_freq 0.5)
  (scale_fact 0.3)
  (all_rats '())
  (all_plants (make-hash-table :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct for rat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct rat
  (x (ash *width* -1))
  (y (ash *height* -1))
  (energy 1000)
  (dir 0)
  (genes (loop for x from 1 to 8
	      collect (1+ (randi 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rat_mem methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod any_rat ((mem rat_mem))
  "select any currently living rat"
  (nth (randi (length (rat_mem-all_rats mem)))
       (rat_mem-all_rats mem)))

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
  (with-slots (all_plants all_rats) mem
    (mapc #'(lambda (r) (update_rat r mem)) all_rats)
    (add_plants mem)
    (setf all_rats (remove-if #'kill_rat all_rats))))

(defmethod results ((mem rat_mem))
  "print average gene for all living rats"
  (with-slots (all_rats) mem
    (format t "Average gene: ")
    (mapc #'(lambda (x) (format t "~f " x))
	    (mapcar #'(lambda (x) (/ x (length all_rats)))
		    (sum_lists (mapcar #'rat-genes all_rats))))))

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
  "true if energy < 0"
  (with-slots (energy) r
    (if (<= energy 0)
	(progn (incf *dead_rats*) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add_rat ((mem rat_mem) new_rat)
  "add rat to list of living rats..if no rat is passed
   a default will be created"
  (with-slots (all_rats) mem
    (if (null new_rat)
	(push (make-rat) all_rats)
	(push new_rat all_rats))))

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
   from de_candidate"
  (with-slots (energy) r
    (when (>= energy *reproduction-energy*)
      (setf energy (ash energy -1))
      (let ((child (copy-structure r)))
	(setf (rat-genes child) (de_candidate mem r))
	(add_rat mem child)))))

(defmethod update_rat ((r rat) (mem rat_mem))
  "life-cycle of rat"
  (turn_rat r)
  (move_rat r)
  (eat_rat r mem)
  (reproduce_rat r mem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rat_de (&key (c_freq 0.5) (scale_fact 0.7) (gens 100) 
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact)))
  (setf *dead_rats* 0)
  (setf *plants_eaten* 0)
  ;; initial population
  (dotimes (i 4)
    (add_rat mem nil)) 
  (dotimes (i gens)
    (update_mem mem))
  ;(mapc #'print (rat_mem-all_rats mem))
  (format t "Number of living rats: ~a~%" (length (rat_mem-all_rats mem)))
  (format t "Number of dead rats: ~a~%" *dead_rats*)
  (results mem)
  (format t "~%Number of plants eaten: ~a~%" *plants_eaten*))

(defun rat_de_run (n runs mem)
  (if (zerop n)
      t
      (rat_de_step (- n 1) runs mem)))

(defun rat_de_step (n runs mem)
  )

(defun de_candidate (mem parent)
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
		    
(defun sum_lists (x)
  "sums a list containing lists"
  (labels ((sum_it (l)
	     (if (null l)
		 (make-list (length x) :initial-element 0)		 
		 (mapcar #'+ (car l) (sum_it (cdr l))))))
    (sum_it x)))