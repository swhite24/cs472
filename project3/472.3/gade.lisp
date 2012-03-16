;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GA & DE shared constants & functions ;;;;;;;;;;

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
  (id 0)
  (genes (loop for x from 1 to 8
	      collect (1+ (randi 10)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rat_mem methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update_mem ((mem rat_mem) candidate_func freq
		       obj_func)
  "update each rat, add plants, and remove dead rats"
  (with-slots (all_plants all_rats gen) mem
    (maphash #'(lambda (key r) 
		 (update_rat r mem candidate_func
			     obj_func)) all_rats)
    (add_plants mem)
    (kill_rats mem)
    (summarize mem freq)
    (incf gen)))

(defmethod add_rat ((mem rat_mem) new_rat)
  "add rat to list of living rats..if no rat is passed
   a default will be created"
  (with-slots (all_rats current) mem
    (setf (gethash current all_rats)
	  (if (null new_rat)
	      (make-rat :id current)
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

(defmethod kill_rats ((mem rat_mem))
  "kills off rats with energy <= 0" 
  (with-slots (all_rats) mem
    (let ((dead_rats '()))
      (maphash #'(lambda (key r)
		   (if (<= (rat-energy r) 0)
		       (push key dead_rats)))
	       all_rats)
      (dolist (key dead_rats)
	(incf *dead_rats*)
	(remhash key all_rats)))))

(defmethod summarize ((mem rat_mem) n)
  "prints summary of population every n generations"
  (with-slots (gen all_rats) mem    
    (if (= (mod (rat_mem-gen mem) n) 0)
	(progn (format t "~%gen: ~a living rats: ~a "
		       gen (hash-table-count all_rats))
	       (format t "dead rats: ~a plants eaten: ~a~%"
		       *dead_rats* *plants_eaten*)
	       (average_gene mem)))))

(defmethod average_gene ((mem rat_mem))
  "print average gene for all living rats"
  (with-slots (all_rats) mem
    (format t "Average gene: ")
    (let ((total_gene (make-list 8 :initial-element 0)))      
      (maphash #'(lambda (key r)
		   (setf total_gene (mapcar #'+ total_gene (rat-genes r))))
	       all_rats)
      (mapc #'(lambda (x) (format t "~5,2F " x))
	    (mapcar #'(lambda (x) (/ x (hash-table-count all_rats)))
		    total_gene))
      (format t "~%"))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rat methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update_rat ((r rat) (mem rat_mem) candidate_func
		       obj_func)
  "a day in the life of a rat"
  (turn_rat r)
  (move_rat r)
  (eat_rat r mem)
  (reproduce_rat r mem candidate_func)
  (funcall obj_func r mem #'score_rat_energy))

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

(defmethod reproduce_rat ((r rat) (mem rat_mem) candidate_func)
  "if a rat has enough energy, create a child with genes
   from de_candidate, then check who survives"
  (with-slots (energy id creation) r
    (when (and (>= energy *reproduction-energy*)
	       (> (rat_mem-gen mem) creation))
      (setf energy (ash energy -1))
      (let ((child (copy-structure r)))
	(setf (rat-genes child) (funcall candidate_func mem r)
	      (rat-creation child) (rat_mem-gen mem)
	      (rat-parent child) id
	      (rat-id child) (1+ (rat_mem-current mem)))
	(add_rat mem child)))))

(defmethod single_obj ((r rat) (mem rat_mem) obj_func)
  "check if rat is old enough to be killed, then compare
   its score with parent score.  loser dies."
  (with-slots (energy creation parent) r
    (with-slots (all_rats gen) mem
      (when (and (>= (- gen creation) 25)
		 (gethash parent all_rats))
	(if (<= (funcall obj_func r mem)
		(funcall obj_func (gethash parent all_rats) 
				    mem))
	    (setf energy 0)
	    (setf (rat-energy 
		   (gethash parent all_rats)) 
		  0))))))

(defmethod two_obj ((r rat) (mem rat_mem) obj_func)
  "check if rat is old enough to be killed, then compare
   energy and children score with parent.  If one dominates
   the other, the other dies."
  (with-slots (parent creation energy) r
    (with-slots (all_rats gen) mem
      (when (and (>= (- gen creation) 25)
		 (gethash parent all_rats))
	(let* ((p (gethash parent all_rats))
	       (child_score_en (score_rat_energy r mem))
	       (child_score_ch (score_rat_children r mem))
	       (p_score_en (score_rat_energy p mem))
	       (p_score_ch (score_rat_children p mem)))
	  (if (and (<= child_score_en p_score_en)
		   (<= child_score_ch p_score_ch))
	      (setf energy 0))
	  (if (and (<= p_score_en child_score_en)
		   (<= p_score_ch child_score_ch))
	      (setf (rat-energy (gethash parent all_rats))
		    0)))))))

(defmethod kill_rat ((r rat))
  "true if energy <= 0"
  (with-slots (energy) r
    (if (<= energy 0)
	(progn (incf *dead_rats*) t))))

(defmethod score_rat_genes ((r rat))
  "scores rat based on percentage of values in either
   the front or back of gene, max of 1"
  (with-slots (genes) r
    (let ((genes_sum (apply #'+ genes))
	  (early_sum (apply #'+ (butlast genes 5)))
	  (late_sum (apply #'+ (last genes 3))))
      (max (/ early_sum genes_sum)
	   (/ late_sum genes_sum)))))

(defmethod score_rat_children ((r rat) (mem rat_mem))
  "score rat based on the number times a child was
   successfully created, max of 1"
  (with-slots (creation id) r
    (with-slots (all_rats gen) mem
      (let ((child_count 0))
	(maphash #'(lambda (key r)
		     (when (= (rat-parent r) id)
		       (incf child_count)))
		 all_rats)
	(/ child_count
	   (- gen creation))))))

(defmethod score_rat_energy ((r rat) (mem rat_mem))
  "scores rat based on energy, max of 1"
  (with-slots (energy) r
    (/ energy 1000)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run_alg (alg c_freq scale_fact gens n init summarize_freq 
		obj_func mem)
  (when (> n 0)
    (setf *dead_rats* 0)
    (setf *plants_eaten* 0)
    ;; initial population - large enough 
    (dotimes (i init)
      (add_rat mem nil))
    ;; conduct generations
    (dotimes (i gens)
      (update_mem mem alg summarize_freq obj_func))
    (summarize mem summarize_freq)
    (run_alg alg c_freq scale_fact gens (1- n) init  
	     summarize_freq obj_func
	     (make-rat_mem :c_freq c_freq
			   :scale_fact scale_fact))))

(defun print_rats (mem)
  (maphash #'(lambda (key r)
	       (print r))
	   (rat_mem-all_rats mem)))

