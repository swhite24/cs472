(defparameter *width* 1000)
(defparameter *height* 1000)

(defun rat_de (&key (c_freq 0.3) (scale_fact 0.7) (pop_size 10) 
	       (mem (make-rat_mem :c_freq c_freq
				  :scale_fact scale_fact
				  :pop_size pop_size)))
  (dotimes (i (rat_mem-pop_size mem))
    (add_rat mem))
  (rat_mem-all mem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct rat_mem
  (c_freq 0.5)
  (scale_fact 0.3)
  (pop_size 10)
  (all '()))

(defmethod any_rat ((mem rat_mem))
  (nth (randi (length (rat_mem-all mem)))
       (rat_mem-all mem)))

(defmethod add_rat ((mem rat_mem))
  (with-slots (all) mem
    (push (make-rat) all)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct rat
  (x (ash *width* -1))
  (y (ash *height* -1))
  (energy 1000)
  (dir 0)
  (genes (loop for x from 1 to 8
	      collect (1+ (randi 10)))))

(defmethod plus_energy ((r rat))
  (with-slots (energy) r
    (incf energy)))