#| 
############################################
de.lisp: Differential evolution
Tim Menzies, tim@menzies.us, http://menzies.us
(C) 2012 GPL 3.0

To use: (load "de.lisp")

For more on differential evolution, 
see http://tinyurl.com/83d9daa
############################################### 
|#

(defun de (&key 
	   (cf 0.5) (f 0.7) (np 10000)  
	   (d (make-d :cf cf :f f :np np)))
    "CL-USER> (de :f 0.25)
    100000 [100%] 22%  0.52 (  6  5  3  7  3  4  4  4  5  3)
     99998 [100%] 38%  0.72 (  9  7  5  9  7  5  4  6  9  7)
     99986 [100%]  4%  0.75 (  4  8  8  5  8  8  9  8  5 10)
     99520 [100%]  0%  0.75 (  5  9  7  8  5  7 10  5  7  9)
     99502 [100%]  1%  0.76 (  7 10  7 10  6  8  8  7  6  4)
     99481 [ 99%]  0%  0.76 (  9 10  6  6  6  5  9 10  7  6)
     99193 [ 99%]  3%  0.78 (  9  8 10  7  9  4  7  9  7  6)
     98706 [ 99%]  3%  0.80 (  7  6 10  9  6 10  8  6 10  7)
     97091 [ 97%]  2%  0.82 (  9  7  6  9  9  6  8  8 10  6)
     96009 [ 96%]  4%  0.85 (  9  6  9  8  7 10  8  8  7  8)
     80994 [ 81%]  3%  0.88 ( 10  7 10  8  8  7 10 10 10  9)
     66706 [ 67%]  2%  0.90 (  9  7  9 10 10 10  8  8  8 10)
     50099 [ 50%]  1%  0.90 (  9  8 10  8  8 10  7  8  9  9)
     48810 [ 49%]  2%  0.92 ( 10 10  8  9 10  9  8  8  9 10)
     20360 [ 20%]  2%  0.94 ( 10  9  9  9  9  8 10  9  9  8)
    want : (9 9 9 9 9 9 9 9 9 9)
    got  : (10 9 9 9 9 8 10 9 9 8)
    NIL
    CL-USER> 
    ; No value
    CL-USER> (de :f 0.5)
    100000 [100%] 22%  0.52 (  5  5  3  7  3  4  4  4  6  3)
     99998 [100%] 36%  0.71 (  9  7  4  9  8  6  4  6  8  6)
     99986 [100%]  4%  0.74 (  4  7 10  6  8  8  9 10  4 10)
     99913 [100%]  1%  0.75 (  6  6  6  5  9  5  9  9  7  9)
     99520 [100%]  1%  0.75 (  6 10  7  8  5  7 10  5  6  9)
     99481 [ 99%]  5%  0.79 (  8  9  6  7  6  6  9 10  8  6)
     98706 [ 99%]  1%  0.80 (  7  6 10  9  6 10  8  6 10  7)
     97091 [ 97%]  2%  0.82 (  9  7  6  9  9  6  8  8 10  6)
     96009 [ 96%]  2%  0.83 (  9  6  9  8  7 10  8  6  7  9)
     91622 [ 92%]  2%  0.85 (  8  9  8 10  8  6  6  9 10  8)
     87396 [ 87%]  1%  0.86 (  5  9 10  9  9  8 10  9  8  8)
     86596 [ 87%]  3%  0.88 (  9  9  8  9 10  6 10 10  8 10)
     71702 [ 72%]  1%  0.89 (  7 10 10  8 10 10  8 10 10 10)
     63911 [ 64%]  1%  0.89 ( 10 10  7  9 10 10 10  8  8 10)
     62597 [ 63%]  2%  0.91 ( 10  8  9  8 10 10  8  8  8  8)
     36319 [ 36%]  1%  0.92 ( 10 10  9  8 10 10  9 10 10  9)
     19782 [ 20%]  1%  0.92 (  9  9  8  8 10  8  8  9 10  9)
    want : (9 9 9 9 9 9 9 9 9 9)
    got  : (9 9 8 8 10 8 8 9 10 9)
    NIL
    CL-USER> (de :f 0.75)
    100000 [100%] 20%  0.51 (  4  5  3  7  3  4  4  4  6  3)
     99998 [100%] 30%  0.67 (  9  7  3  9 10  6  4  6  8  4)
     99991 [100%]  5%  0.70 ( 10  8  7  4  6  4  6  6  8  7)
     99986 [100%]  5%  0.74 (  4  6 10  7  8  8  9 10  4 10)
     99752 [100%]  1%  0.75 (  9  9  5  9  4  7  7  8  6  7)
     99520 [100%]  3%  0.77 (  8 10  7  8  5  7 10  5  6  9)
     99481 [ 99%]  4%  0.80 (  8  8  6  8  6  6  9 10  9  6)
     98706 [ 99%]  0%  0.80 (  7  6 10  9  6 10  8  6 10  7)
     97091 [ 97%]  2%  0.82 (  9  7  6  9  9  6  8  8 10  6)
     91622 [ 92%]  2%  0.84 (  8  9  7 10  8  6  6  9 10  8)
     91282 [ 91%]  0%  0.84 (  7  6  7 10 10  8  8  9  7 10)
     90030 [ 90%]  0%  0.84 ( 10  6  8 10 10 10 10  6  9 10)
     89737 [ 90%]  3%  0.87 (  7 10  7  8  7  8 10 10  9  8)
     86596 [ 87%]  1%  0.88 (  8 10  7  9 10  7 10 10  8 10)
     77255 [ 77%]  1%  0.89 ( 10  7  9  7 10 10  8  9  9  9)
     74050 [ 74%]  1%  0.90 (  9 10 10  8 10  9 10  8  8  7)
     67554 [ 68%]  1%  0.91 ( 10 10  9  7 10  9  8  8  9  9)
     48810 [ 49%]  1%  0.92 (  9 10  9 10 10  9  8 10  8 10)
     17140 [ 17%]  1%  0.92 ( 10 10  8  9 10 10  9  9  9 10)
    want : (9 9 9 9 9 9 9 9 9 9)
    got  : (10 10 8 9 10 10 9 9 9 10)
    "
  (reset-seed)
  (dotimes (i (d-np d)) 
    (new d))
  (let* ((zero (any d))
	 (runs (1+ (* 10 (d-np d))))
	 (got  (de-run runs runs
		       zero 
		       (closest zero d) 
		       d)))
    (format t "~&want : ~a~%got  : ~a~%"  
	    (d-goal d) got)))

(defun de-run (n runs best best-score d)
  "run n times"
  (if (zerop n)
      best
      (de-step (- n 1) runs best best-score d)))

(defun de-step (n runs best best-score d)
  "for each member of the population, perhaps
    replace it with a mutated child"
  (let* ((id           (mod n (d-np d)))
	 (parent       (gethash id (d-all d) (any d)))
	 (child        ;(any d) 
		       (candidate d parent)
	               )
	 (parent-score (closest parent d))
	 (child-score  (closest child  d))
	 (winner       parent)
	 (winner-score parent-score))
    (when (> child-score parent-score)
      (setf winner                 child
	    winner-score           child-score
	    (gethash id (d-all d)) child))
    (if (<= winner-score best-score)
	(de-run n runs best   best-score   d)
	(progn
	  (format t "~&~6d [~3d%]~3d% ~5,2f (~{~3d~})~%" 
		  n
		  (round (* 100 (/ n runs)))
		  (round 
		   (* 100 (/ (- winner-score best-score)
			     best-score)))
		  winner-score winner)
	  (de-run n runs winner winner-score d)))))

(defun candidate (d parent)
  "The DE mutator candidate = any + f*(another - yetAnother)"
  (labels ((candidate1 (x y z r &aux (f (d-f d)))
	     (trim r (round (+ x (* f (- y z))))))
	   (cross-over (parent c)
	     (if (<= (randf 1.0) (d-cf d)) parent c)))
    (mapcar #'cross-over parent 
	    (mapcar #'candidate1 
		    (any d) (any d) (any d) (d-dd d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct range 
 " defining ranges for each attribute"
  (name (gensym "NAME"))
  (w 1)
  (min 0) 
  (max 10))

(defmethod any ((r range))
  "pull a random number from this range"
  (with-slots (min max) r
    (round (+ min (* (- max min) (randf 1.0))))))

(defmethod trim ((r range) n)
  "demand that a number falls within range"
  (with-slots (min max) r
    (cond ((> n max) max)
	  ((< n min) min)
	  (t         n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct d 
  "Defining the working memory for our de"
  (cf 0.5)
  (f  0.3)
  (np 10000)
  (dd (loop for x from 1 to 10
	 collect (make-range  :min 0 :max 10)))
  (goal '(9 9 9 9 9 9 9 9 9 9)) ; <=== secret goal
  (all   (make-hash-table)))


(defmethod any-id ((d d))
  "Select any id of the population"
  (1+ (randi  (hash-table-count (d-all d)))))

(defmethod  any ((d d))
  "Select any member of the population"
  (or (gethash (any-id d) (d-all d))
      (any d)))

(defmethod new ((x d))
  "Create a new member of the population,store 
   at old size + 1"
  (with-slots (dd n all) x
    (let ((where (+ 1 (hash-table-count all))))
      (setf (gethash where all)
	    (mapcar #'any dd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun closest (x d)
  "objective function to be maximizes:
    compliment of distance between x and goal"
  (- 1 (dist x (d-goal d) (d-dd d))))

(defun !closest (&aux (d (make-d)))
  "testing closest. should return 1"
  (closest (d-goal d) d))

(defun dist(x y dd &aux (sum 0) (ws 0))
  "normalized distance between two things"
  (labels ((norm (n r)
	     (/ (- n (range-min r)) 
		(- (range-max r) (range-min r))))
	   (inc (a b r)
	     (let ((w (range-w r)))
	       (incf ws w)
	       (incf sum 
		     (* w (expt (- (norm a r) 
				   (norm b r)) 
				2))))))
    (mapc #'inc x y dd)
    (/ (sqrt sum) 
       (sqrt ws))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils. a random number generator where you
; can control the seed

(let* ((seed0      10013)
       (seed       seed0)
       (multiplier 16807.0d0)
       (modulus    2147483647.0d0))
  (defun reset-seed ()  (setf seed seed0))
  (defun randf      (n) (* n (- 1.0d0 (park-miller-randomizer))))
  (defun randi      (n) (floor (* n (/ (randf 1000.0) 1000))))
  (defun park-miller-randomizer ()
    "cycle= 2,147,483,646 numbers"
    (setf seed (mod (* multiplier seed) modulus))
    (/ seed modulus))
)

    