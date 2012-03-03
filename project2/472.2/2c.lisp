;; N-ARY TREE UTILS ;;

(defun binary_tree(x)
  (list (* 2 x)
	(1+ (* 2 x))))

(defun nthary_tree (x b)
  "get children of parent x with branching factor b"
  (let* ((val (* b x))
	 (l (list val (+ val 1))))
    (dotimes (x (- b 2) l)
      (push (- val (+ 1 x)) l))))

(defun finite_nthary_tree (n b)
  "successer function that generates n-ary tree to depth n"
  #'(lambda (x)
      (remove-if #'(lambda (child)
		     (>= (node_depth child b)
			 n))
		 (nthary_tree x b))))


(defun node_depth (val b_factor)
  "height of node in n-ary tree with branching factor b_factor"
  (floor (log (* (- b_factor 1) val) b_factor)))

;; SEARCH-UTILS ;;
(defun tree-search (states goal-p successors combiner)
 ; (blab :search "~&;; Search: ~a" states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) 
	 (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun prepend (x y)
  (append y x))

(defun blab (what str data)
  (declare (ignore what))
  (format t str data))

(defun is (n)
  #'(lambda (x) (equal x n)))

(defun within (x y) 
  (+ x (* (- y x) 
	  (randf 1.0))))

(defun any (l)
  (nth (randi (length l)) l))


;; DFID & DFS ;;

(defun depth-first-search (start goal-p successors)
  (tree-search (list start)  ; initially, our states are just "start"
	       goal-p        ; a function to recognize "success"
	       successors    ; a generator of states next to current
	       #'append      ; dfs= explore new before old
	       ))

(defun dfid_dfs_search (start goal-p successors max b_factor &optional (now 1))
  "DFID using a n-ary tree with branching factor b_factor"
  (unless (> now max)
   ; (blab :dfid "~&;; Extending leash to : ~a" now)
    (or (depth-first-search start 
			     goal-p 
			     (funcall successors now b_factor))
	(dfid_dfs_search start goal-p successors max b_factor (1+ now)))))

;; DFID & BEAM ;;
(defun dfid_beam_search (start goal-p successors max b_factor cost-fn 
			beam_width &optional (now 1))
  (unless (> now max)
   ; (blab :dfid_beam "~&;; Extending leash to : ~a" now)
    (or (beam-search start
		     goal-p
		     (funcall successors now b_factor)
		     cost-fn
		     beam_width)
	(dfid_beam_search start goal-p successors max b_factor
			  cost-fn beam_width (1+ now)))))

(defun beam-search (start goal-p successors cost-fn beam-width)
  (tree-search (list start) goal-p successors
	       (beam_combine cost-fn beam-width)))

;; BEAM UTILS ;;

(defun diff (num) 
  "Return the function that finds the difference from num."
  #'(lambda (x) 
      (if (> x num)
	  most-positive-fixnum
	  (- num x))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun beam_combine (cost-fn beam-width)
  #'(lambda (old new)
      (let ((sorted (funcall (sorter cost-fn) old new)))
	(if (> beam-width (length sorted))
	    sorted
	    (subseq sorted 0 beam-width)))))

;; TEST FUNCTIONS ;;

(deftest !test_dfid_dfs (&optional (n 80) (b 2))
  (let* ((target (1+ (randi n)))
	 (max (1+ (node_depth n b))))
    (dfid_dfs_search 1
		     (is target)
		     #'finite_nthary_tree
		     max
		     b)))

(deftest !test_dfs (&optional (n 80) (b 2))
  (let* ((target (1+ (randi n)))
	 (max (1+ (node_depth n b))))	 
    (depth-first-search 1 
			(is target)
			(funcall #'finite_nthary_tree  max b))))

(deftest !test_dfid_beam (&optional (n 80) (b 2) (b_factor 10))
  (let* ((target (1+ (randi n)))
	 (max (1+ (node_depth n b))))
    (dfid_beam_search 1 (is target)  
		      #'finite_nthary_tree
		      max
		      b
		      (diff target)
		      b_factor)))

(deftest !test_beam (&optional (n 80) (b 2) (b_factor 10))
  (let* ((target (1+ (randi n)))
	 (max (1+ (node_depth n b))))
    (beam-search 1
		 (is target)
		 (funcall #'finite_nthary_tree max b)
		 (diff target)
		 b_factor)))


;; macro used to find average real & run time of each search
(defmacro time-of-1000 (&body to_do)
  (reset-seed)
  (let ((real1 (gensym))
	(run1 (gensym))
	(real2 (gensym))
	(run2 (gensym))
	(done (gensym))
	(reals (gensym))
	(runs (gensym)))
    `(let ((,reals '())
	   (,runs '()))
       (dotimes (x 1000)
	 (let ((,real1 (get-internal-real-time))
	       (,run1 (get-internal-run-time)))
	   (progn ,@to_do)
	   (let ((,real2 (get-internal-real-time))
		 (,run2 (get-internal-run-time)))
	     (push (/ (- ,real2 ,real1) internal-time-units-per-second)
		   ,reals)
	     (push (/ (- ,run2 ,run1) internal-time-units-per-second)
		   ,runs))))
       (format t "~11:@<~f~>" (/ (apply #'+ ,runs) (length ,runs))))))
	