(defun within (x y)
  "generate random number between x and y"
  (+ x
     (* (- y x) (randf 1.0))))

(defun any (alist)
  "select random item from list"
  (nth (randi (length alist)) alist))



(defun landscape (n f1 f2)
  "create a random landscape of n nodes with a fanout between f1 & f2"
  ; create n nodes
  (make_nodes n)
  ; create edges
  (make_edges2 f1 f2)
  ; create objects
  (make_objs n)
  (add_wizard))


(defun make_nodes(n)
  "create n nodes and add to *nodes*"
  (dotimes (x n)
    (let ((temp_node_name (gensym "garden")))
      (push (list temp_node_name
		  `(You are in ,temp_node_name))
	    *nodes*)
      (push (list temp_node_name nil nil) *node_pos*))))

(defun init_edges ()
  "add initial sublist to *edges* for each node"
  (dolist (node *nodes*)
    (if (null (assoc (car node) *edges*))
	(push (list (car node)) *edges*))))

(defun make_edges (f1 f2)
  "create f1 to f2 edges for nodes"
  (init_edges)
  (labels 
       ; number of adjacent nodes
      ((neighbor_count (sym) (length (rest (assoc sym *edges*))))
       ; insert bi-directional edge into *edges*
       (two-way (sym1 sym2 dirs)
	 (push (list sym2 (car dirs)) 
	       (rest (assoc sym1 *edges*)))
	 (push (list sym1 (cadr dirs)) 
	       (rest (assoc sym2 *edges*)))))
    (dolist (node *nodes*)
      (let ((num_edges (- (round (within f1 f2)) 
			  (neighbor_count (car node))))
	    (node_count 0)
	    temp_node)
	(dotimes (x num_edges)
	  ; do-while loop to find node with < f2 neighbors
	  ; node_count ensures loop breaks
	  (loop do
	       (setf temp_node (car (any *nodes*)))
	       (incf node_count)
	     while (and (not (< (neighbor_count temp_node) f2))
			(< node_count (length *nodes*))))
	  ; make sure loop broke by finding node
	  (if (< node_count (length *nodes*))
	      ; add edges to both nodes
	      (two-way (car node) 
		       temp_node
		       (case (round (within 1 4))
			 (1 (list 'east 'west))
			 (2 (list 'west 'east))
			 (3 (list 'north 'south))
			 (4 (list 'south 'north)))))	  
	  ; reset node_count
	  (setf node_count 0))))))

(defun make_objs (n)
  "create n objects and assign random location"
  (dotimes (x n)
    (let ((temp_obj_name (gensym "sword")))
      (push temp_obj_name *objects*)
      (push (list temp_obj_name (car (any *nodes*)))
	    *object-locations*))))

; global used to hold nodes visited via rwalk
(defparameter *prev_nodes* '())

(defun objs_at (loc)
  "return list of objects at node loc"
  (remove nil
	  (remove-duplicates (mapcar #'(lambda (pair)
					 (if (member loc pair)
					     (car pair)))
				     *object-locations*))))

(defun gathered_objs ()
  "return list of objects gathered at all *prev_nodes*"
  (flatten (remove nil (mapcar #'objs_at
			       (remove-duplicates *prev_nodes*)))))

(defun rwalk ()
  "pickup everything at current location, then walk somewhere"  
  (push *location* *prev_nodes*)
  (let ((directions (mapcar #'cadr (cdr (assoc *location* *edges*)))))
    (dolist (object (objects-at *location* *objects* *object-locations*))
      (pickup object))
    (walk (any directions))))

(defun rwalk-worker (&optional (rooms 20) (f1 1) (f2 5) (steps 100))
  (reset-seed)
  (landscape 20 1 5)
  (format t "nodes: ~a~%~%edges: ~a~%~%objects: ~a~%~%at: ~a~%~%"
	  *nodes* *edges* *objects* *object-locations*)
  (dotimes (i steps)
    (rwalk))
  (inventory))

;; TESTS ;;
#|
(deftest !test_create_nodes()
  (load "wizards_game.lisp")
  (reset-seed)
  (make_nodes 10)
  (test 13
	(length *nodes*)))

(deftest !test_create_objects ()
  (load "wizards_game.lisp")
  (reset-seed)
  (make_nodes 10)
  (make_objs 10)
  (test 14
	(length *objects*))
  (test 14
	(length *object-locations*)))

(deftest !test_init_edges ()
  (load "wizards_game.lisp")
  (reset-seed)
  (make_nodes 10)
  (init_edges)
  (test 13
	(length *edges*)))

(deftest !test_create_edges ()
  (load "wizards_game.lisp")
  (reset-seed)
  (make_nodes 10)
  (make_edges 1 3)
  (labels ((neighbors (node)
	     (length (rest (assoc (car node) *edges*))))
	   (within_bounds (val)
	     (and (<= val 3) (>= val 1))))
    ; test that no node has a number of neighbors outside the bounds
    (test nil
	  (member nil (mapcar #'within_bounds 
			      (mapcar #'neighbors *nodes*))))))

(deftest !test_rwalk ()
  (load "wizards_game.lisp")
  (reset-seed)
  (rwalk)
  (test '(whiskey bucket)
	(cdr (inventory))))

(deftest !test_rwalk_worker ()
  (load "wizards_game.lisp")
  (reset-seed)
  ; test length of inventory after rwalk_worker
  (test (length '(ITEMS- #:|sword2632| #:|sword2631| #:|sword2630| 
	  #:|sword2629| #:|sword2628| #:|sword2627| #:|sword2626| 
	  #:|sword2625| #:|sword2624| #:|sword2623| #:|sword2622| 
	  #:|sword2621| #:|sword2620| #:|sword2619| #:|sword2617|
	  #:|sword2616| #:|sword2615| #:|sword2614| #:|sword2613| 
	  WHISKEY BUCKET FROG CHAIN))
	(length (rwalk-worker)))
  ; test that all expected items are found in inventory
  (test nil 
	(set-difference (gathered_objs) (cdr (inventory))))
  ; test that no additional items are found in inventory
  (test nil
	(set-difference (cdr (inventory)) (gathered_objs))))
|#