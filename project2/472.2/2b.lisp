;; POSSIBLE DIRECTION & VALUES ;; 
(defparameter *dirs* '(north south east west))
(defparameter *dir_valsy* '((north 3)
			    (south -3)
			    (east 0)
			    (west 0)))
(defparameter *dir_valsx* '((east 3)
			    (west -3)
			    (north 0)
			    (south 0)))

;; VARIOUS STRUCTURES USED IN CREATING EDGES ;;
(defparameter *node_pos* '())
(defparameter *used_nodes* '())
(defparameter *nodes* '())
(defparameter *edges* '())
(defparameter *objects* '())
(defparameter *object-locations* '())

(defun redo ()
  (setf *node_pos* '())
  (setf *used_nodes* '())
  (setf *nodes* '())
  (setf *edges* '())
  (setf *objects* '())
  (setf *object-locations* '())
  (setf *dfs_visited* '())
  (setf *smart_visited* '()))

(defun get_dirs (node)
  "get list of current directions to exit node"
  (mapcar #'cadr (cdr (assoc node *edges*))))

(defun rem_dirs (node)
  "get list of remaining directions to exit node"
  (set-difference *dirs* (get_dirs node)))

(defun opp_dir (dir)
  "get opposite direction to dir"
  (case dir
    ('east 'west)
    ('west 'east)
    ('north 'south)
    ('south 'north)))

(defun avail_nodes (max)
  "get list of all nodes who can still be added to"
  (mapcar #'car
	  (remove-if-not #'(lambda (x)
			     (< (length (cdr x))
			     max))
			 *edges*)))

(defun compat_nodes (node max)
  "get list of available nodes who have compatible directions
   to node"
  (remove-if #'(lambda (x) 
		 (or (null (intersection 
			    (mapcar #'opp_dir (rem_dirs node))
			    (rem_dirs x)))
		     (equal node x)
		     (member x *used_nodes*)
		     (member x (neighbors node))))
	     (avail_nodes max)))



(defun get_common_dir (from_node to_node)
  "get list of opposite dirs to connect node1 & node2"
  (let ((dir (any (intersection (mapcar #'opp_dir (rem_dirs from_node))
				(rem_dirs to_node)))))
    (list (opp_dir dir) dir)))

(defun get_specific_dir (from_node to_node)
  "returns list of dirs connecting two nodes already on grid"
  (let ((x1 (cadr (assoc from_node *node_pos*)))
	(y1 (caddr (assoc from_node *node_pos*)))
	(x2 (cadr (assoc to_node *node_pos*)))
	(y2 (caddr (assoc to_node *node_pos*)))
	to_return)
    (setf to_return
	  (if (equal x1 x2)
	      (if (< y1 y2)
		  '(north south)
		  '(south north))
	      (if (equal y1 y2)
		  (if (< x1 x2)
		      '(east west)
		      '(west east)))))))

(defun neighbors (node)
  "get list of neighbors to node"
  (mapcar #'car (cdr (assoc node *edges*))))

(defun node-at (x y)
  "get node at position x y"
  (caar (remove-if-not #'(lambda (node) (equal `(,x ,y)
						       (rest node)))
			       *node_pos*)))

(defun edge_between (node1 node2)
  "return the edge from node1 to node 2"
  (assoc node2 (rest (assoc node1 *edges*))))

(defun update_pos (node)
  "update node with adjacent nodes"
  (let* ((x (cadr (assoc node *node_pos*)))
	 (y (caddr (assoc node *node_pos*)))
	 (adj_nodes (remove nil
			    (list (node-at (+ x 3) y)
				  (node-at (- x 3) y)
				  (node-at x (+ y 3))
				  (node-at x (- y 3))))))
    (dolist (temp_node (set-difference adj_nodes (neighbors node)))
     ; (if (not (edge_between node temp_node))
      (two_way node temp_node (get_specific_dir node temp_node)))))

(defun set_pos (node x y)
  "sets position of node to x,y"
  (setf (rest (assoc node *node_pos*))
	(list x y)))
    
(defun get_pos (node)
  "gets x,y position of node"
  (list (cadr (assoc node *node_pos*))
	(caddr (assoc node *node_pos*))))

(defun two_way (sym1 sym2 dirs)
  "insert a bi-directional edge between sym1 & sym2"
   (if (not (member nil dirs))
       (and (push (list sym2 (car dirs)) 
		  (rest (assoc sym1 *edges*)))
	    (push (list sym1 (cadr dirs)) 
		  (rest (assoc sym2 *edges*)))
	    (if (member nil (cdr (assoc sym2 *node_pos*)))
		(setf (rest (assoc sym2 *node_pos*))
		      (mapcar #'+ 
			      (list (cadr (assoc (car dirs) *dir_valsx*))
				    (cadr (assoc (car dirs) *dir_valsy*)))
			      (cdr (assoc sym1 *node_pos*))))))))

(defun distance_between (node1 node2)
  "returns straight-line distance from node1 to node2"
  (let ((node1_pos (get_pos node1))
	(node2_pos (get_pos node2)))
    (sqrt (+ (expt (- (car node2_pos) (car node1_pos)) 2)
	     (expt (- (cadr node2_pos) (cadr node1_pos)) 2)))))

(defun add_wizard()
  "inserts wizard at random location"
  (push 'wizard *objects*)
  (push (list 'wizard (car (any *nodes*)))
	*object-locations*))

(defun wizard_loc()
  "returns location of wizard"
  (cadr (assoc 'wizard *object-locations*)))

(defun make_edges2 (f1 f2)
  "create f1 to f2 edges for nodes"
  (init_edges)
  (labels
      (;return number of neighbors to node 
       (neighbor_count (sym) (length (rest (assoc sym *edges*)))))
    (let (; list containing nodes to be parsed
	  (node_list (list (car (any *nodes*)))))
      ; set initial node to 0,0
      (set_pos (car node_list) 0 0)
      (loop do
	   (let* ((node (pop node_list)) ;current node to parse
		  (num_edges (- (round (within f1 f2)) 
				(neighbor_count node))))
	     (dotimes (x (max f1 num_edges)) ; add edges	  
	       ; find compatible node to connect with
	       (let ((temp_node (any (compat_nodes node f2))))
		 ; check that node was found
		 (if (not (null temp_node))
		     ; add edges to both nodes
		     (and (push temp_node node_list) 
			  (push temp_node *used_nodes*)
			  (two_way node
				   temp_node
				   (get_common_dir node temp_node))
			  ; update latest node
			  (update_pos temp_node)))))
	     (remove node node_list))
	 while (not (null node_list))))))

;; SEARCH-UTILS ;;
(defun tree-search (states goal-p successors combiner)
  (blab :search "~&;; Search: ~a" states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) 
	 ;(format t "~&Found the wizard at ~a!~%~%" (first states))
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

;; DUMB-SEARCH ;;
(defparameter *dfs_visited* '())

(defun dumb_combine_new (node)
  (let ((new_nodes (set-difference (neighbors node)
				   *dfs_visited*)))
    (dolist (x (neighbors node))
      (pushnew x *dfs_visited*))
    new_nodes))

(defun dumb_success (node)
  (equal node (wizard_loc)))

(defun depth-first-search (start goal-p successors)
  (tree-search (list start)  ; initially, our states are just "start"
	       goal-p        ; a function to recognize "success"
	       successors    ; a generator of states next to current
	       #'append      ; dfs= explore new before old
	       ))

(defun breadth-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors #'prepend))

(defun try_dfs ()
  (let ((start (node-at 0 0)))
    (setf *dfs_visited* '())
    (push start *dfs_visited*)
    (depth-first-search start
			#'dumb_success
			#'dumb_combine_new)))

(defun try_bfs ()
  (let ((start (node-at 0 0)))
    (setf *dfs_visited* '())
    (push start *dfs_visited*)
    (breadth-first-search start
			  #'dumb_success
			  #'dumb_combine_new)))

;; SMART SEARCH ;;
(defparameter *smart_visited* '())

(defun smart_combine_new (node)
  (let ((new_nodes (set-difference 
		    (mapcar #'(lambda (x) 
				(list x (distance_between x
							  (wizard_loc))))
			    (neighbors (car node)))
		    *smart_visited* 
		    :key #'car)))
    (dolist (x (neighbors (car node)))
      (pushnew (list x
		     (distance_between x (wizard_loc)))
	       *smart_visited*
	       :key #'car))
    new_nodes))

(defun smart_success (node)
  (equal (car node) (wizard_loc)))

(defun dist_from_wiz ()
  #'(lambda (new old)
      (sort (append new old) #'< :key #'cadr)))

(defun beam_dist_from_wiz (beam_width)
  #'(lambda (old new)
      (let ((sorted (funcall (dist_from_wiz) old new)))
	(if (> beam_width (length sorted))
	    sorted
	    (subseq sorted 0 beam_width)))))

(defun best-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors (dist_from_wiz)))

(defun beam-search (start goal-p successors beam-width)
  (tree-search (list start) goal-p successors
	       (beam_dist_from_wiz beam-width)))

(defun try_best ()
  (let* ((start (node-at 0 0))
	 (start-l (list start
			(distance_between start (wizard_loc)))))
    (setf *smart_visited* '())
    (push start-l *smart_visited*)
    (best-first-search start-l
		       #'smart_success
		       #'smart_combine_new)))

(defun try_beam (beam_width)
  (let* ((start (node-at 0 0))
	 (start-l (list start
			(distance_between start (wizard_loc)))))
    (setf *smart_visited* '())
    (push start-l *smart_visited*)
    (beam-search start-l
		 #'smart_success
		 #'smart_combine_new
		 beam_width)))

;; TESTS ;;
(deftest !test_get_common_dir ()
  (setf *nodes* '()
	*edges* '())
  (push (list 'a '(you are in a)) *nodes*)
  (push (list 'b '(you are in b)) *nodes*)
  (push (list 'a '(x north) '(y south) '(z west)) *edges*)
  (push (list 'b '(x north) '(y south) '(z east)) *edges*)
  (test '(east west)
	(get_common_dir 'a 'b)))

(deftest !setup_tests ()
  (!)
  (landscape 99 1 4))

(deftest !test_dfs ()
  (test (wizard_loc)
	(try_dfs)))

(deftest !test_bfs ()
  (test (wizard_loc)
	(try_bfs)))

(deftest !test_best ()
  (test (wizard_loc)
	(car (try_best))))

(deftest !test_beam ()
  (test (wizard_loc)
	(car (try_beam 3))))

