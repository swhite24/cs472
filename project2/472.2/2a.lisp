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
