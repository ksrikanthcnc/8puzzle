#||
(setf *goal-state* '(	1 2 3
						8 - 4
						7 6 5))
(setf *initial-state* '(1 2 3
						4 5 6
						- 8 7 ))
||#
(setf *goal-state* '(	1 2 3
						4 5 6
						7 8 -))
(setf *initial-state* '(1 2 3
						4 5 6
						8 7 - ))
(defstruct problem
	(initial-node)
	(goal-test)
	(operators))

(defstruct node
	(state)
	(goal-dist)
	(travel-dist)
	(dist)
	(parent))

(defun travel-dist (node)
	;(print "Travel-dist");
	;(print node)
	(setq dist (+ 	(node-goal-dist node)
					(node-travel-dist node)))
	(return-from travel-dist (node-travel-dist node)))

(defun goal-dist (node)
	;(print "Goal-dist");
	;(print node)
	(setq dist (+ 	(node-goal-dist node)
					(node-travel-dist node)))
	(return-from goal-dist (node-goal-dist node)))

(defun 8puzz-goal-dist (state &aux (dist 0) (correct-tile 1))
	;(print "Finding goal-dist")
	(loop for tile in state
		do
			(progn
				(if (not (eq tile (nth 	(- correct-tile 1) *goal-state*)))
					(incf dist))
				(incf correct-tile)))
	;(print "Found Goal-dist")
	dist)

(defun 8puzz-goal-test (state)
	(equal state *goal-state*))

(defun move-blank-right (node &aux pos)
	(setf pos (index '- (node-state node)))
	(if (not (or (eq pos 3) (eq pos 6) (eq pos 9)))
		(progn
			;(print "Moving tile right")
			(setf state (swap-tiles (node-state node) pos (+ pos 1)))
			(setf new (make-node 	:state state
									:parent (cons (node-state node) (node-parent node))
									:travel-dist (+ (node-travel-dist node) 1)
									:goal-dist (8puzz-goal-dist state)))
			;(print "Moved tile right")
			new)))

(defun move-blank-up (node &aux pos)
	(setf pos (index '- (node-state node)))
	(if (> pos 3)
		(progn
			;(print "Moving tile up")
			(setf state (swap-tiles (node-state node) pos (- pos 3)))
			(setf new (make-node 	:state state
									:parent (cons (node-state node) (node-parent node))
									:travel-dist (+ (node-travel-dist node) 1)
									:goal-dist (8puzz-goal-dist state)))
			;(print "Moved tile up")
			new)))
			
(defun move-blank-down (node &aux pos)
	(setf pos (index '- (node-state node)))
	(if (< pos 7)
		(progn
			;(print "Moving tile down")
			(setf state (swap-tiles (node-state node) pos (+ pos 3)))
			(setf new (make-node 	:state state
									:parent (cons (node-state node) (node-parent node))
									:travel-dist (+ (node-travel-dist node) 1)
									:goal-dist (8puzz-goal-dist state)))
			;(print "Moved tile down")
			new)))

(defun move-blank-left (node &aux pos)
	(setf pos (index '- (node-state node)))
	(if (not (or (eq pos 1) (eq pos 4) (eq pos 7)))
		(progn
			;(print "Moving tile left")
			(setf state (swap-tiles (node-state node) pos (- pos 1)))
			(setf new (make-node 	:state state
									:parent (cons (node-state node) (node-parent node))
									:travel-dist (+ (node-travel-dist node))
									:goal-dist (8puzz-goal-dist state)))
			;(print "Moved tile left")
			new)))

(defvar 8puzz-problem nil)

(setf 8puzz-problem
	(make-problem
		:initial-node (make-node	:state	*initial-state*
									:parent	(list *initial-state*)
									:travel-dist 0
									:goal-dist (8puzz-goal-dist *initial-state*))
		:goal-test #'8puzz-goal-test
		:operators (list #'move-blank-up #'move-blank-down #'move-blank-left #'move-blank-right)))
#||		
(setf 	(node-goal-dist (problem-initial-node 8puzz-problem))
		(8puzz-goal-dist (problem-initial-node 8puzz-problem)))
||#
(defun test8-dfs ()
	;(print "DFS")
	(General-Search 8puzz-problem #'DFS-queue))
(defun test8-bfs ()
	;(print "BFS")
	(General-Search 8puzz-problem #'BFS-queue))
(defun test8-a* ()
	;(print "A*")
	(General-Search 8puzz-problem #'A*-queue))

(defun General-Search (problem algo &aux nodes (visited nil))
	;(print "General-Search")
	(setf 	nodes
			(list (problem-initial-node problem)))
	(loop do
		(progn
			;(print "Loop")
			(if (null nodes)
				(progn
					;(print "Failed")
					(return-from General-Search 'failure)))

			(setf node (car nodes))
			(if (null node)
				(progn
					(setf node (cadr nodes))
					(setf nodes (cdr nodes)))
				(setf nodes (cdr nodes)))
;			(setq node null)
			(loop 
				while (null node)
					do	(if (null node)
							(progn
								(setf node (cadr nodes))
								(setf nodes (cdr nodes)))
							(setf nodes (cdr nodes))))
			(when 	(not (member (node-state node) visited :test #'equal))
				(format t "Testing (~s nodes visited, ~s nodes open)~%~a ~a ~a ~%~a ~a ~a ~%~a ~a ~a ~%Distance from Goal:~a~%Depth travelled so far:~a~%h(n):~a~%"
					(length visited) (length nodes) 
					(nth 0 (node-state node)) 
					(nth 1 (node-state node))
					(nth 2 (node-state node))
					(nth 3 (node-state node))
					(nth 4 (node-state node))
					(nth 5 (node-state node))
					(nth 6 (node-state node))
					(nth 7 (node-state node))
					(nth 8 (node-state node))
					(node-goal-dist node)
					(node-travel-dist node)
					(+	(node-goal-dist node)
						(node-travel-dist node)))
				(setq temp (node-travel-dist node))
				(loop
					while 	(> temp 0)
					do
							(progn
								(format t ".")
								(decf temp)))
				(terpri)
				(setq temp (+	(node-goal-dist node)
								(node-travel-dist node)))
				(loop
					while 	(> temp 0)
					do
							(progn
								(format t "-")
								(decf temp)))
				(terpri)
;					(p node);---------------
				(terpri)
				(terpri)
				(terpri)
				(if (= (node-goal-dist node) 0)
					(progn
						(answer node)
						(return-from General-Search (values node)))
				(progn
					;(print "Entering...")
					(push (node-state node) visited)
					(setf 	nodes
							(funcall algo	nodes
											(Expand node
													(problem-operators problem)
													visited)))
					;(print "Exiting...")
					))))))
													

(defun Expand (node operators visited)
	;(print "Expanding")
	(remove-if #'(lambda(n) (member n visited :test #'equal))
		(progn
			(apply #'nconc (list (mapcar #'(lambda (op) (funcall op node)) operators))))))

(defun DFS-queue (nodes new-nodes)
	;(print "DFS-Q")
	(nconc new-nodes nodes))

(defun BFS-queue (nodes new-nodes)
	;(print "BFS-Q")
	(if (null nodes)
		(progn
			(return-from BFS-queue new-nodes)))
	(nconc nodes new-nodes))

(defun A*-queue (nodes new-nodes)
	;(print "A*-Q")
	(loop for new in new-nodes 
		do (if 	(not (null new))
				(setf nodes (insert-node-ordered new nodes))))
	nodes)

(defun p (node)
	;(print "p")
	;(terpri)
	;(print "Printing Parent")
;	(setf node (node-parent node))
	(format t "~a ~a ~a ~%~a ~a ~a ~%~a ~a ~a ~%"
		(nth 0 (node-state node)) 
		(nth 1 (node-state node))
		(nth 2 (node-state node))
		(nth 3 (node-state node))
		(nth 4 (node-state node))
		(nth 5 (node-state node))
		(nth 6 (node-state node))
		(nth 7 (node-state node))
		(nth 8 (node-state node)))
	;(print "end-p")
	)

(defun print-state (node)
	;(print "print-state")
	;(terpri)
	;(print "Printing Parent")
;	(setf node (node-parent node))
	(format t "~a ~a ~a ~%~a ~a ~a ~%~a ~a ~a ~%"
		(nth 0 node) 
		(nth 1 node)
		(nth 2 node)
		(nth 3 node)
		(nth 4 node)
		(nth 5 node)
		(nth 6 node)
		(nth 7 node)
		(nth 8 node))
	;(print "end-print-state")
	)

(defun insert-node-ordered (new nodes)
	;(print "Inserting-node")
	(setf nod (car nodes))
	(cond	((null nodes)
				(list new))
			((< (+ 	(travel-dist node)
					(goal-dist node))
				(+ 	(travel-dist nod)
					(goal-dist nod)))
				(cons new nodes))
			(t
				(cons (car nodes) (insert-node-ordered new (cdr nodes))))))


(defun index (element state &optional (pos 1))
	;(print "Fetching index")
	(if (null state)
		(error "Error in index~%"))
	(if (eq element (car state))
		pos
		(index element (cdr state) (incf pos))))
	
(defun swap-tiles (state pos1 pos2 &aux temp state2)
	;(print "Swapping tiles")
	(setf state2 (copy-tree state))
	(decf pos1)
	(decf pos2)
	(setf temp (nth pos1 state2))
	(setf (nth pos1 state2) (nth pos2 state2))
	(setf (nth pos2 state2) temp)
	state2)


(defun answer (node)
	(print "answer")
	(print "Start-State")
	(terpri)
;	(print node)
	(par (node-parent node))
	(print "Goal-state")
	(terpri)
	(p node))

(defun par (par-node-list)
	;(print "Par")
	;(print par-node-list)
	(if (null (cdr par-node-list))
		(progn
			;(print-state (car par-node-list))
			;(terpri)
			(return-from par)))
	(par (cdr par-node-list))
	(print-state (car par-node-list))
	(terpri)
	)


;		(test8-bfs)
;		(test8-dfs)
		(test8-a*)
	

























