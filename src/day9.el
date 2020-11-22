(defun parse-distance (s)
  (destructuring-bind (p1 to-literal p2 eq-literal distance)
      (split-string s)
    (assert (string= to-literal "to"))
    (assert (string= eq-literal "="))
    (list (intern (concat ":" (downcase  p1)))
	  (intern (concat ":" (downcase p2)))
	  (string-to-number distance))))

(defun add-edge (a b distance network)
  (let ((edges-a (gethash a network nil))
	(edges-b (gethash b network nil)))
    (puthash a (cons (cons b distance) edges-a) network)
    (puthash b (cons (cons a distance) edges-b) network)
    network))

;; Network is just adjacency list representation of the graph
(defun create-network (distances)
  "Create a route network from the list of DISTANCES provided."
  (let ((nw (make-hash-table :test 'eq)))
    (mapc (lambda (edge-desc)
	    (destructuring-bind (a b distance) edge-desc
	      (add-edge a b distance nw)))
	  distances)
    nw))

(defun shortest-trail-from (from network)
  "Return the shortest trail starting FROM in the NETWORK."
  )

(defun shortest-trail (network)
  "Find the shortest path that covers all the elements of NETWORK.")
(provide 'day9)
