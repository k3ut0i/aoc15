(defun get-dim (s)
  (mapcar 'string-to-number (split-string s "x")))

(defun req-area (ls)
  (destructuring-bind (a b c) ls
    (let* ((x (* a b))
	   (y (* b c))
	   (z (* c a))
	   (area (+ (* 2 x) (* 2 y) (* 2 z)))
	   (extra (min x y z)))
      (+ area extra))))


(defun total (dim-symbol-list measure-fn)
  (reduce '+ (mapcar (lambda (s)
		       (funcall measure-fn (get-dim (symbol-name s))))
		     dim-symbol-list)))

(defun ribbon-length (ls)
  (destructuring-bind (a b c) ls
    (let* ((x (+ a b))
	    (y (+ b c))
	    (z (+ c a))
	    (wrap (* 2 (min x y z)))
	    (bow (* a b c)))
	 (+ wrap bow))))
