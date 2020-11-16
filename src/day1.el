(defconst parc '((?\( . 1)
		 (?\) . -1)))

(defun count-par (par-list)
  "Left paranthesis minus right paranthesis in PAR-LIST."
  (reduce #'+ (mapcar (lambda (c)
			(cdr (assoc c parc)))
		      par-list)))

(defun read-line (filename)
  (let ((old-buf (current-buffer))
	(buf (find-file-noselect filename))
	(chars nil))
    (set-buffer buf)
    (setq start (point-min)
	  end (point-max))
    (do ((p (goto-char start) (forward-char)))
	((eq end (point)))
      (push (following-char) chars))
    (set-buffer old-buf)
    chars))

(defun find-first (dir-list)
  "Given the list of paranthesis in DIR-LIST return the step when he reaches -1"
  (let ((floor 0))
    (do ((l dir-list (cdr l))
	 (n 0 (1+ n)))
	((or (null l) (= floor -1)) (cons n floor))
      (setf floor (+ floor (cdr (assoc (car l) parc)))))))
