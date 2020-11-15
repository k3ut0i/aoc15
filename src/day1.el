(defconst parc '((?\( . 1)
		 (?\) . -1)))

(defun count-par (par-list)
  "Left paranthesis minus right paranthesis in PAR-LIST."
  (reduce #'+ (mapcar (lambda (c)
			(cdr (assoc c parc)))
		      par-list)))

(defun read-line (filename)
  (let ((buf (find-file filename))
	(chars nil))
    (setq start (point-min)
	  end (point-max))
    (do ((p start (forward-char)))
	((eq end (point)))
      (push (following-char) chars))
    chars))
