;;; -*- lexical-binding : t -*-
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
    (read-only-mode)
    (setq start (point-min)
	  end (point-max))
    (do ((p (goto-char start) (forward-char)))
	((eq end (point)))
      (push (following-char) chars))
    (set-buffer old-buf)
    chars))

(defun find-first (dir-list)
  "Given the list of paranthesis in DIR-LIST return the step when he reaches -1"
  (do ((l dir-list (cdr l))
       (n 0 (1+ n))
       (floor 0))
      ((< floor 0) n)
    (incf floor (cdr (assoc (car l) parc)))))

(defun day1/find-basement-pos (filename)
  (let ((buf (find-file-noselect filename)))
    (with-current-buffer buf
      (read-only-mode)
      (do ((p (goto-char (point-min)) (forward-char))
	   (floor 0))
	  ((= floor -1) (prog2
			    (backward-char) ;; step is executed before comparing
			    (point))) ;; so we back once before returning
	(incf floor (if (= (following-char) ?\() 1 -1))))))
