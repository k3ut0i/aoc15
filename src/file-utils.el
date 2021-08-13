(defun line-pos ()
  (values (line-beginning-position) (line-end-position)))

;; With some help from the good folks at #emacs
(defun read-lines (filename)
  (let ((buf (find-file-noselect filename))
	(lines))
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (multiple-value-bind (begin end) (line-pos)
	    (push (buffer-substring-no-properties begin end) lines)
	    (forward-line)))))
    (reverse lines)))

(defun read-file (filename)
  "return the whole file FILENAME as a string"
  (let ((buf (find-file-noselect filename)))
    (with-current-buffer buf
      (save-excursion
	(buffer-substring-no-properties (point-min) (point-max))))))
