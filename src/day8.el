(defun count-chars (cs)
  (if (null cs) 0
    (case (car cs)
      (?\" (count-chars (cdr cs)))
      (?\\ (handle-backslash (cdr cs)))
      (t (1+ (count-chars (cdr cs)))))))

(defun handle-backslash (cs)
  "CS contains the rest of the list after a backslash"
  (case (car cs)
    (?\\ (1+ (count-chars (cdr cs))))
    (?\" (1+ (count-chars (cdr cs))))
    (?x (1+ (count-chars (cdddr cs))))))

(defun mem-string-diff (s)
  (let* ((cs (string-to-list s))
	 (str-len (count-chars cs))
	 (mem-len (length cs)))
    (- mem-len str-len)))

(defun encode-char (c)
  (case c
    (?\" (list ?\\ ?\"))
    (?\\ (list ?\\ ?\\))
    (t c)))

(defun encode-string (s)
  (let ((cs (string-to-list s)))
    (apply 'string (flatten-list (append (cons ?\" (mapcar 'encode-char cs))
					 (list ?\"))))))

(defun encode-diff (s)
  (- (length (encode-string s))
     (length s)))

(defun char-encode-len (c)
  (case c
    (?\" 2)
    (?\\ 2)
    (t 1)))

(defun string-encode-len (s)
  (let ((cs (string-to-list s)))
    (+ 2 (reduce '+ (mapcar 'char-encode-len cs)))))

(defun encode-len-diff (s)
  (- (string-encode-len s)
     (length s)))

(provide 'day8)
