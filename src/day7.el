(defconst gates '(:and :or :lshift :rshift :not))
(defconst bit-width #xFFFF)

(defun parse-joint (line)
  (destructuring-bind (sig wire) (split-string line "\\(?:->\\)")
    (cons (parse-gate (s-trim sig)) (parse-wire (s-trim wire)))))

(defun parse-wire (wire)
  (let ((first-char (car (string-to-list (substring-no-properties wire 0 1)))))
    (if (and (>= first-char ?a) (<= first-char ?z))
	(list :wire wire)
      (list :const (string-to-number wire)))))

(defun parse-gate (s)
  (let ((terms (split-string s)))
    (case (length terms)
      (1 (parse-wire (car terms)))
      (2 (if (string= "NOT" (car terms))
	     (list :not (parse-wire (cadr terms)))
	   (error "Signal with arity 2 is not a NOT gate")))
      (3 (list (gate-from-string (cadr terms))
	       (parse-wire (car terms))
	       (parse-wire (caddr terms))))
      (t (error "Un-recogized arity of the gates %n" (length terms))))))

;; Is there a function that creates keywords from string?
(defun gate-from-string (gate-string)
  (cond ((string= gate-string "AND") :and)
	((string= gate-string "OR") :or)
	((string= gate-string "LSHIFT") :lshift)
	((string= gate-string "RSHIFT") :rshift)	))

(defun create-network (gate-connections)
  (let ((network (make-hash-table :test 'equal
				  :size 330)))
    (dolist (c gate-connections network)
      (puthash (caddr c) (car c) network))))

(defun rshift (value count)
  (ash value (- count)))

(defun get-val (wire network)
  (case (car wire)
    (:const (cadr wire))
    (:wire 
     (let ((gate (gethash (cadr wire) network)))
       (case (car gate)
	 (:const (cadr gate))
	 (:wire (get-val gate network))
	 (:not (logand bit-width (lognot (get-val (cadr gate) network))))
	 (:and (logand (get-val (cadr gate) network)
		       (get-val (caddr gate) network)))
	 (:or (logior (get-val (cadr gate) network)
		      (get-val (caddr gate) network)))
	 (:lshift (logand bit-width
			  (ash (get-val (cadr gate) network)
			       (get-val (caddr gate) network))))
	 (:rshift (rshift (get-val (cadr gate) network)
			  (get-val (caddr gate) network)))
	 (t (error "Unknown gate")))))))

(provide 'day7)
