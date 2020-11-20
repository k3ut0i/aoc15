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
	((string= gate-string :RSHIFT) :rshift)	))

(defun run-gate (gate)
  (case (car gate)
    (:const (cadr gate))
    (:wire (run-gate (cadr gate))) ;; Recursive Here
    (:not (logand bit-width (lognot (cadr gate))))
    (:and (logand (cadr gate) (caddr gate)))
    (:or (logior (cadr gate) (caddr gate)))
    (:lshift (logand bit-width (ash (cadr gate) (caddr gate))))
    (:rshift (ash (- (cadr gate)) (caddr gate)))))

(provide 'day7)
