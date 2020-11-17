(defconst moves '((?^ . north)
		  (?v . south)
		  (?< . west)
		  (?> . east)))

;; In Standard 2d co-ordinate system
(defun mv-diff (mv)
  (case mv
    (?^ '(0 . 1))
    (?v '(0 . -1))
    (?> '(1 . 0))
    (?< '(-1 . 0))))

(defun add-pos (P1 P2)
  (cons (+ (car P1) (car P2)) (+ (cdr P1) (cdr P2))))

(defun eq-pos (P1 P2)
  (and (= (car P1) (car P2))
       (= (cdr P1) (cdr P2))))

(defun get-trail (mv-list)
  (seq-reduce (lambda (trail mv)
		(cons (add-pos (car trail) (mv-diff mv)) trail))
	      mv-list
	      '((0 . 0))))

(defun visited (mv-list)
  (remove-duplicates (get-trail mv-list)
		     :test 'eq-pos))

(defun unzip (in)
  (let (fst snd)
    (do ((ls in (cdr (cdr ls))))
	((or (null ls) (null (cdr ls)))
	 (prog2 (if (not (null ls)) (push (car ls) fst))
	     (values (reverse fst) (reverse snd))))
      (push (car ls) fst)
      (push (cadr ls) snd))))
