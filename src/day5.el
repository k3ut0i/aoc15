(defconst vowels '(?a ?e ?i ?o ?u))

(defun has-n-vowels (n s)
  (let ((cs (string-to-list s)))
    (do ((ls cs (cdr ls))
	 (count 0))
	((or (null ls) (= count n)) (= count n))
      (if (member (car ls) vowels) (incf count)))))

(defun has-3-vowels (s) (has-n-vowels 3 s))

(defun has-rep (s)
  (let ((cs (string-to-list s)))
    (do ((ls (cdr cs) (cdr ls))
	 (c (car cs)))
	((or (null ls) (= c (car ls))) (consp ls)) ; ls is non nil if it has some rep
      (setf c (car ls)))))

(defun no-triv (s)
  (let ((r (regexp-opt '("ab" "cd" "pq" "xy"))))
    (not (string-match r s))))

(defun nice (s)
  (every (lambda (p)
	   (funcall p s))
	 (list 'has-3-vowels 'has-rep 'no-triv)))
;; Part 1
(defun two-pair-helper (cs)
  (do ((ls (cddr cs) (cdr ls))
       (a (car cs))
       (b (cadr cs)))
      ((or (null ls) (null (cdr ls)) (and (= a (car ls)) (= b (cadr ls))))
       ;; TODO: Needs good return value
       (unless (null (cdr ls)) (and (= a (car ls)) (= b (cadr ls)))))))

;; TODO: debug xxyxx case. What is going on if the last two chars of a word match?
(defun two-pair (s)
  (let ((cs (string-to-list s)))
    (do ((ls cs (cdr ls)))
	((or (null ls) (two-pair-helper ls))
	 (two-pair-helper ls)))))

(defun aba-pattern (s)
  (let ((cs (string-to-list s)))
    (do ((ls cs (cdr ls)))
	((or (null (cdddr ls)) (= (car ls) (caddr ls)))
	 (= (car ls) (caddr ls))))))

(defun nice2 (s)
  (and (two-pair s) (aba-pattern s)))
(provide 'day5)
