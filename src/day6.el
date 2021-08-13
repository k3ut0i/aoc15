;;; -*- lexical-binding : t -*-
(require 'array2d)

(defun parse-positions (ls)
  (destructuring-bind (x1 y1 through x2 y2) ls
    (assert (string= through "through"))
    (mapcar 'string-to-number (list x1 y1 x2 y2))))

(defun parse-order (s)
  (let ((words (s-split-words s)))
    (cond ((string= (car words) "turn")
	   (cond ((string= (cadr words) "on")
		  (cons :on (parse-positions (cddr words))))
		 ((string= (cadr words) "off")
		  (cons :off (parse-positions (cddr words))))))
	  ((string= (car words) "toggle")
	   (cons :toggle (parse-positions (cdr words)))))))

(defun op1 (order)
  (case (car order)
    (:on (lambda (a) 1))
    (:off (lambda (a) 0))
    (:toggle (lambda (a) (- 1 a)))))

(defun op2 (order)
  (case (car order)
    (:on (lambda (a) (+ a 1)))
    (:off (lambda (a) (if (<= (- a 1) 0) 0 (- a 1))))
    (:toggle (lambda (a) (+ a 2)))))

(defun step (array order op)
  (destructuring-bind (o x1 y1 x2 y2) order
    (let ((order-fn (funcall op order)))
      (do ((x x1 (1+ x)))
	  ((> x x2) array)
	(do ((y y1 (1+ y)))
	    ((> y y2))
	  (setf-aref2d array x y
		       (funcall order-fn
				(aref2d array x y))))))))

(defun lit-lights (array)
  (count-if (lambda (a) (= a 1)) (aref array 1)))

(defun step-orders-1 (ls array)
  (seq-reduce (lambda (a o)
		(step a o 'op1))
	      ls
	      array))

(defun step-orders-2 (ls array)
  (seq-reduce (lambda (a o)
		(step a o 'op2))
	      ls
	      array))

(defun plot-array (array buf)
  (write-char ?\n buf)
  (multiple-value-bind (xsize ysize) (array2d-size array)
    (do ((x 0 (1+ x)))
	((= x xsize))
      (do ((y 0 (1+ y)))
	  ((= y ysize))
	(if (= (aref2d array x y) 0)
	    (write-char ?. buf)
	  (write-char ?* buf)))
      (write-char ?\n buf))
    array))

(provide 'day6)
