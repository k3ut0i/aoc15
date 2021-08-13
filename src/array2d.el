(defun make-array2d (xsize ysize &optional val)
  "Create a two-dimensional array implemented as a row-major vector"
  (assert (> xsize 0))
  (assert (> ysize 0))
  (let ((size (* xsize ysize)))
    (record 'array2d (make-vector size val) xsize ysize)))

(defun aref2d (array x y)
  (let ((vec (aref array 1))
	(xsize (aref array 2))
	(ysize (aref array 3)))
    (aref vec (+ x (* y ysize)))))

(defun setf-aref2d (array x y val)
  (let ((vec (aref array 1))
	(xsize (aref array 2))
	(ysize (aref array 3)))
    (assert (< x xsize))
    (assert (< y ysize))
    (setf (aref vec (+ x (* y ysize))) val)
    array))

(defun array2d-size (array)
  (values (aref array 2) (aref array 3)))

(provide 'array2d)
