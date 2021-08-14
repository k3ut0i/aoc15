(defun prefix-zeros? (str n)
  "Does md5sum of STR has N zeros as its prefix?"
  (string-equal (substring-no-properties (secure-hash 'md5 str) 0 n)
		(make-string n ?0)))
(defun find-min (key nzeros)
  (do ((i 0 (+ i 1)))
      ((prefix-zeros? (format "%s%d" key i) nzeros) i)))

(provide 'day4)
