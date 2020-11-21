(require 'test-simple)
(test-simple-start)
(assert-t (load-file "src/day6.el") "load src file")
(assert-t (equal (parse-order "turn on 0,0 through 999,999")
		 '(:on 0 0 999 999))
	  "turn on")
(assert-t (equal (parse-order "toggle 0,0 through 999,0")
		 '(:toggle 0 0 999 0))
	  "toggle")
(assert-t (equal (parse-order "turn off 499,499 through 500,500")
		 '(:off 499 499 500 500))
	  "turn off")

(end-tests)