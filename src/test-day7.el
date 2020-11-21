(require 'day7)
(require 'test-simple)

(test-simple-start)

(assert-t (= 14 (run-gate '(:lshift 7 1))) "LSHIFT 7 by 1")

(assert-t (= 13 (run-gate '(:or 12 5))) "OR 12 and 5")

(assert-t (equal (parse-joint "x AND y -> z")
		 '((:and (:wire "x") (:wire "y")) . (:wire "z")))
	  "PARSE JOINT AND xyz")

(assert-t (equal (parse-joint "NOT e -> f")
		 '((:not (:wire "e")) . (:wire "f")))
	  "PARSE JOINT NOT ef")
(assert-t (equal (parse-joint "x LSHIFT 23 -> y")
		 '((:lshift (:wire "x") (:const 23)) . (:wire "y")))
	  "PARSE JOINT LSHIFT xy")
(assert-t (equal (parse-joint "x RSHIFT 2 -> y")
		 '((:rshift (:wire "x") (:const 2)) . (:wire "y")))
	  "PARSE JOINT RSHIFT xy")
(end-tests)
