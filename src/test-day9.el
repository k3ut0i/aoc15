(require 'day9)
(require 'test-simple)
(test-simple-start)

(assert-equal (parse-distance "London to Dublin = 464")
	      '(:london :dublin 464)
	      "Parse london to dublin")

(end-tests)
