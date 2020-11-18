(require 'day5)
(require 'test-simple)
(test-simple-start)

;; Part1
(assert-t (nice "ugknbfddgicrmopn"))
(assert-t (nice "aaa"))
(assert-nil (nice "jchzalrnumimnmhp"))
(assert-nil (nice "haegwjzuvuyypxyu"))
(assert-nil (nice "dvszwmarrgswjxmb"))

;; Part2
(assert-t (two-pair "xyxy"))
(assert-t (two-pair "aabcdefgaa"))
(assert-nil (two-pair "aaa"))
(assert-t (aba-pattern "xyx"))
(assert-t (aba-pattern "abcdefeghi"))
(assert-t (aba-pattern "aaa"))

(assert-nil (aba-pattern "abc"))
(assert-nil (aba-pattern "1234567"))

(assert-t (nice2 "qjhvhtzxzqqjkmpb") "1")
(assert-t (nice2 "xxyxx") "2")
(assert-nil (nice2 "uurcxstgmygtbstg") "3")
(assert-nil (nice2 "ieodomkazucvgmuy") "4")

(end-tests)

(provide 'test-day5)
