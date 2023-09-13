(in-package :line-parser/tests)

(def-suite* lparser-extensions
  :in lparser
  :description "Testing for high(er) level wrappers of the parsing component of `line-parser'.")

(test extract-from-strings-base
  (let* ((input-list (with-open-file (s *testfile2* :direction :input)
		       (read s)))
	 (error-result (line-parser::extract-from-strings input-list ": error"))
	 (error-findings 6))
    (is (equal (car (car error-result)) ": error"))
    (is (= (cdr (car error-result)) error-findings))
    (is (< (cdr (car error-result)) (length input-list)))))
