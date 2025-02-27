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

(test extract-from-strings-default-regexp
  (let* ((strings-to-pass (cdr (assoc 'line-parser::output (line-parser::duplicate-free-warnings ": warning" *testfile3*))))
	 (result (line-parser::extract-from-strings strings-to-pass))
	 (size-expect 6)
	 (size-actual (length result))
	 (size-1234-expect 3)
	 (size-1234-actual (cdr (assoc "CA1234" result :test 'equal))))
    (is (= size-actual size-expect)
	     "Wrong number of results when extracting strings from test-buildoutput: expected ~a, got ~a"
	     size-expect
	     size-actual)
    (is (= size-1234-actual size-1234-expect)
	     "Expected 'CA1234' to occurr exactly ~a times, but found it ~a times."
	     size-1234-expect
	     size-1234-actual)))

(test filter-all-found-warnings/errors-cleans-temp-files
  (let* ((tmp-path (asdf:system-relative-pathname "line-parser" "static/"))
	 (count-initial (length (uiop:directory-files tmp-path))))
    (line-parser::filter-all-found-warnings/errors *testfile3* 'warning)
    (is (= (length (uiop:directory-files tmp-path)) count-initial)
	"The temporary file generated by `filter-all-found-warnings/errors-cleans-temp-files' is not cleaned properly.")))

(test filter-all-found-warnings/errors-nil-for-nonexisting-key
  (is (equal (line-parser::filter-all-found-warnings/errors *testfile3* 'error) '(() ()))))
