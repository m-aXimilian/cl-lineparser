(in-package :line-parser/tests)

(def-suite lparser-main
  :description "Global tests for the line parser.")

(in-suite lparser-main)

(test drop-after-base
  (let ((input "the fist e passed")
	(output-e "the")
	(output-p "the fist e p"))
    (is (equal (line-parser::drop-after "e" input) output-e))
    (is (equal (line-parser::drop-after "p" input) output-p))))

(test warning-filter-without-drop-correct-output-size
  "The raw output contains all findings of the search character."
  (let* ((output-list (line-parser::warning-filter "i" *testfile1*))
	 (output-size (length output-list)))
    (is (= 4 output-size))
    "Expected list of size 4 but got ~A elements" output-size))

(test duplicate-free-warnings-one-duplicate-correct-removal
  "Duplicate lines get removed properly."
  (let* ((dup-free-list (line-parser::duplicate-free-warnings "i" *testfile1*))
	 (raw-size (cdr (assoc 'line-parser::raw-length dup-free-list)))
	 (set-size (cdr (assoc 'line-parser::set-length dup-free-list))))
    (is (= 4 raw-size))
    (is (= 3 set-size))))

(test duplicate-drop-inexisting-char-empty-output
  "Filtering for a non existing character procudes an empty outpt."
  (let* ((dup-free-list (line-parser::duplicate-free-warnings "#" *testfile1*)))
    (is-false (cdr (assoc 'line-parser::output dup-free-list)))
    (is (= 0 (cdr (assoc 'line-parser::raw-length dup-free-list))))
    (is (= 0 (cdr (assoc 'line-parser::set-length dup-free-list))))))

(test dropping-after-char-creates-different-outputs
  "When dropping after a character, the output list is different than when not dropping."
  (let* ((dup-free-list-raw (line-parser::duplicate-free-warnings "i" *testfile1*))
	 (raw-set-length (cdr (assoc 'line-parser::set-length dup-free-list-raw)))
	 (raw-raw-length (cdr (assoc 'line-parser::raw-length dup-free-list-raw)))
	 (dup-free-list-drop (line-parser::duplicate-free-warnings "i" *testfile1* ")"))
	 (dup-set-length (cdr (assoc 'line-parser::set-length dup-free-list-drop)))
	 (dup-raw-length (cdr (assoc 'line-parser::raw-length dup-free-list-drop)))
	 (line-lengths-raw (check-line-lengths (cdr (assoc 'line-parser::output dup-free-list-raw))))
	 (line-lengths-drop (check-line-lengths (cdr (assoc 'line-parser::output dup-free-list-drop)))))
    (is-false (equal line-lengths-raw line-lengths-drop))
    (is (= raw-raw-length dup-raw-length))
    (is (= raw-set-length dup-set-length))))

(test valid-output-despite-no-drop-found
  "Even if not every line contains the drop character D, the count of results should be the same
as if there was no attempt to drop parts of a line."
  (let* ((dup-free-list-wo-d (line-parser::duplicate-free-warnings ")" *testfile1*))
	 (dup-free-list-drop (line-parser::duplicate-free-warnings ")" *testfile1* "y"))
	 (line-lenghts-wo-d (check-line-lengths (cdr (assoc 'line-parser::output dup-free-list-wo-d))))
	 (line-lengths-drop (check-line-lengths (cdr (assoc 'line-parser::output dup-free-list-drop)))))
    (is-false (equal line-lenghts-wo-d line-lengths-drop))
    (is (= (length line-lenghts-wo-d) (length line-lengths-drop)))))

;; helpers
(defun check-line-lengths (l)
  "Given a list of strings L, create a list with the length of each string in L."
  (let ((lengths '()))
    (dolist (line l)
      (push (length line) lengths))
    lengths))


