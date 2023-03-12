(defpackage buildwarning-parser/tests/main
  (:use :cl
        :buildwarning-parser
        :fiveam))
(in-package :buildwarning-parser/tests/main)


(defparameter *testfile1* (merge-pathnames (uiop/os:getcwd) "testfile1"))

(def-suite bparser-main
  :description "Global tests for the buildwarning parser.")

(in-suite bparser-main)

(test warning-filter-without-drop-correct-output-size
  "The raw output contains all findings of the search character."
  (let* ((output-list (buildwarning-parser::warning-filter "i" *testfile1*))
	 (output-size (length output-list)))
    (is (= 4 output-size))
    "Expected list of size 4 but got ~A elements" output-size))

(test duplicate-free-warnings-one-duplicate-correct-removal
  "Duplicate lines get removed properly."
  (let* ((dup-free-list (buildwarning-parser::duplicate-free-warnings "i" *testfile1*))
	 (raw-size (cdr (assoc 'buildwarning-parser::raw-length dup-free-list)))
	 (set-size (cdr (assoc 'buildwarning-parser::set-length dup-free-list))))
    (is (= 4 raw-size))
    (is (= 3 set-size))))

(test duplicate-drop-inexisting-char-empty-output
  "Filtering for a non existing character procudes an empty outpt."
  (let* ((dup-free-list (buildwarning-parser::duplicate-free-warnings "#" *testfile1*)))
    (is-false (cdr (assoc 'buildwarning-parser::output dup-free-list)))
    (is (= 0 (cdr (assoc 'buildwarning-parser::raw-length dup-free-list))))
    (is (= 0 (cdr (assoc 'buildwarning-parser::set-length dup-free-list))))))

(test dropping-after-char-creates-different-outputs
  "When dropping after a character, the output list is different than when not dropping."
  (let* ((dup-free-list-raw (buildwarning-parser::duplicate-free-warnings "i" *testfile1*))
	 (raw-set-length (cdr (assoc 'buildwarning-parser::set-length dup-free-list-raw)))
	 (raw-raw-length (cdr (assoc 'buildwarning-parser::raw-length dup-free-list-raw)))
	 (dup-free-list-drop (buildwarning-parser::duplicate-free-warnings "i" *testfile1* ")"))
	 (dup-set-length (cdr (assoc 'buildwarning-parser::set-length dup-free-list-drop)))
	 (dup-raw-length (cdr (assoc 'buildwarning-parser::raw-length dup-free-list-drop)))
	 (line-lengths-raw (check-line-lengths (cdr (assoc 'buildwarning-parser::output dup-free-list-raw))))
	 (line-lengths-drop (check-line-lengths (cdr (assoc 'buildwarning-parser::output dup-free-list-drop)))))
    (is-false (equal line-lengths-raw line-lengths-drop))
    (is (= raw-raw-length dup-raw-length))
    (is (= raw-set-length dup-set-length))))

;; helpers
(defun check-line-lengths (l)
  "Given a list of strings L, create a list with the length of each string in L."
  (let ((lengths '()))
    (dolist (line l)
      (push (length line) lengths))
    lengths))


