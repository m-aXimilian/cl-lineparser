(defpackage buildwarning-parser/tests/main
  (:use :cl
        :buildwarning-parser
        :fiveam))
(in-package :buildwarning-parser/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :buildwarning-parser)' in your Lisp.

(def-suite bparser-main
  :description "Global tests for the buildwarning parser.")

(in-suite bparser-main)

(test warning-filter-without-drop-correct-output-size
  (let* ((output-list (buildwarning-parser::warning-filter "i" "testfile1"))
	 (output-size (length output-list)))
    (is (= 4 output-size))
    "Expected list of size 4 but got ~A elements" output-size))


(test duplicate-free-warnings-one-duplicate-correct-removal
  (let* ((dup-free-list (buildwarning-parser::duplicate-free-warnings "i" "testfile1"))
	 (raw-size (cdr (assoc 'raw-length dup-free-list)))
	 (set-size (cdr (assoc 'set-length dup-free-list))))
    (is (= 4 raw-size))
    (is (= 3 set-size))
))

(test duplicate-drop-inexisting-char-empty-output
  (let* ((dup-free-list (buildwarning-parser::duplicate-free-warnings "#" "testfile1")))
    (is-false (assoc 'output dup-free-list))))


(defun check-line-lengths (l)
  (let ((lengths '()))
    (dolist (line l)
      (push (length line) lengths))
    lengths))


