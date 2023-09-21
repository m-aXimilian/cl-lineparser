(in-package :line-parser/tests)

(def-suite* lparser-configuration
  :in lparser
  :description "Testing for the configuration of `line-parser'.")


(test change-warning-list-works
  (let ((orig-warning-file line-parser::warning-list-config-file)
	(orig-list line-parser::warning-list))
    (setf line-parser::warning-list-config-file *testfile2*)
    (line-parser::change-warning-list)
    (is-false (equal line-parser::warning-list orig-list))
    (setf line-parser::warning-list-config-file orig-warning-file)
    (line-parser::change-warning-list)
    (is (equal line-parser::warning-list orig-list))))
