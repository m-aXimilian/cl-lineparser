(defpackage buildwarning-parser/tests/main
  (:use :cl
        :buildwarning-parser
        :rove))
(in-package :buildwarning-parser/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :buildwarning-parser)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
