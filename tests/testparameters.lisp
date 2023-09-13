(in-package :line-parser/tests)

(defparameter *system-relative-test-data-directory* "tests/data/")

(defparameter *testfile1*  (asdf:system-relative-pathname "line-parser" (concatenate 'string *system-relative-test-data-directory* "testfile1")))

(defparameter *testfile2* (asdf:system-relative-pathname "line-parser" (concatenate 'string *system-relative-test-data-directory* "extract-from-strings-input")))
