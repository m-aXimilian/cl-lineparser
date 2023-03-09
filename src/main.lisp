;; (asdf:load-asd #P"d:/Projects/buildwarning-parser/buildwarning-parser.asd")
;; (asdf:load-system "buildwarning-parser")
;; (ql:quickload "buildwarning-parser")
;; (sb-ext:save-lisp-and-die #P"d:/Projects/buildwarning-parser/build/buildwarning-parser" :toplevel #'buildwarning-parser::main :executable t)


(defpackage buildwarning-parser
  (:use :cl :clingon))
(in-package :buildwarning-parser)


(defparameter *warning-output-path* "d:/buildwarnings/ca2016-secondfix")


(defun warning-filter (w f)
  "Name of the warning W  and input file F."
  (let ((o '()))
    (with-open-file (in f :direction :input :external-format :iso-8859-1)
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(if (not (eq (search w line) nil))
	    (push line o)))
      o)))

(defun drop-after (s i)
  "In input string I, drop everything after string S"
  (subseq i 0 (+ 1 (position s i :test #'string-equal))))


(defun duplicate-free-warnings (w f)
  "Wrapper around warning-filter."
  (let ((raw-dup-free (remove-duplicates (warning-filter w f) :test #'string-equal :from-end t)))
    (mapcar #'(lambda (n) (drop-after ")" n)) raw-dup-free)))


;; e.g.(main "CA2016" *warning-output-path*)
(defun main (w f &optional dra)
  (duplicate-free-warnings w f))



;; cmdline stuff

;; blah blah blah.
