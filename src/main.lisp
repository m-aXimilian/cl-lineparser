(defpackage buildwarning-parser
  (:use :cl :clingon)
  (:export :main))
(in-package :buildwarning-parser)

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

(defun print-list (l)
  (dolist (line l)
    (print line)))

(defun top-level/command ()
  (clingon:make-command
   :name "buildwarning-parser"
   :description "Parse a file of a dotnet build to generate a unique warning list;"
   :usage "[-w <WARNING>] [-i <INPUTFILE>]"
   :options (top-level/options)
   :handler #'top-level/handler))

(defun top-level/options ()
  (list
   (clingon:make-option
    :string
    :description "The path to the input file with the buildoutput."
    :short-name #\i
    :long-name "input"
    :key :input)
   (clingon:make-option
    :string
    :description "The warning key. (Some random string that gets filtered)"
    :short-name #\w
    :long-name "warning"
    :key :warning)))

(defun top-level/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
	(input (clingon:getopt cmd :input))
	(warning (clingon:getopt cmd :warning)))
    (progn (format t "Total number of args ~A ~%" (length args))
	   (format t "Will check for ~A in ~A ~%" warning input)
	   (let ((output (duplicate-free-warnings warning input)))
	     (print-list output)))))

;; main entry point
(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))


