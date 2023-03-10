(defpackage buildwarning-parser
  (:use :cl :clingon)
  (:export :main))
(in-package :buildwarning-parser)

(defun warning-filter (w f)
  "Name of the warning W  and input file F. Returns a list holding every line in F that contains W."
  (let ((o '()))
    (with-open-file (in f :direction :input :external-format :iso-8859-1)
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(if (not (eq (search w line) nil))
	    (push line o)))
      o)))

(defun drop-after (s i)
  "In input string I, drop everything after string S."
  (subseq i 0 (+ 1 (position s i :test #'string-equal))))

(defun duplicate-free-warnings (w f d)
  "Uses the list of warning-filter based on warning W and input file F, removes duplicates in this, and drops everything in the output list after string D."
  (let ((raw-dup-free (remove-duplicates (warning-filter w f) :test #'string-equal :from-end t)))
    (mapcar #'(lambda (n) (drop-after d n)) raw-dup-free)))

(defun print-list (l)
  "Print every element in list L to a new line."
  (dolist (line l)
    (print line)))

(defun save-list (f l)
  "Save list L to the file F."
  (with-open-file (stream f :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (line l)
      (format stream "~A~%" line))))

(defun top-level/command ()
  (clingon:make-command
   :name "buildwarning-parser"
   :description "Parse a file of a dotnet build to generate a unique warning list;"
   :usage "[-w <WARNING>] [-i <INPUTFILE>] [-d <dropafter>] [-o <outputfilename>]"
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
    :key :warning)
   (clingon:make-option
    :string
    :description "The string after which the rest of the line is dropped."
    :short-name #\d
    :long-name "drop"
    :key :drop
    :initial-value ")")
   (clingon:make-option
    :string
    :description "The outputfile to store the results in."
    :short-name #\o
    :long-name "output"
    :key :output)))

(defun top-level/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
	(input (clingon:getopt cmd :input))
	(warning (clingon:getopt cmd :warning))
	(drop (clingon:getopt cmd :drop))
	(outfile (clingon:getopt cmd :output)))
    (progn (format t "Total number of args ~A ~%" (length args))
	   (format t "Will check for ~A in ~A ~%" warning input)
	   (let ((output (duplicate-free-warnings warning input drop)))
	     (progn
	       (format t "Found ~A occurrences of ~A in the input file.~%" (length output) warning)
	       (if outfile
		   (progn
		     (format t "Dropping results to ~A~%" outfile)
		     (save-list outfile output)))
	       (print-list output))))))

;; main entry point
(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))


