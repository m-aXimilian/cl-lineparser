(in-package :line-parser)

(defun top-level/command ()
  (clingon:make-command
   :name "line-parser"
   :description "Parse a file of a dotnet build to generate a unique warning list. Attention: this will crash for very large numbers of results. The inputfilesize does not matter."
   :usage "[-w <WARNING>] [-i <INPUTFILE>] [-d <dropafter>] [-o <outputfilename>] [-r] [-s]"
   :options (top-level/options)
   :version "0.6.0"
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
    :description "The string after which the rest of the line is dropped. Ignored if nothing is specified."
    :short-name #\d
    :long-name "drop"
    :key :drop)
   (clingon:make-option
    :string
    :description "The output file to store the results in."
    :short-name #\o
    :long-name "output"
    :key :output)
   (clingon:make-option
    :boolean/true
    :description "Save additional raw results, i.e., containing duplicates as well."
    :short-name #\r
    :long-name "raw-results"
    :key :raw)
   (clingon:make-option
    :boolean/true
    :description "Run silent. Don't print results to the standard output."
    :short-name #\s
    :long-name "silent"
    :key :silent)))

(defun top-level/handler (cmd)
  (let ((input (clingon:getopt cmd :input))
	(warning (clingon:getopt cmd :warning))
	(drop (clingon:getopt cmd :drop))
	(outfile (clingon:getopt cmd :output))
	(raw (clingon:getopt cmd :raw))
	(silent (clingon:getopt cmd :silent)))
    (progn (format t "Will check for ~A in ~A ~%" warning input)
	   (let ((output (duplicate-free-warnings warning input drop)))
	     (progn
	       (let* ((raw-count (cdr (assoc 'raw-length output)))
		      (set-count (cdr (assoc 'set-length output)))
		      (unique-rate (if (> raw-count 0)
				       (/ set-count raw-count 1.0)
				       0)))
		 (format t "Found ~A occurrences of ~A in the input file of which ~A are unique (~A%).~%"
			 raw-count
			 warning
			 set-count
			 (* 100 unique-rate)))
	       (if outfile
		   (progn
		     (format t "Dropping results to ~A~%" outfile)
		     (save-list outfile (cdr (assoc 'output output)))
		     (if raw
			 (save-list (concatenate 'string outfile "-raw") (cdr (assoc 'raw-results output))))))
	       (if (not silent)
		   (progn
		     (format t "~%~A~%Duplicate-free findings:~%" "--------------------------------------------------")
		     (print-list (cdr (assoc 'output output))))))))))
