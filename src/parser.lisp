(in-package :line-parser)

(defun warning-filter-tmp-file (w f)
  "Name of the warning W  and input file F. Returns the number holding of findings of W in F."
  (let ((o 0))
    (with-open-file (in f :direction :input :external-format :iso-8859-1)
      (progn
	(with-open-file (out "tmp" :direction :output :if-exists :append :if-does-not-exist :create)
	  (do ((line (read-line in nil)
		     (read-line in nil)))
	      ((null line))
	    (if (not (eq (search w line) nil))
		;;(push line o)
		(progn
		  (format out "~A~%" line)
		  (incf o)
		  )
		))))
      o)))

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

(defun duplicate-free-warnings (w f &optional d)
  "Uses the list of warning-filter based on warning W and input file F, removes duplicates in this, and drops everything in the output list after string D."
  (let* ((raw-results (warning-filter w f))
	 (raw-dup-free (remove-duplicates raw-results :test #'string-equal :from-end t)))
    (list
     (cons 'output
	   (if d
	       (mapcar #'(lambda (n) (drop-after d n)) raw-dup-free)
	       raw-dup-free))
     (cons 'raw-results raw-results)
     (cons 'raw-length (length raw-results))
     (cons 'set-length (length raw-dup-free)))))

(defun print-list (l)
  "Print every element in list L to a new line."
  (dolist (line l)
    (print line)))

(defun save-list (f l)
  "Save list L to the file F."
  (with-open-file (stream f :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (line l)
      (format stream "~A~%" line))))
