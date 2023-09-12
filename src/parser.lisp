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
  (let ((index-s (position s i :test #'string-equal)))
    (if (null index-s)
	i
	(subseq i 0 (+ 1 index-s)))))

(defun duplicate-free-warnings (w f &optional d)
  "Uses the list of warning-filter based on warning W and input file F, removes duplicates in this, and drops everything in the output list after string D."
  (let* ((raw-results (warning-filter w f))
	 (raw-dup-free ;; (remove-duplicates raw-results :test #'string-equal :from-end t)
	   (if d
	       (remove-duplicates (mapcar #'(lambda (n) (drop-after d n)) raw-results) :test #'string-equal :from-end t)
	       (remove-duplicates raw-results :test #'string-equal :from-end t))
	   ))
    (list
     (cons 'output raw-dup-free)
     (cons 'raw-results raw-results)
     (cons 'raw-length (length raw-results))
     (cons 'set-length (length raw-dup-free)))))

(defun check-all-warnings (f)
  "For the file F with the list of warnings from."
  (let ((all-warnings '()))
    (dolist (warning warning-list)
      (push (cons warning (duplicate-free-warnings warning f)) all-warnings))
    all-warnings))

(defun extract-from-strings (strings &optional regex)
  "A list of strings STRINGS is benchmarked by means of REGEX."
  (let ((results nil)
	(reg (if regex
		 regex
		 dotnet-stylecop-analyzer-regexp)))
    (dolist (element strings)
      (let ((parsed (cl-ppcre:scan-to-strings reg element)))
	(when parsed
	  (let ((curr (assoc parsed results :test 'equal)))
	    (if curr
		(incf (cdr curr))
		(push (cons parsed 1) results))))))
    results))

(defun save-all-warnings-statistic-results (f all-warns)
  "Drops the results in ALL-WARNS to file F."
  (with-open-file (s f :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format s "warning,raw,unique~%")
    (dolist (warn all-warns)
      (format s "~a,~d,~d~%" (car warn) (cdr (assoc 'raw-length (cdr warn))) (cdr (assoc 'set-length (cdr warn)))))))

;; Example call:
;; (save-all-warnings-with-filtered-output #P"d:/buildwarnings/filtered-output-tst.csv" (check-all-warnings #P"d:/buildwarnings/releasebuild-2023-09-05.txt") file-filter-regexp)
(defun save-all-warnings-with-filtered-output (f all-warns regex-string)
  "Saves parse outputs to a file F with a list of warnings ALL-WARNS and filters the raw output per warning with the REGEX-STRING."
  (with-open-file (s f :direction :output :if-exists :supersede)
    (format s "key;raw count;unique count;locations~%")
    (dolist (w all-warns)
      (let ((files (cdr (assoc 'output (cdr w))))
	    (warning (car w))
	    (raw (cdr (assoc 'raw-length (cdr w))))
	    (set (cdr (assoc 'set-length (cdr w)))))
	(labels ((filter (n)
		   (scan-to-strings regex-string n)))
	  (let ((files (format nil "~{~a~^~%~}" (mapcar #'filter files))))
	    (format s "~a;~d;~d;~s~%" warning raw set files)))))))

(defun print-list (l)
  "Print every element in list L to a new line."
  (dolist (line l)
    (print line)))

(defun save-list (f l)
  "Save list L to the file F."
  (with-open-file (stream f :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (line l)
      (format stream "~A~%" line))))
