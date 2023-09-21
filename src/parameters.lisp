(in-package :line-parser)

(defparameter *app-name* "line-parser")

(defparameter *config-dir* (let ((confdir
				   (cond
				     ((find :win32 *features*) (merge-pathnames "line-parser/" (truename (uiop/os:getenv "programdata"))))
				     ((find :linux *features*) (merge-pathnames "line-parser/" (progn
												 (ensure-directories-exist "~/.config/")
												 (truename "~/.config/"))))
				     (t (error "Could not find a base directory for the configuration. Take care of `*config-dir*'.")))))
			     (ensure-directories-exist confdir)
			     confdir)
  "Base directory to where possible config files might be located.")

(defparameter *config-file* (merge-pathnames "lparser.lisp.config" *config-dir*)
  "Global file for configuration persistance.")

(defparameter warning-list-config-file (asdf:system-relative-pathname "line-parser" "static/WarningList")
  "Location of the warning list. Has to be a quoted, plain list of strings.
This is the basis for `warning-list' and should be used to plut in alternative lists of search strings.
When this parameter was altered, call `change-warning-list' to signal a change to the `warning-list' parameter. ")

(defparameter warning-list
  (with-open-file (s warning-list-config-file :direction :input :if-does-not-exist :error)
    (read s))
  "The list of warning strings defined in the `warning-list-config-file'.")

(defparameter file-filter-regexp "[A-Za-z0-9.]*\\.cs\\([0-9]*\\,[0-9]*\\)"
  "Filters the filename with occurence lines in brackets.")

(defparameter dotnet-stylecop-analyzer-regexp "SA[0-9]{4}|CA[0-9]{4}|CS[0-9]{4}"
  "Regex for filtering dotnet keys of styelcop and code analyzer warnings.")

(defparameter extract-from-strings-default-regexp dotnet-stylecop-analyzer-regexp
  "The regex used by `extract-from-strings' for filtering input lines.")

(defun change-warning-list ()
  "Updat the `warning-list' parameter based on `warning-list-config-file'."
  (if (uiop:file-exists-p warning-list-config-file)
      (with-open-file (s warning-list-config-file :direction :input :if-does-not-exist :error)
        (setf warning-list (read s)))))

((lambda ()
   "Runs to ensure a (empty) config file is present.
Attention: this scoped (hardcoded) in package 'line-parser'."
  (if (not (uiop:file-exists-p *config-file*))
      (with-open-file (s *config-file* :direction :output)
	(format s ";; This is a auto-generated file used for configuration of the ~a.~%" *app-name*)
	(format s ";; The syntax should be something a Lisp system can understand.~%")
	(format s ";; Do NOT edit this manually!~%")
	(format s ";; You can savely delete this. Note however that this will reset every configuration that was done to the ~a~%" *app-name*)
	(format s "(in-package :line-parser)"))
      (with-open-file (s *config-file* :direction :input)
        (do ((c (read s nil :eof)
		(read s nil :eof)))
	    ((eql c :eof))
	  (eval c))))))
