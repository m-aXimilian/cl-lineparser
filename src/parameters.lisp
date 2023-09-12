(in-package :line-parser)

(defparameter warning-list-config-file (asdf:system-relative-pathname "line-parser" "static/WarningList"))

(defparameter warning-list
  (with-open-file (s warning-list-config-file :direction :input :if-does-not-exist :error)
    (eval (read s)))
  "The list of warning strings defined in the +warning-list-config-file+.")

(defparameter file-filter-regexp "[A-Za-z0-9.]*\\.cs\\([0-9]*\\,[0-9]*\\)"
  "Filters the filename with occurence lines in brackets.")

(defparameter dotnet-stylecop-analyzer-regexp "SA[0-9]{4}|CA[0-9]{4}|CS[0-9]{4}"
  "Regex for filtering dotnet keys of styelcop and code analyzer warnings.")


(defun change-warning-list ()
  "Updat the `warning-list' parameter based on `warning-list-config-file'."
  (if (uiop:file-exists-p warning-list-config-file)
      (with-open-file (s warning-list-config-file :direction :input :if-does-not-exist :error)
	(setf warning-list (eval (read s))))))
