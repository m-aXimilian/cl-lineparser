(in-package :line-parser/tests)

(defun check-line-lengths (l)
  "Given a list of strings L, create a list with the length of each string in L."
  (let ((lengths '()))
    (dolist (line l)
      (push (length line) lengths))
    lengths))
