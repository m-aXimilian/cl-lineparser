(in-package :line-parser)

;; main entry point
(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))


