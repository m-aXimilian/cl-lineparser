(in-package :line-parser)


(defun startup ()
  )


;; main entry point
(defun main ()
  (let ((app (top-level/command)))
    (format t "Running...")
    (clingon:run app)))





