(defpackage line-parser
  (:use :cl :clingon :cl-ppcre)
  (:export
   :main
   :warning-filter-tmp-file
   :warning-filter
   :drop-after
   :duplicate-free-warnings
   :check-all-warnings
   :extract-from-strings
   :filter-all-found-warnings/errors
   :compare-filtered-statistics
   :save-compared-results
   :save-all-warnings-statistic-results
   :save-all-warnings-with-filtered-output))
