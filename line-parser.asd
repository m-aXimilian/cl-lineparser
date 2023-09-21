(defsystem "line-parser"
  :version "0.7.0"
  :author "Maximilian Kueffner"
  :mailto "kueffnermax@gmail.com"
  :license "zlib"
  :depends-on (:clingon
	       :cl-ppcre)
  :components ((:module "src"
		:pathname #P"src/"
                :components
                ((:file "package")
		 (:file "parameters")
		 (:file "main")
		 (:file "parser")
		 (:file "commandline"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "build/line-parser"
  :entry-point "line-parser:main"
  :in-order-to ((test-op (test-op "line-parser/tests"))))

(defsystem "line-parser/tests"
  :author "Maximilian Kueffner"
  :license "zlib"
  :depends-on ("line-parser"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "package")
		 (:file "testparameters")
		 (:file "helpers")
		 (:file "main")
		 (:file "extensions")
		 (:file "configuration"))))
  :description "Test system for line-parser"
  :perform (test-op (op c) (symbol-call :rove :run c)))
