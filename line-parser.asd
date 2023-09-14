(defsystem "line-parser"
  :version "0.6.0"
  :author ""
  :license ""
  :depends-on (:clingon
	       :cl-ppcre)
  :components ((:module "src"
		:pathname #P"src/"
                :components
                ((:file "package")
		 (:file "main")
		 (:file "parameters")
		 (:file "parser")
		 (:file "commandline"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "build/line-parser"
  :entry-point "line-parser:main"
  :in-order-to ((test-op (test-op "line-parser/tests"))))

(defsystem "line-parser/tests"
  :author ""
  :license ""
  :depends-on ("line-parser"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "package")
		 (:file "testparameters")
		 (:file "helpers")
		 (:file "main")
		 (:file "extensions"))))
  :description "Test system for line-parser"
  :perform (test-op (op c) (symbol-call :rove :run c)))
