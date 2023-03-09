(defsystem "buildwarning-parser"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:clingon)
  :components ((:module "src"
		:pathname #P"src/"
                :components
                ((:file "main"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "build/buildwarning-parser"
  :entry-point "buildwarning-parser:main"
  :in-order-to ((test-op (test-op "buildwarning-parser/tests"))))

(defsystem "buildwarning-parser/tests"
  :author ""
  :license ""
  :depends-on ("buildwarning-parser"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for buildwarning-parser"
  :perform (test-op (op c) (symbol-call :rove :run c)))
