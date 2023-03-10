LISP ?= sbcl

build:
	$(LISP) --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "buildwarning-parser.asd"))' \
		--eval '(ql:quickload :buildwarning-parser)' \
		--eval '(asdf:make :buildwarning-parser)' \
		--eval '(quit)'
