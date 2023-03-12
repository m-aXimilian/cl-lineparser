LISP ?= sbcl --dynamic-space-size 2000 --noinform

build:
	$(LISP) --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "buildwarning-parser.asd"))' \
		--eval '(ql:quickload :buildwarning-parser)' \
		--eval '(asdf:make :buildwarning-parser)' \
		--eval '(quit)'
test:
	$(LISP) --non-interactive --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "buildwarning-parser.asd"))' \
	--eval '(uiop:chdir (merge-pathnames (uiop/os:getcwd) "tests"))' \
	--eval '(ql:quickload :buildwarning-parser/tests)' \
	--eval '(in-package "BUILDWARNING-PARSER/TESTS/MAIN")' \
	--eval '(in-suite bparser-main)' \
	--eval '(run!)' \

clean:
	rm -rf build

.PHONY: clean
