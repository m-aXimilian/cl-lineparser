LISP ?= sbcl --noinform

build:
	$(LISP) --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "line-parser.asd"))' \
		--eval '(ql:quickload :line-parser)' \
		--eval '(asdf:make :line-parser)' \
		--eval '(quit)'
test:
	$(LISP) --non-interactive --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "line-parser.asd"))' \
	--eval '(uiop:chdir (merge-pathnames (uiop/os:getcwd) "tests"))' \
	--eval '(ql:quickload :line-parser/tests)' \
	--eval '(in-package "LINE-PARSER/TESTS")' \
	--eval '(run-all-tests)' \

clean:
	rm -rf build

.PHONY: clean
