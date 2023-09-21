LISP ?= sbcl --noinform --non-interactive

build:
	$(LISP) --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "line-parser.asd"))' \
		--eval '(ql:quickload :line-parser)' \
		--eval '(asdf:make :line-parser)' \
		--eval '(quit)'
test:
	$(LISP) --eval '(asdf:load-asd (merge-pathnames (uiop/os:getcwd) "line-parser.asd"))' \
	--eval '(ql:quickload :line-parser/tests)' \
	--eval '(in-package "LINE-PARSER/TESTS")' \
	--eval '(run-all-tests)' \

clean:
	rm -rf build

.PHONY: clean
