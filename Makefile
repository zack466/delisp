run:
	sbcl --eval '(asdf:load-system :delisp)' \
		--eval '(quit)'

interactive:
	sbcl --eval '(asdf:load-system :delisp)'

build:
	sbcl --eval '(asdf:make :delisp)' \
		--eval '(quit)'

test:
	sbcl --eval '(asdf:test-system :delisp)' \
		--eval '(quit)'
