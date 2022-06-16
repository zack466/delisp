run:
	sbcl --eval '(asdf:load-system :langs)' \
		--eval '(quit)'

interactive:
	sbcl --eval '(asdf:load-system :langs)'

build:
	sbcl --eval '(asdf:make :langs)' \
		--eval '(quit)'
