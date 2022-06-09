run:
	sbcl --eval '(asdf:load-system :langs)' \
		--eval '(quit)'

interactive:
	sbcl --eval '(asdf:load-system :langs)'
