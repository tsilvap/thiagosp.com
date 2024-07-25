LISP ?= sbcl

build:
	$(LISP) --eval '(asdf:make "thiagosp")' \
		--eval '(quit)'
