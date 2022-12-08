.PHONY: test check

build:
	dune build

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
code:
	-dune build
	code .
	! dune build --watch
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe