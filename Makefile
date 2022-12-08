.PHONY: test check

build:
	dune build

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

code:
	dune clean
	code .