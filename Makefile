.PHONY: test check

build:
	dune build

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	dune build @doc

count: 
	cloc --by-file --include-lang=OCaml .