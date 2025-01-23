.DEFAULT_GOAL := default

install:
	brew install opam ocaml

init: install
	opam init -y

clean:
	rm sim8086{.cmi,.cmo,}

run:
	opam exec -- ocamlfind ocamlc -package bitv -linkpkg -o sim8086 sim8086.ml && ./sim8086

default: run clean
