.DEFAULT_GOAL := run

install:
	brew install opam ocaml

init: install
	opam init -y

run:
	opam exec -- ocamlfind ocamlc -package bitv -linkpkg -o sim8086 sim8086.ml && ./sim8086
	rm sim8086{.cmi,.cmo,}
