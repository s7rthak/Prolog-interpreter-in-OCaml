all:
	ocamlbuild -use-ocamlfind main.byte; ./main.byte

run:
	./main.byte

build: 
	ocamlbuild -use-ocamlfind main.byte
