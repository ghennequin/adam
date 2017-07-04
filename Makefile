
lib:
	ocamlbuild -use-ocamlfind adam.cma
	ocamlbuild -use-ocamlfind adam.cmxa

install:
	@ocamlfind remove adam
	@ocamlfind install adam META _build/adam.cmi \
		_build/adam.cma _build/adam.cmxa _build/adam.a _build/adam.cmx

all: lib install

clean:
	ocamlbuild -clean
	
