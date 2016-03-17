default:
	ocamlbuild -use-ocamlfind src/nottyme.native

clean:
	ocamlbuild -clean
