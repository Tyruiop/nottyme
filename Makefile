default:
	ocamlfind ocamlc -package notty -package notty.lwt -package lwt -linkpkg -thread -o nottyme nottyme.ml

clean:
	rm *.cmi *.cmo
