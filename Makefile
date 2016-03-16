default:
	ocamlfind ocamlc -package notty -package notty.lwt -package lwt -linkpkg -thread -o clock clock.ml

clean:
	rm *.cmi *.cmo
