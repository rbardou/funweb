OCAMLBUILD := ocamlbuild
OCAMLBUILD := $(OCAMLBUILD) -no-links
OCAMLBUILD := $(OCAMLBUILD) -use-ocamlfind
OCAMLBUILD := $(OCAMLBUILD) -tag annot
OCAMLBUILD := $(OCAMLBUILD) -I src

default:
	$(OCAMLBUILD) src/funweb.cma examples/convert.byte examples/login.byte
	js_of_ocaml _build/examples/convert.byte -o _build/examples/convert.js
	js_of_ocaml _build/examples/login.byte -o _build/examples/login.js

clean:
	rm -rf _build

.PHONY: default clean
