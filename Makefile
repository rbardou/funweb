OCAMLBUILD := ocamlbuild
OCAMLBUILD := $(OCAMLBUILD) -no-links
OCAMLBUILD := $(OCAMLBUILD) -use-ocamlfind
OCAMLBUILD := $(OCAMLBUILD) -tag annot
OCAMLBUILD := $(OCAMLBUILD) -I src

default:
	$(OCAMLBUILD) src/funweb.cma \
		examples/convert.byte \
		examples/login.byte \
		examples/save.byte \
		examples/canvas.byte
	js_of_ocaml _build/examples/convert.byte -o _build/examples/convert.js
	js_of_ocaml _build/examples/login.byte -o _build/examples/login.js
	js_of_ocaml _build/examples/save.byte -o _build/examples/save.js
	js_of_ocaml _build/examples/canvas.byte -o _build/examples/canvas.js

%.example:
	$(OCAMLBUILD) src/funweb.cma examples/$*.byte
	js_of_ocaml _build/examples/$*.byte -o _build/examples/$*.js

lib:
	$(OCAMLBUILD) src/funweb.cma

doc:
	$(OCAMLBUILD) funweb.docdir/index.html

BASE = _build/src/
FILES = $(BASE)funweb.cma $(BASE)funweb.cmi

install:
	ocamlfind install funweb META $(FILES)

uninstall:
	ocamlfind remove funweb

reinstall: uninstall install

clean:
	rm -rf _build

.PHONY: default clean
