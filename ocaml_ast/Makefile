OCAMLBUILD ?= ocamlbuild

all: main.native
clean:
	$(OCAMLBUILD) -clean

main.native:
	$(OCAMLBUILD) -use-ocamlfind $@
.PHONY: main.native
