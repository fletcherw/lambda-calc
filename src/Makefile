.PHONY: all
all: repl

SOURCES=lex.ml parse.ml repl.ml
INTERFACES=$(SOURCES:.ml=.mli)

repl: $(SOURCES) interfaces
	ocamlopt -o ../repl $(SOURCES) 

interfaces: $(INTERFACES)
	ocamlc -c $(INTERFACES)

.PHONY: clean
clean: cleanbuild
	rm ../repl

.PHONY: cleanbuild
cleanbuild:
	rm -f *.cmi *.cmx *.o 
