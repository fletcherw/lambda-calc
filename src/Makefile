.PHONY: all
all: repl

SOURCES=util.ml lex.ml parse.ml repl.ml
INTERFACES=$(SOURCES:.ml=.mli)

repl: main.ml $(SOURCES) interfaces
	ocamlopt -o ../repl $(SOURCES) main.ml

test: test.ml $(SOURCES) interfaces
	ocamlopt -o ../test $(SOURCES) test.ml

interfaces: $(INTERFACES)
	ocamlc -c $(INTERFACES)

.PHONY: clean
clean: cleanbuild
	rm ../repl

.PHONY: cleanbuild
cleanbuild:
	rm -f *.cmi *.cmx *.o 
