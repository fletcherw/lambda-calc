.PHONY: all
all: repl

.PHONY: repl
repl: 
	cd src && make repl

.PHONY: clean
clean: cleanbuild
	rm repl

.PHONY: cleanbuild
cleanbuild:
	cd src && rm -f *.cmi *.cmx *.o 
