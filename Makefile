.PHONY: all
all: repl test

.PHONY: repl
repl:
	cd src && make repl

.PHONY: test
test:
	cd src && make test

.PHONY: clean
clean: cleanbuild
	rm -f repl test

.PHONY: cleanbuild
cleanbuild:
	cd src && rm -f *.cmi *.cmx *.o 
