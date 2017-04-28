OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

SRCDIR=.
MAIN=main
TESTFILE=test.bfly

.PHONY: build help clean test

default: all

all: build

build:
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) $(MAIN).$(BUILD)

test:
	./$(MAIN).$(BUILD) $(TESTFILE)

clean:
	$(OCAMLBUILD) -clean
