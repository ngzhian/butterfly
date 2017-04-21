OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

SRCDIR=src
MAIN=test

.PHONY: build help clean

default: all

all: build

build:
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) $(MAIN).$(BUILD)

clean:
	$(OCAMLBUILD) -clean

