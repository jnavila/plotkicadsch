.PHONY: build clean test install uninstall distrib publish release

build:
	dune build

clean:
	dune clean

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

distrib:
	dune-release tag
	dune-release
