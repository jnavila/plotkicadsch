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
	[ -x $$(opam config var root)/plugins/opam-publish/repos/plotkicadsch ] || \
	  opam-publish repo add plotkicadsch jnavila/plotkicadsch
	topkg tag
	topkg distrib

REPO=../opam-repository
PACKAGES=$(REPO)/packages


# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
