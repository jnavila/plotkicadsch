.PHONY: build clean test install uninstall distrib publish release

build:
	jbuilder build --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev

install:
	jbuilder install

uninstall:
	jbuilder uninstall

distrib:
	[ -x $$(opam config var root)/plugins/opam-publish/repos/plotkicadsch ] || \
	  opam-publish repo add plotkicadsch jnavila/plotkicadsch
	topkg tag
	topkg distrib

publish:
	topkg publish
	topkg opam pkg
	topkg opam submit

release: distrib publish
