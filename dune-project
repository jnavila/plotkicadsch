(lang dune 2.9)
(name plotkicadsch)
(generate_opam_files true)

(package
	(name plotkicadsch)
	(synopsis "A command line tool to plot and compare KiCad schematics")
	(description "plotkicadsch lets you export Kicad schematics to SVG, up to version 9, while plotgitsch lets you compare git revisions of the schematic pages")
	(authors "Jean-Noël Avila <jn.avila@free.fr>")
	(maintainers "Jean-Noël Avila <jn.avila@free.fr>")
	(bug_reports "https://github.com/jnavila/plotkicadsch/issues")
	(version 0.4.0)
	(source (uri "git+https://github.com/jnavila/plotkicadsch"))
	(homepage "https://github.com/jnavila/plotkicadsch")
	(license GPL-2.0-or-later)
	(depends
	 ocaml
      	 base64
	 kicadsch
	 digestif
	 dune
	 cmdliner
	 lwt_ppx
	 ppx_deriving
	 tyxml
	 git-unix
	 sha
	)
)
(package
	(name kicadsch)
	(synopsis "A library to parse KiCad schematics")
	(description "kicadsch is a library to parse KiCad schematics, up to version 9")
	(authors "Jean-Noël Avila <jn.avila@free.fr>")
	(maintainers "Jean-Noël Avila <jn.avila@free.fr>")
	(bug_reports "https://github.com/jnavila/plotkicadsch/issues")
	(version 0.4.0)
	(source (uri "git+https://github.com/jnavila/plotkicadsch"))
	(homepage "https://github.com/jnavila/plotkicadsch")
	(license GPL-2.0-or-later)
	(depends
	ocaml
	uuidm
	dune
	csexp
	ppx_deriving
	ppx_inline_test
	parsexp
	ounit2
	))
