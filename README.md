![build result](https://travis-ci.org/jnavila/plotkicadsch.svg?branch=master)

# Kicad schematic plotter

PlotKicadsch is a small tool to export Kicad Sch files to SVG pictures. In the future, export to other formats may be available (PDF, PNG).

This package also provides the `plotgitsch` command which allows to visually compare git revisions of schematics:

![Visual diff](docs/svg_diff.png)

For more information type `plotgitsch --help`.

## Objectives

This project is mainly an attempt at using ocaml with functional programing on a pet real-world project.

The quality of the output is not a first requirement (meaning: not supposed to match Kicad one to one), but the accuracy of positioning matters.

# Installation

The stable version of plotkicadsch can be installed with [opam](http://opam.ocaml.org/):

```bash
$ opam switch 4.06.0
$ eval `opam config env`
$ opam update
$ opam install plotkicadsch
```

If you don't use opam consult the .opam files for build instructions.

## Master version

The latest running version can also be installed from this repo by pinning the project in opam:

```bash
$ opam pin add kicadsch .
$ opam pin add plotkicadsch .
$ opam update
$ opam install plotkicadsch
```

## Contributing

This project accepts GitHub pull requests, but as it is a self-teaching project, I would prefer to do all the core stuff. If you see some parts of the code whose style is not ocamlish or not FP ready, please let me know.

If this project happens to be of any use, let me know also!
