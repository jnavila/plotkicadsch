# Kicad schematic plotter

PlotKicadsch is a small tool to export Kicad Sch files to SVG pictures. In the future, export to other formats may be available (PDF, PNG).

## Objectives
This project is mainly an attempt at using ocaml with functional programing on a pet real-world project.

The quality of the output is not a first requirement (meaning: not supposed to match Kicad one to one), but the accuracy of positioning matters. The end objective is to be able to provide a visual diff on sch files for version control.

# Installation

plotkicadsch can be installed with opam:

    $ opam install plotkicadsch

If you don't use opam consult the opam file for build instructions.

## Contributing

This project accepts GitHub pull requests, but as it is a self-teaching project, I would prefer to do all the core stuff. If you see some parts of the code whose style is not ocamlish or not FP ready, please let me know.

If this project happens to be of any use, let me know also!
