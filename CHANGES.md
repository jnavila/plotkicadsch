v0.8.0
------

 - plotgitsch: split wires with respect to jonction points to refine diff
 - manage escaped strings in fields
 - Remove dependency to Core_kernel


v0.7.0
------

 - plotgitsch: introduce -z option to highlight zones of changes
 - plotgitsch: implement home made internal diff
 - plotgitsch: fix bug in keep option (#39)
 - plotgitsch: z-order shapes in SVG according to new, old, idem status

v0.6.1
------

 - Switch to ocaml 4.09.0 and JaneStreet libs 0.13.x

v0.6.0
------

 - Search libs and schs recursively from working directory (fixes #33)

v0.5.2
------

 - Manage Fields with delimited strings
 - Use environment variables for internal differ
 - add dependency to an implementation of digestif

v0.5.1
------

 - fix compatibility with kicad 5.x

v0.5.0
------

 - add compatibility with kicad 5.x
 - update 'massaging' with rescue lib
 - become independant on line endings types
 - add an option to select output directory in plotkicadsch
 - update lib versions

v0.4.0
------

 - add the -l option
 - add the -c option
 - add the -t option
 - add the -k option
 - enhance svg drawing
 - add appveyor builds and Windows binaries
 - switch to dune
 - add a small user's guide

v0.3.0
------

 - rework opam interaction
 - fix version watermark
 - allow to have project in subdir of git working dir
 - update Readme for installation procedure
 - fix arc drawing (works for arcs less than 180°)
 - set up documentation of lib

v0.2.0
------

 - Add plotgitsch internal diffing
 - fix #2, #3 and #4

v0.0.1
------

 - Initial release
