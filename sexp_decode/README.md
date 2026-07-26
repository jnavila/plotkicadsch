Sexp_decode
===========

[![pipeline status](https://gitlab.inria.fr/bmontagu/sexp_decode/badges/main/pipeline.svg)](https://gitlab.inria.fr/bmontagu/sexp_decode/-/commits/main)

``Sexp_decode`` is a library of monadic combinators for decoding
S-expressions (as defined in the
[Csexp library](https://github.com/ocaml-dune/csexp)) into structured data.

Important remark: the current API is well suited to the _complete_
parsing of S-expressions. Partial parsing (e.g., parsing only _some_
fields of an S-expression encoded record) is doable, but remains
convoluted with the current API.

Example
-------

The purpose of the library is to help the translation of S-expressions
into structured data.

For example, you may want to transform an address book encoded as an
S-expression into structured data, that is easier to process.

Let's assume your address book looks like the following:

```ocaml
open Sexp_decode

let address_book : sexp =
List
  [
    List
      [
        Atom "entry";
        List [ Atom "name"; Atom "John Doe" ];
        List [ Atom "country"; Atom "New Zealand" ];
      ];
    List
      [
        Atom "entry";
        List [ Atom "name"; Atom "Mary Poppins" ];
        List [ Atom "email"; Atom "umbrella@imaginary-domain.uk" ];
      ];
    List
      [
        Atom "entry";
        List [ Atom "name"; Atom "Groot" ];
        List [ Atom "country"; Atom "Groot" ];
      ];
  ]
```

A representation as an OCaml value that is probably easier to work
with, is by using the following ``entry`` type:

```ocaml
type entry =
  { name : string; country : string option; email : string option }

type address_book = entry list
```

It is easy to define decoders that produce values of types ``entry``
and ``address_book``:

```ocaml
let entry_decoder : entry decoder =
field "entry"
@@ let* name = field "name" atom in
   let* country = maybe @@ field "country" atom in
   let+ email = maybe @@ field "email" atom in
   { name; country; email }

let address_book_decoder : address_book decoder = list entry_decoder
```

Then, you can execute the ``run`` function, that has type
``'a decoder -> sexp -> 'a option``. It produces the following result on
our ``address_book`` example:

```ocaml
let result = run address_book_decoder address_book
(* result =
     Some
      [{name = "John Doe"; country = Some "New Zealand"; email = None};
       {name = "Mary Poppins"; country = None;
        email = Some "umbrella@imaginary-domain.uk"};
       {name = "Groot"; country = Some "Groot"; email = None}]
*)
```

In addition to the ``field``, ``maybe``, ``atom`` and ``list``
decoders, the ``Sexp_decode`` library provides combinators to build
compound decoders from basic ones, and compose them together. In
particular, decoders for variants and records are provided.

For example, with the ``fields`` combinator, you could define
``entry_decoder`` as follows:

```ocaml
let entry_decoder_alt : entry decoder =
  field "entry"
  @@ fields
       ~default:{ name = ""; country = None; email = None }
       [
         ("name", atom >>| fun name entry -> { entry with name });
         ("country", atom >>| fun country entry -> { entry with country = Some country });
         ("email", atom >>| fun email entry -> { entry with email = Some email });
       ]
```

With this alternative decoder for entries, the fields ``"name"``
``"country"`` and ``"email"`` might occur in any order, and any number
of times.

How to install the library
--------------------------

``Sexp_decode`` is available on ``opam`` and can be installed with the
following command:

```bash
opam install sexp_decode
```

Alternatively, you can install the development version as follows:

```bash
git clone https://gitlab.inria.fr/bmontagu/sexp_decode.git
cd sexp_decode
opam pin add .
```

Author: Benoît Montagu <benoit.montagu@inria.fr>

Copyright Inria 2022
