# 0.8

- added a `try_` combinator to try a decoder without changing the state
- refactoring of the library

# 0.7

- Mentioned in the documentation that the support for partial parsing
  of S-expressions is currently not well supported
- Added example tests that were raised in bug reports and in a
  `discuss.ocaml.org` thread
- Added a functorized interface, so that the library is independent
  from the actual definition of S-expressions (fixes issue #1)

# 0.6

- Fixes to opam package description
- Added the fields and fields_advanced combinator
- Minor changes to documentation

# 0.3

- Fixes to opam package description
- Improved documentation
- Added changelog
- Added todo list
- Added combinator ``first``
- Added combinator ``record`` for decoding records
- Added combinator ``record_advanced`` for decoding records while
  imposing that some fields are mandatory or must not have multiple
  occurrences
- Fixed combinator ``peek``
- Added inline tests

# 0.2

- Fixes to opam package description
- Improved documentation

# 0.1

- Initial release
