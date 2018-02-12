# extlint

An extensible linter for OCaml programs.
Designed to be used as part of the grading infrastructure for CSCI2041.

## Building

### Dependencies

Currently, only tested on Linux systems.
May work on macOS, but no guarantees are made.

Requires a working Rust installation, which can be obtained with [rustup](https://rustup.rs/).
Requires an installation of OCaml, OPAM, findlib, and ocaml-compiler-libs.
Requires a C compiler.

[just](https://github.com/casey/just) is recommended, and will be used in the instructions here.
It is technically optional, however; one can also run the commands in `Justfile` by hand.

If compiling a static executable, requires the `4.06.0+musl+static+flambda` OPAM switch to be installed.
If compiling a dynamic executable, requires the `4.06.0` OPAM switch to be installed.
If the `OPAMSWITCH` environment variable is defined, that switch will be used instead.

### Compiling

TODO

### Installation

TODO

## Code Organization

`get-gitgrade-repos` is a small helper program that clones all the student repos.
This is mainly for testing and assessments.

`json_of_ocaml` is a command-line tool that dumps an OCaml AST as JSON.
It's the best way to interface with `ocaml-ast` without linking directly to it.

`ocaml-ast` is a hybrid Rust-OCaml crate to parse OCaml code.

## Known Bugs

Due to [OCaml Bug #6754](https://caml.inria.fr/mantis/view.php?id=6754), there is a warning about `topdirs.cmi` when building `ocaml-ast`.

## License

Licensed under either of

 * Apache License, Version 2.0, (http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license (http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
