# extlint

[![Build Status](https://travis-ci.org/remexre/extlint.svg?branch=master)](https://travis-ci.org/remexre/extlint)
[![Dependency Status](https://deps.rs/repo/github/remexre/extlint/status.svg)](https://deps.rs/repo/github/remexre/extlint)

An extensible linter for OCaml programs.
Designed to be used as part of the grading infrastructure for CSCI2041.

Currently, only tested on Linux systems.
May work on macOS, but no guarantees are made.

## Obtaining

Tagged releases are automatically built as static executables by [Travis CI](https://travis-ci.org/remexre/extlint), and uploaded to this repo's [releases page](https://github.com/remexre/extlint/releases).
Alternatively, the project can be built from source with the below instructions.

## Building from Source

### Dependencies

Requires a working Rust installation, which can be obtained with [rustup](https://rustup.rs/).
Requires an installation of OCaml, OPAM, and ocamlfind.
Requires a C compiler.

[just](https://github.com/casey/just) is recommended, and will be used in the instructions here.
It is technically optional, however; one can also run the commands in `Justfile` by hand.

If compiling a static executable, requires the `4.06.0+musl+static+flambda` OPAM switch to be installed.
If compiling a dynamic executable, requires the `4.06.0` OPAM switch to be installed.
If the `OPAMSWITCH` environment variable is defined, that switch will be used instead.

### Compiling

Run `just package` or `just package-static`.
This will compile `extlint` and its tools, and create the same `extlint.tar.gz` bundle as built by Travis.

## Installation

Extract the binaries from `extlint.tar.gz` to somewhere on your `PATH`.

## Code Organization

`get-gitgrade-repos` is a small helper program that clones all the student repos.
This is mainly for testing and assessments.

`ocaml-ast` is a hybrid Rust-OCaml crate to parse OCaml code.

`rules` contains rules developed for linting.

`src/json_of_ocaml` is a command-line tool that dumps an OCaml untyped AST as JSON.
It's the best way to interface with `ocaml-ast` without linking directly to it.

`src/ocaml-syntax-check` is a command-line tool that pretty-prints any syntax error in the given file.
It's mainly for debugging.

## Known Bugs

Due to [OCaml Bug #6754](https://caml.inria.fr/mantis/view.php?id=6754), there is a warning about `topdirs.cmi` when building `ocaml-ast`.

## License

Licensed under either of

 * Apache License, Version 2.0, (http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license (http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
