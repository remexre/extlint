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
This is tested with OCaml 4.06.0, so it is recommended to use that version.

### Compiling

Run `make`.
This will compile the extlint binaries, creating the same `extlint.tar.gz` bundle as built by Travis.

## Installation

Extract the binaries from `extlint.tar.gz` to somewhere on your `PATH`.

## Code Organization

`get-gitgrade-repos` is a small helper program that clones all the student repos.
This is mainly for testing and assessments.

`ocaml_ast` is an OCaml project for getting an OCaml AST and serializing it.

`pretty-errors` is a Rust crate for printing errors similarly to how `rustc` does.

`rules` contains the Prolog portion of the project, which does the actual linting.
The `extlintMain` predicate, defined in `rules/main.pl`, acts as the main function.
It will output to a JSON file.

`src/extlint_render_error` processes the JSON, adding pretty errors to each lint.

## License

Licensed under either of

 * Apache License, Version 2.0, (http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license (http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
