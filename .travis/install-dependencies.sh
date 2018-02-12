#!/bin/sh

set -eu

curl https://sh.rustup.rs -sSf | sh -s -- -y
export PATH="$HOME/.cargo/bin:$PATH"
rustup target add x86_64-unknown-linux-musl

pacman -Sy --noconfirm base-devel clang musl opam

cargo install just

opam init -a
opam switch 4.06.0+musl+static+flambda
eval `opam config env`
opam install ocamlfind ocaml-compiler-libs
