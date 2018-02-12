#!/bin/sh

set -eu

export PATH="$HOME/.cargo/bin:$PATH"
eval `opam config env`
just test-static package-static
