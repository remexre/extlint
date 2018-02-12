#!/bin/sh

set -eu

export PATH="$HOME/.cargo/bin:$PATH"
eval `opam config env`
just package-static
