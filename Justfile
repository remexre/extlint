all: test

static_flags = "--target x86_64-unknown-linux-musl"

build:
	cargo build --all --release
build-static:
	cargo build --all {{static_flags}} --release

build-all: build build-static

test:
	cargo test --all --release
test-static:
	cargo test --all {{static_flags}} --release

package: build
	tar czvf extlint.tar.gz \
		target/release/get-gitgrade-repos \
		target/release/json_of_ocaml
package-static: build-static
	tar czvf extlint.tar.gz \
		target/x86_64-unknown-linux-musl/release/get-gitgrade-repos \
		target/x86_64-unknown-linux-musl/release/json_of_ocaml

test-on-previous-class CLASS: build
	@mkdir -p test-data/{{CLASS}}
	tar -zxf /class/grades/Spring-2018/csci2041/extlint-test-data/{{CLASS}}.tar.gz -C test-data/{{CLASS}}
	find test-data/{{CLASS}}/repo-score100 -name '*.ml' -exec target/release/json_of_ocaml -s {} \;
test-on-student-repos: build
	target/release/get-gitgrade-repos test-data/repos --grading-scripts test-data/grading-scripts
	find test-data/repos -name '*.ml' -exec target/release/json_of_ocaml -s {} \;

unprivileged-tests: test test-static
privileged-tests: test-on-student-repos
	just test-on-previous-class S17

# This target is used for CI builds; you probably don't want to call it yourself!
travis-ci:
	opam init
	eval `opam config env`
	opam switch 4.06.0+musl+static+flambda
	eval `opam config env`
	opam install ocamlfind ocaml-compiler-libs
	just build-static test-static package-static
