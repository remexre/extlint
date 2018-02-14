cargo_static_flags = "--target x86_64-unknown-linux-musl"

build:
	cargo build --all --release
build-static:
	cargo build --all {{cargo_static_flags}} --release

build-all: build build-static

clean:
	cargo clean
	if test -d dist; then rm -r dist; fi
	if test -d test-data; then rm -rf test-data; fi
	if test -f extlint.tar.gz; then rm extlint.tar.gz; fi

test:
	cargo test --all --release
test-static:
	cargo test --all {{cargo_static_flags}} --release

package: build
	mkdir -p dist/bin
	cp target/release/get-gitgrade-repos dist/bin/get-gitgrade-repos
	cp target/release/json_of_ocaml dist/bin/json_of_ocaml
	tar -czf extlint.tar.gz -C dist bin
package-static: build-static
	mkdir -p dist/bin
	install -s target/x86_64-unknown-linux-musl/release/get-gitgrade-repos dist/bin
	install -s target/x86_64-unknown-linux-musl/release/json_of_ocaml dist/bin
	tar -czf extlint.tar.gz -C dist bin

test-on-previous-class CLASS: build
	@mkdir -p test-data/{{CLASS}}
	tar -zxf /class/grades/Spring-2018/csci2041/extlint-test-data/{{CLASS}}.tar.gz -C test-data/{{CLASS}}
	find test-data/{{CLASS}}/repo-score100 -name '*.ml' -exec target/release/json_of_ocaml -s {} \;
test-on-student-repos: build
	target/release/get-gitgrade-repos test-data/repos --grading-scripts test-data/grading-scripts
	find test-data/repos -name '*.ml' -exec target/release/json_of_ocaml -s {} \;
privileged-tests: test-on-student-repos
	just test-on-previous-class S17
