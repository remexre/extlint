all: extlint.tar.gz
clean:
	cargo clean
	if test -d dist; then rm -r dist; fi
	if test -d test-data; then rm -rf test-data; fi
	if test -f extlint.tar.gz; then rm extlint.tar.gz; fi
test:
	cargo test --all
.PHONY: all clean test

extlint.tar.gz: ocaml_ast/main.native target/release/extlint_render_error target/release/get-gitgrade-repos
	mkdir -p dist/bin
	install ocaml_ast/main.native               dist/bin/datalog_of_ocaml
	install ocaml_ast/main.native               dist/bin/json_of_ocaml
	install target/release/extlint_render_error dist/bin/extlint_render_error
	install target/release/get-gitgrade-repos   dist/bin/get-gitgrade-repos
	tar -czf extlint.tar.gz -C dist bin

ocaml_ast/main.native:
	$(MAKE) -C ocaml_ast main.native
.PHONY: ocaml_ast/main.native
target/release/% target/release/%.d:
	cargo build --all --release

include target/release/extlint_render_error.d
