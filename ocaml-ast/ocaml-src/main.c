#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

/**
 * Tries to initialize OCaml, using the given executable name.
 *
 * Takes ownership of exec_name if 0 is returned.
 *
 * Returns 0 if initialization occurred, or 1 if it did not. Initialization
 * does not occur if it already has in the past.
 */
int ocaml_ast_init(char* exec_name) {
	static atomic_flag already_inited = ATOMIC_FLAG_INIT;
	if(!atomic_flag_test_and_set(&already_inited)) {
		char** argv = malloc(2 * sizeof(*argv));
		argv[0] = exec_name;
		argv[1] = NULL;
		caml_startup(argv);
		return 0;
	}
	return 1;
}

/**
 * The values (logically) returned by ocaml_ast_parse. They're not literally
 * returned, though, since that's harder to FFI.
 */
typedef union {
	/**
	 * This is an owned pointer.
	 */
	char* success;

	/**
	 * This is a static pointer.
	 */
	const char* missing_value;
} out_data;

/**
 * Tries to parse the given string.
 *
 * Borrows src and path (that is, the caller frees src and path). src and
 * out_data must be non-null, path may be null.
 *
 * Writes to out_data.
 * Returns the discriminant for out.
 */
int ocaml_ast_parse(const char* src, const char* path, out_data* out_data) {
	CAMLparam0();
	CAMLlocal3(oSrc, oPath, oTree);

	oSrc = caml_copy_string(src);
	oPath = caml_copy_string(path ? path : "");

	value* oParse = caml_named_value("parse");
	if(!oParse) {
		out_data->missing_value = "parse";
		CAMLreturnT(int, 1);
	}

	oTree = caml_callback2(*oParse, oSrc, oPath);

	char* tree = String_val(oTree);
	size_t len = strlen(tree);
	char* out = malloc(len + 1);
	out[len] = '\0';
	out_data->success = strncpy(out, tree, len);
	CAMLreturnT(int, 0);
}
