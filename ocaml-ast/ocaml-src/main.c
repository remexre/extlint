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
	 *
	 * This variant has a discriminant of 0.
	 */
	char* success;

	/**
	 * This is a static pointer.
	 *
	 * This variant has a discriminant of 1.
	 */
	const char* missing_value;
} out_data;

/**
 * Tries to parse the given string.
 *
 * Borrows src and path (that is, the caller frees src and path). src and
 * out_data must be non-null, path may be null. src_len and path_len are the
 * length of src and path in bytes, not including the null terminator. If path
 * is null, path_len must also be null.
 *
 * Writes to out_data.
 * Returns the discriminant for out.
 */
int ocaml_ast_parse(const char* src, size_t src_len, const char* path, size_t path_len, out_data* out_data) {
	// Set up the GC roots.
	CAMLparam0();
	CAMLlocal3(oSrc, oPath, oTree);

	// Copy src and path into the OCaml GC heap.
	oSrc = caml_alloc_initialized_string(src_len, src);
	if(path) {
		oPath = caml_alloc_initialized_string(path_len, path);
	} else {
		oPath = caml_alloc_string(0);
	}

	// Get the parse function (declared in ocaml_ast.ml).
	value* oParse = caml_named_value("parse");
	if(!oParse) {
		out_data->missing_value = "parse";
		CAMLreturnT(int, 1);
	}

	// Call the parse function with src and path as arguments.
	oTree = caml_callback2(*oParse, oSrc, oPath);

	// Convert the return value from the parse function into a C string.
	char* tree = String_val(oTree);
	size_t len = strlen(tree);
	char* out = malloc(len + 1);
	out[len] = '\0';
	out_data->success = strncpy(out, tree, len);
	CAMLreturnT(int, 0);
}
