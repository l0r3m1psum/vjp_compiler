#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdnoreturn.h>

noreturn void exit(int);
#include <stdio.h>
size_t strlen(const char *);
int strcmp(const char *, const char *);
void *memset(void *, int, size_t);
void *memswp(void *restrict lhs, void *restrict rhs, size_t count) {
	unsigned char *restrict lhsp = lhs, *restrict rhsp = rhs;
	unsigned char tmp = 0;
	for (size_t i = 0; i < count; i++) {
		tmp = lhsp[i];
		lhsp[i] = rhsp[i];
		rhsp[i] = tmp;
	}
	return lhs;
}
#define COUNTOF(a) (sizeof (a) / sizeof *(a))
#define STRLEN(s) (sizeof (s) - 1)

enum Error {
	ERROR_BAD_ALLOC,
	ERROR_BAD_ARG,
	ERROR_BAD_DATA,
};

noreturn void
panic(enum Error err) {
	// TODO: this should print to stderr.
	// https://learn.microsoft.com/en-us/windows/win32/debug/capturestackbacktrace
	switch (err) {
	case ERROR_BAD_ALLOC:
		printf("unable to allocate memory\n");
	case ERROR_BAD_ARG:
		printf("a bad argument was passed\n");
	case ERROR_BAD_DATA:
		printf("data in a node has violated an invariant\n");
	}
	fflush(stdout);
// We assume that each platform compiles with it compiler for simplicity
#if defined(REPL)
	;
#elif defined(_WIN64)
	// if (IsDebuggerPresent()) DebugBreak();
	__debugbreak();
#elif defined(__APPLE__)
	// https://stackoverflow.com/questions/2200277/
	__builtin_debugtrap();
#elif defined(__linux__)
	// https://stackoverflow.com/questions/3596781/
	__builtin_trap();
#else
#endif
	exit(1);
}

typedef enum Kind {
	KIND_NULL,
	KIND_VAR,
	KIND_CONST,
	KIND_ADD,
	KIND_MUL,
	KIND_TRANS,
	KIND_DIFF,
	KIND_INNER,
} Kind;
#define KIND_COUNT 8
#define CHECK_KIND(n) _Static_assert(KIND_COUNT == (n), \
	"the number of elements in the Kind enumeration has changed")

// The lower the number the higher the precedence (think of it as first, second,
// third, ...)
static int
kind_precedence(Kind kind) {
	CHECK_KIND(8);
	switch (kind) {
	case KIND_NULL:  return 0;
	case KIND_VAR:   return 0;
	case KIND_CONST: return 0;
	case KIND_DIFF:  return 1;
	case KIND_TRANS: return 2;
	case KIND_MUL:   return 3;
	case KIND_INNER: return 4;
	case KIND_ADD:   return 5;
	}
	panic(ERROR_BAD_ARG);
}

typedef struct ExprHandle { uint16_t value; } ExprHandle;
#define HANDLE_MAX_VALUE UINT16_MAX
#define HANDLE_NULL ((ExprHandle){0})

// TODO: use an hash table to be able to use multicharacter names for the
// matrices and use the index in the table as the ID of the node.
// NOTE: there are two functions that need to know where the differential are
// down the tree (i.e. expr_differentiate_internal and
// expr_expose_differentials) the req_grad bit could be added by
// expr_distr_internal when it builds the tree from the bottom so that it is a
// property calculated once and for all (note that the various functions still
// need to propagte this information arround when contructing new nodes.)
typedef struct ExprNode {
	uint8_t kind;
	union { uint8_t name; uint8_t val; };
	ExprHandle arg0;
	ExprHandle arg1;
} ExprNode;
typedef const ExprNode * ExprNodeRef;


static ExprNode node_pool[HANDLE_MAX_VALUE + 1];
static uint16_t node_pool_watermark = 1;
static char str_buf_arg0[COUNTOF(node_pool)*STRLEN("()+()") + 1];
static char str_buf_arg1[COUNTOF(node_pool)*STRLEN("()+()") + 1];
static char diff_var_name = 'X';

static bool trace_execution;

// NOTE: the struture is so small that I could also return it by value...
static ExprNodeRef
expr_get_node(ExprHandle handle) {
	return node_pool + handle.value;
}

static bool
expr_write_internal(ExprHandle handle, char **buf, size_t *len) {
#define TRY_WRITE(c) do { \
			if (*len == 0) return false; \
			else *(*buf)++ = (c), (*len)--; \
		} while (0)

	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) { return true; }

	int node_precedence = kind_precedence(node->kind);
	int arg0_precedence = kind_precedence(expr_get_node(node->arg0)->kind);
	int arg1_precedence = kind_precedence(expr_get_node(node->arg1)->kind);
	bool arg0_print_parenthesis = arg0_precedence > node_precedence;
	bool arg1_print_parenthesis = arg1_precedence > node_precedence;

	if (arg0_print_parenthesis) { TRY_WRITE('('); }
	if (!expr_write_internal(node->arg0, buf, len)) { return false; }
	if (arg0_print_parenthesis) { TRY_WRITE(')'); }

	CHECK_KIND(8);
	switch (node->kind) {
	case KIND_NULL:  TRY_WRITE('\0'); break;
	// NOTE: We should allow only upper case letters
	case KIND_VAR:   TRY_WRITE(node->name); break;
	case KIND_CONST: {
		uint8_t val = node->val;
		char digits_buf[3] = {0}, *digits_ptr = digits_buf;
		static_assert(UINT8_MAX == 255, "");
		do {
			*digits_ptr++ = val%(uint8_t)10 + (uint8_t)'0';
		} while ((val /= (uint8_t)10));
		while (digits_ptr-- != digits_buf) {
			assert(digits_buf <= digits_ptr);
			TRY_WRITE(*digits_ptr);
		}
		TRY_WRITE('.');
		TRY_WRITE('I');
	}; break;
	case KIND_ADD:   TRY_WRITE('+'); break;
	case KIND_MUL:   TRY_WRITE(' '); break;
	case KIND_TRANS: TRY_WRITE('\''); break;
	case KIND_DIFF:  TRY_WRITE('d'); break;
	case KIND_INNER: TRY_WRITE(':'); break;
	}

	if (arg1_print_parenthesis) { TRY_WRITE('('); }
	if (!expr_write_internal(node->arg1, buf, len)) { return false; }
	if (arg1_print_parenthesis) { TRY_WRITE(')'); }
	return true;
#undef TRY_WRITE
}

static bool
expr_write(ExprHandle handle, char *buf, size_t len) {
	char *orig_buf = buf;
	size_t orig_len = len;
	if (!expr_write_internal(handle, &buf, &len) || len == 0) {
		return false;
	}
	*buf = 0;
	size_t write_len = buf - orig_buf;
	assert(strlen(orig_buf) == write_len);
	assert(write_len < orig_len); // For the null terminator.
	// TODO: return struct OptionalSize { bool nothing; size_t just; }
	return true;
}

static void
expr_print(ExprHandle handle) {
	// NOTE: Is there any way to divide the tree in chunks to make the the print
	// just a loop of expr_write over the chunk of this tree? I guess that this
	// can be done by saving some sort of context of where the recursion stopped
	// and then resume the recursion from an empty buffer (or just suck it up
	// and write again a print function...)
	bool ok = expr_write(handle, str_buf_arg0, sizeof str_buf_arg0);
	assert(ok);
	printf("%s\n", str_buf_arg0);
}

// Basically pointer equality
static bool
expr_is_equal(ExprHandle arg0, ExprHandle arg1) {
	return arg0.value == arg1.value;
}

static bool
expr_is_valid(ExprHandle handle) {
	return handle.value != HANDLE_NULL.value;
}

// TODO: find a way to order commutative operators.
// NOTE: This function can probably be implemented recursivelly (without doing
// string comparisons) going down two tree like expr_write does but waiting for
// the recursion to "sync-up".
static bool
expr_structural_equal(ExprHandle arg0, ExprHandle arg1) {
	bool ok = false;
	ok = expr_write(arg0, str_buf_arg0, sizeof str_buf_arg0); assert(ok);
	ok = expr_write(arg1, str_buf_arg1, sizeof str_buf_arg1); assert(ok);
	return strcmp(str_buf_arg0, str_buf_arg1) == 0;
}

static ExprHandle
expr_make_node(Kind kind, uint8_t name_or_val, ExprHandle arg0, ExprHandle arg1) {
	if (node_pool_watermark == HANDLE_MAX_VALUE) {
		panic(ERROR_BAD_ALLOC);
	}
	bool both_valid = expr_is_valid(arg0) && expr_is_valid(arg1);
	if (both_valid && expr_is_equal(arg0, arg1)) {
		panic(ERROR_BAD_ARG);
	}
	ExprHandle res = (ExprHandle){node_pool_watermark++};
	if (both_valid && (expr_is_equal(arg0, res) || expr_is_equal(arg1, res))) {
		panic(ERROR_BAD_ARG);
	}
	node_pool[res.value] = (ExprNode){
		.kind = kind,
		.name = name_or_val,
		.arg0 = arg0,
		.arg1 = arg1,
	};
	return res;
}

static ExprHandle
expr_make_operand(Kind kind, uint8_t name_or_val) {
	if (kind != KIND_VAR && kind != KIND_CONST) {
		panic(ERROR_BAD_ARG);
	}
	return expr_make_node(kind, name_or_val, HANDLE_NULL, HANDLE_NULL);
}

static ExprHandle
expr_make_operator(Kind kind, ExprHandle arg0, ExprHandle arg1) {
	if (kind != KIND_ADD
		&& kind != KIND_MUL
		&& kind != KIND_TRANS
		&& kind != KIND_DIFF
		&& kind != KIND_INNER) {
		panic(ERROR_BAD_ARG);
	}
	if (!expr_is_valid(arg0)) panic(ERROR_BAD_ARG);
	if (kind != KIND_TRANS && kind != KIND_DIFF) {
		if (!expr_is_valid(arg1)) panic(ERROR_BAD_ARG);
	} else {
		if (expr_is_valid(arg1)) panic(ERROR_BAD_ARG);
	}
	if (kind == KIND_DIFF) {
		// This is done to make the print function work seamlessly with unary
		// operators that have to appear on the left of their argument. Think of
		// arg0 as the lhs and arg1 as the rhs of a binary operator.
		memswp(&arg0, &arg1, sizeof arg0);
	}
	return expr_make_node(kind, '\0', arg0, arg1);
}

static ExprHandle
expr_copy(ExprHandle handle) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}
	ExprHandle arg0_copy = expr_copy(node->arg0);
	ExprHandle arg1_copy = expr_copy(node->arg1);
	return expr_make_node(node->kind, node->name, arg0_copy, arg1_copy);
}

static ExprHandle
expr_differentiate_internal(ExprHandle handle, bool *req_grad) {
	ExprNodeRef node = expr_get_node(handle);

	CHECK_KIND(8);
	if (node->kind == KIND_NULL) {
		*req_grad = false;
		return HANDLE_NULL;
	}
	if (node->kind == KIND_VAR) {
		if (node->name == diff_var_name) {
			*req_grad = true;
			return expr_make_operator(KIND_DIFF, expr_copy(handle), HANDLE_NULL);
		}
		*req_grad = false;
		return HANDLE_NULL;
	}
	if (node->kind == KIND_CONST) {
		*req_grad = false;
		return HANDLE_NULL;
	}

	bool arg0_req_grad = false;
	bool arg1_req_grad = false;
	ExprHandle arg0_der = expr_differentiate_internal(node->arg0, &arg0_req_grad);
	ExprHandle arg1_der = expr_differentiate_internal(node->arg1, &arg1_req_grad);
	assert(arg0_req_grad == expr_is_valid(arg0_der));
	assert(arg1_req_grad == expr_is_valid(arg1_der));
	if (!arg0_req_grad && !arg1_req_grad) {
		// TODO: free arg0_der and arg1_der
		return *req_grad = false, HANDLE_NULL;
	}

	if (node->kind == KIND_TRANS) {
		*req_grad = true;
		return expr_make_operator(KIND_TRANS, arg0_der, HANDLE_NULL);
	}
	if (node->kind == KIND_ADD) {
		if (arg0_req_grad && arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_ADD, arg0_der, arg1_der);
		}
		if (arg0_req_grad) {
			*req_grad = true;
			return arg0_der;
		}
		if (arg1_req_grad) {
			*req_grad = true;
			return arg1_der;
		}
	}
	if (node->kind == KIND_MUL || node->kind == KIND_INNER) {
		Kind prod_kind = node->kind;
		// TODO: since we hace introduced scalars a unification algorithm
		// for matrix dimensions is needed.
		if (arg0_req_grad && arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(prod_kind, arg0_der, expr_copy(node->arg1)),
				expr_make_operator(prod_kind, expr_copy(node->arg0), arg1_der)
			);
		}
		if (arg0_req_grad) {
			*req_grad = true;
			return expr_make_operator(prod_kind, arg0_der, expr_copy(node->arg1));
		}
		if (arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(prod_kind, expr_copy(node->arg0), arg1_der);
		}
	}
	if (node->kind == KIND_DIFF) {
		// Differential nodes should only be applied to the variable we are
		// differentiating w.r.t and only be applied by this function.
		panic(ERROR_BAD_DATA);
	}

	panic(ERROR_BAD_DATA);
}

static ExprHandle
expr_differentiate(ExprHandle handle) {
	bool req_grad = true;
	ExprHandle res = expr_differentiate_internal(handle, &req_grad);
	if (!expr_is_valid(res)) {
		// Instead of returning just 0.I we return 0.I:dX because in this way
		// expr_expose_differentials and other functions can be left unchanged.
		res = expr_make_operator(KIND_INNER,
			expr_make_operand(KIND_CONST, 0),
			expr_make_operator(KIND_DIFF,
				expr_make_operand(KIND_VAR, diff_var_name),
				HANDLE_NULL
			)
		);
	}
	return res;
}

// NOTE: this function could distribute differentials but we would then need to
// introduce contant zeros and eventually remove them from the tree... Can this
// function be fused with expr_differentiate so that we avoid the creations of
// constant zeros for differential while we distribute the other operations?
// If so we could just distribute expressions like "d(G:F(x))".
// TODO: check that differentials, if present, is applied to the correct
// variable directly.
static ExprHandle
expr_distr_internal(ExprHandle handle, bool *changed) {
	ExprNodeRef node = expr_get_node(handle);
	ExprNodeRef arg0_node = expr_get_node(node->arg0);
	ExprNodeRef arg1_node = expr_get_node(node->arg1);

	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}

	// Transpose interact with the inner product and the constant in the
	// following ways
	// (A:B)' => A:B
	// A':B'  => A:B
	//  c.I'  => c.I
	// We say that they are transpose invariant i.e. we can consider the
	// transpose to be applied to them or not.
	// TODO: when transpose interacts with any of this two operations remove it
	// and take advantage of this property when factoring.

	if (node->kind == KIND_TRANS) {
		if (arg0_node->kind == KIND_TRANS) {
			// (A')' => A
			*changed = true;
			return expr_distr_internal(arg0_node->arg0, changed);
		}
		if (arg0_node->kind == KIND_ADD) {
			// (A+B)' => A' + B'
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_TRANS,
					expr_distr_internal(arg0_node->arg0, changed),
					HANDLE_NULL
				), expr_make_operator(KIND_TRANS,
					expr_distr_internal(arg0_node->arg1, changed),
					HANDLE_NULL
				)
			);
		}
		if (arg0_node->kind == KIND_MUL) {
			// (A B)' => B'A'
			*changed = true;
			return expr_make_operator(KIND_MUL,
				expr_make_operator(KIND_TRANS,
					expr_distr_internal(arg0_node->arg1, changed),
					HANDLE_NULL
				), expr_make_operator(KIND_TRANS,
					expr_distr_internal(arg0_node->arg0, changed),
					HANDLE_NULL
				)
			);
		}
	}

	if (node->kind == KIND_MUL || node->kind == KIND_INNER) {
		Kind prod_kind = node->kind;
		bool arg0_is_add = arg0_node->kind == KIND_ADD;
		bool arg1_is_add = arg1_node->kind == KIND_ADD;
		if (arg0_is_add && arg1_is_add) {
			// Up to commutative property it is equivalent to multiply starting
			// from the left or the right.
			// (A+B) (C+D) => (A+B) C+(A+B) D => A C+B C+A D+B D
			// (A+B):(C+D) => (A+B):C+(A+B):D => A:C+B:C+A:C+B:D
			ExprHandle A = expr_distr_internal(arg0_node->arg0, changed);
			ExprHandle B = expr_distr_internal(arg0_node->arg1, changed);
			ExprHandle C = expr_distr_internal(arg1_node->arg0, changed);
			ExprHandle D = expr_distr_internal(arg1_node->arg1, changed);

			ExprHandle AC = expr_make_operator(prod_kind, A, C);
			ExprHandle BC = expr_make_operator(prod_kind, B, C);
			ExprHandle AD = expr_make_operator(prod_kind, A, D);
			ExprHandle BD = expr_make_operator(prod_kind, B, D);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_ADD, AC, BC),
				expr_make_operator(KIND_ADD, AD, BD)
			);
		}
		if (arg0_is_add) {
			// (A+B) C => A C+B C
			// (A+B):C => A:C+B:C
			ExprHandle C = expr_distr_internal(node->arg1, changed);
			ExprHandle A = expr_distr_internal(arg0_node->arg0, changed);
			ExprHandle B = expr_distr_internal(arg0_node->arg1, changed);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(prod_kind, A, C),
				expr_make_operator(prod_kind, B, C)
			);
		}
		if (arg1_is_add) {
			// A (C+D) => A C+A D
			// A:(C+D) => A:C+A:D
			ExprHandle A = expr_distr_internal(node->arg0, changed);
			ExprHandle C = expr_distr_internal(arg1_node->arg0, changed);
			ExprHandle D = expr_distr_internal(arg1_node->arg1, changed);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(prod_kind, A, C),
				expr_make_operator(prod_kind, A, D)
			);
		}
	}

	if (node->kind == KIND_VAR || node->kind == KIND_CONST) {
		return expr_copy(handle);
	}
	return expr_make_node(node->kind, node->name,
		expr_distr_internal(node->arg0, changed),
		expr_distr_internal(node->arg1, changed)
	);
}

static ExprHandle
expr_distr(ExprHandle handle) {
	bool changed = true;
	ExprHandle res = HANDLE_NULL, old_res = handle;
	while (changed) {
		changed = false;
		res = expr_distr_internal(old_res, &changed);
		if (trace_execution) printf("\t"), expr_print(res);
		// TODO: free old_res
		old_res = res;
	}
	return res;
}

// TODO: to avoid having to remember which unary operations have the argument on
// the left or the right a expr_single_child function could be handy to avoid
// trivial mistakes.

static bool
expr_has_differential(ExprHandle handle) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return false;
	}
	if (node->kind == KIND_DIFF) {
		ExprNodeRef child_node = expr_get_node(node->arg1);
		bool res = child_node->kind == KIND_VAR
			&& child_node->name == diff_var_name;
		return res;
	}
	return expr_has_differential(node->arg0)
		|| expr_has_differential(node->arg1);
}

// TODO: use the DFS trick to avoid having to call expr_has_differential.
static ExprHandle
expr_expose_differentials(ExprHandle handle) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}

	if (node->kind == KIND_ADD) {
		return expr_make_operator(KIND_ADD,
			expr_expose_differentials(node->arg0),
			expr_expose_differentials(node->arg1)
		);
	}

	if (node->kind == KIND_INNER) {
		ExprHandle arg0 = node->arg0, arg1 = node->arg1;
		bool diff_on_arg0 = expr_has_differential(arg0);
		bool diff_on_arg1 = expr_has_differential(arg1);
		if (diff_on_arg0 && diff_on_arg1 || !diff_on_arg0 && !diff_on_arg1)
			panic(ERROR_BAD_DATA);
		// By convention we want the differential to be on the rhs.
		if (expr_has_differential(arg0)) {
			memswp(&arg0, &arg1, sizeof arg0);
		}
		ExprHandle A = expr_copy(arg0);
		ExprNodeRef arg1_node = expr_get_node(HANDLE_NULL);
		while (arg1_node = expr_get_node(arg1), arg1_node->kind != KIND_DIFF) {
			ExprHandle B = arg1_node->arg0;
			ExprHandle C = arg1_node->arg1;
			if (arg1_node->kind == KIND_MUL) {
				bool diff_on_B = expr_has_differential(B);
				bool diff_on_C = expr_has_differential(C);
				if (diff_on_B && diff_on_C) panic(ERROR_BAD_DATA);
				if (diff_on_B) {
					// A:B C => B' A:C
					A = expr_make_operator(KIND_MUL,
						A,
						expr_make_operator(KIND_TRANS, expr_copy(C), HANDLE_NULL)
					);
					arg1 = B;
				} else {
					// A:B C => A C':B
					A = expr_make_operator(KIND_MUL,
						expr_make_operator(KIND_TRANS, expr_copy(B), HANDLE_NULL),
						A
					);
					arg1 = C;
				}
			} else if (arg1_node->kind == KIND_TRANS) {
				// A:B' => A':B
				A = expr_make_operator(KIND_TRANS, A, HANDLE_NULL);
				arg1 = B;
			} else goto panic;
		}
		assert(arg1_node->kind == KIND_DIFF);
		if (expr_get_node(arg1_node->arg1)->name != diff_var_name)
			panic(ERROR_BAD_DATA);
		return A;
	}

panic:
	// The expression needs to be in "distributed normal form" and contain a
	// differentiated variable in each inner product.
	panic(ERROR_BAD_DATA);
}

static ExprHandle
expr_normalize_addend_internal(ExprHandle handle, uint8_t *count) {
	ExprNodeRef node = expr_get_node(handle);
	ExprHandle lhs = HANDLE_NULL, rhs = HANDLE_NULL;

	if (node->kind == KIND_MUL) lhs = expr_normalize_addend_internal(node->arg0, count);
	if (node->kind != KIND_MUL) {
		ExprHandle myhandle = handle;
		ExprNodeRef mynode = node;
		bool has_trans = node->kind == KIND_TRANS;
		if (has_trans) myhandle = node->arg0, mynode = expr_get_node(myhandle);

		assert(mynode->kind == KIND_CONST || mynode->kind == KIND_VAR);
		if (mynode->kind == KIND_CONST) {
			*count *= mynode->val;
			return HANDLE_NULL;
		} else {
			ExprHandle myhandlecopy = expr_copy(myhandle);
			return has_trans
				? expr_make_operator(KIND_TRANS, myhandlecopy, HANDLE_NULL)
				: myhandlecopy;
		}
	}
	if (node->kind == KIND_MUL) rhs = expr_normalize_addend_internal(node->arg1, count);

	bool lhs_is_valid = expr_is_valid(lhs), rhs_is_valid = expr_is_valid(rhs);
	if (!lhs_is_valid && !rhs_is_valid) return HANDLE_NULL;
	if (!lhs_is_valid && rhs_is_valid)  return rhs;
	if (lhs_is_valid  && !rhs_is_valid) return lhs;
	if (lhs_is_valid  && rhs_is_valid)  return expr_make_operator(KIND_MUL, lhs, rhs);
	assert(false);
}

static ExprHandle
expr_with_count(ExprHandle handle, uint8_t count) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_CONST) return expr_copy(handle);
	if (count == 0 || node->kind == KIND_NULL) return expr_make_operand(KIND_CONST, 0);
	return count > 1
		? expr_make_operator(KIND_MUL,
			expr_make_operand(KIND_CONST, count),
			handle)
		: handle;
}

static void
expr_accumulate_internal(ExprHandle handle, ExprHandle *prev, uint8_t *count,
	ExprHandle *append) {
	ExprNodeRef node = expr_get_node(handle);

	if (node->kind == KIND_ADD) expr_accumulate_internal(node->arg0, prev, count, append);
	if (node->kind != KIND_ADD) {
		uint8_t addend_count = 1;
		ExprHandle normalized_handle = expr_normalize_addend_internal(handle, &addend_count);
		if (addend_count == 0) {
			return;
		}
		if (!expr_is_valid(normalized_handle)) {
			normalized_handle = expr_make_operand(KIND_CONST, addend_count);
		}
		ExprHandle new_handle = normalized_handle;

		if (expr_structural_equal(new_handle, *prev)) {
			*count += addend_count;
		} else { // append with count
			if (expr_is_valid(*prev)) {
				if (expr_is_valid(*append)) {
					*append = expr_make_operator(KIND_ADD,
						*append,
						expr_with_count(*prev, *count)
					);
				} else {
					*append = expr_with_count(*prev, *count);
				}
			}
			*count = addend_count;
		}
		*prev = new_handle;
		return;
	}
	if (node->kind == KIND_ADD) expr_accumulate_internal(node->arg1, prev, count, append);
}

static ExprHandle
expr_accumulate(ExprHandle handle) {
	if (!expr_is_valid(handle)) {
		return HANDLE_NULL;
	}
	ExprHandle prev = HANDLE_NULL, res = HANDLE_NULL;
	uint8_t count = 1;
	expr_accumulate_internal(handle, &prev, &count, &res);
	if (expr_is_valid(res)) {
		assert(expr_is_valid(prev));
		res = expr_make_operator(KIND_ADD,
			res,
			expr_with_count(prev, count)
		);
	} else {
		res = expr_with_count(prev, count);
	}
	return res;
}

static ExprHandle
expr_factor_internal(ExprHandle handle, bool *changed) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}

	ExprNodeRef arg0_node = expr_get_node(node->arg0);
	ExprNodeRef arg1_node = expr_get_node(node->arg1);
	Kind arg0_kind = arg0_node->kind;
	Kind arg1_kind = arg1_node->kind;
	if (node->kind == KIND_ADD) {
		// A'+B' => (A+B)'
		if (arg0_node->kind == KIND_TRANS && arg1_node->kind == KIND_TRANS) {
			*changed = true;
			return expr_make_operator(KIND_TRANS,
				expr_make_operator(KIND_ADD,
					expr_factor_internal(arg0_node->arg0, changed),
					expr_factor_internal(arg1_node->arg0, changed)
				),
				HANDLE_NULL
			);
		}
		if (arg0_kind == arg1_kind && (arg0_kind == KIND_MUL || arg1_kind == KIND_INNER)) {
			Kind prod_kind = arg0_kind;
			// A B+A C => A (B+C)
			// A:B+A:C => A:(B+C)
			if (expr_structural_equal(arg0_node->arg0, arg1_node->arg0)) {
				*changed = true;
				return expr_make_operator(prod_kind,
					expr_copy(arg0_node->arg0),
					expr_make_operator(KIND_ADD,
						expr_factor_internal(arg0_node->arg1, changed),
						expr_factor_internal(arg1_node->arg1, changed))
				);
			}
			// B A+C A => (B+C) A
			// B:A+C:A => (B+C):A
			if (expr_structural_equal(arg0_node->arg1, arg1_node->arg1)) {
				*changed = true;
				return expr_make_operator(prod_kind,
					expr_make_operator(KIND_ADD,
						expr_factor_internal(arg0_node->arg0, changed),
						expr_factor_internal(arg1_node->arg0, changed)
					),
					expr_copy(arg0_node->arg1)
				);
			}
		}
	}
	if (node->kind == KIND_MUL) {
		// A' B' => (B A)'
		if (arg0_node->kind == KIND_TRANS && arg1_node->kind == KIND_TRANS) {
			*changed = true;
			return expr_make_operator(KIND_TRANS,
				expr_make_operator(KIND_MUL,
					expr_factor_internal(arg1_node->arg0, changed),
					expr_factor_internal(arg0_node->arg0, changed)
				),
				HANDLE_NULL
			);
		}
	}

	if (node->kind == KIND_VAR || node->kind == KIND_CONST) {
		return expr_copy(handle);
	}

	return expr_make_node(node->kind, node->name,
		expr_factor_internal(node->arg0, changed),
		expr_factor_internal(node->arg1, changed)
	);
}

// Best effort factoring, it does not support integer factoring.
static ExprHandle
expr_factor(ExprHandle handle) {
	bool changed = true;
	ExprHandle res = HANDLE_NULL, old_res = handle;
	while (changed) {
		changed = false;
		res = expr_factor_internal(old_res, &changed);
		if (trace_execution) printf("\t"), expr_print(res);
		// TODO: free old_res
		old_res = res;
	}
	return res;
}

/* Parser *********************************************************************/

typedef struct ParserState {
	const char *tokens;
	size_t lentgh;
	size_t current;
} ParserState;

static bool
ParserState_is_at_end(const ParserState *state) {
	return state->lentgh == state->current;
}

static char
ParserState_match(ParserState *state, char type) {
	// NOTE: if we always pass 0 terminated strings this check is unnecessary
	// and we can just unconditionally return peek since '\0' should never be a
	// type to match.
	if (ParserState_is_at_end(state)) return '\0';
	char peek = state->tokens[state->current];
	if (peek == type) {
		state->current++;
		return peek;
	}
	return '\0';
}

/* The grammar to parse:
 *     term         -> factor ("+" factor)*;
 *     factor       -> transpose (" " transpose)*;
 *     transpose    -> differential "'"*;
 *     differential -> "d"? primary;
 *     primary      -> variable | number.I | "(" term ")";
 *     variable     -> [A-HJ-Z];
 *     number       -> 0|[1-9][0-9]*;
 */
static ExprHandle Grammar_term         (ParserState *);
static ExprHandle Grammar_inner        (ParserState *);
static ExprHandle Grammar_factor       (ParserState *);
static ExprHandle Grammar_transpose    (ParserState *);
static ExprHandle Grammar_differential (ParserState *);
static ExprHandle Grammar_primary      (ParserState *);

static ExprHandle
Grammar_term(ParserState *state) {
	ExprHandle expr = Grammar_inner(state);
	while (ParserState_match(state, '+')) {
		ExprHandle right = Grammar_inner(state);
		expr = expr_make_operator(KIND_ADD, expr, right);
	}
	return expr;
}

static ExprHandle
Grammar_inner(ParserState *state) {
	ExprHandle expr = Grammar_factor(state);
	while (ParserState_match(state, ':')) {
		ExprHandle right = Grammar_factor(state);
		expr = expr_make_operator(KIND_INNER, expr, right);
	}
	return expr;
}

static ExprHandle
Grammar_factor(ParserState *state) {
	ExprHandle expr = Grammar_transpose(state);
	while (ParserState_match(state, ' ')) {
		ExprHandle right = Grammar_transpose(state);
		expr = expr_make_operator(KIND_MUL, expr, right);
	}
	return expr;
}

static ExprHandle
Grammar_transpose(ParserState *state) {
	ExprHandle expr = Grammar_differential(state);
	while (ParserState_match(state, '\'')) {
		expr = expr_make_operator(KIND_TRANS, expr, HANDLE_NULL);
	}
	return expr;
}

static ExprHandle
Grammar_differential(ParserState *state) {
	bool has_diff = ParserState_match(state, 'd');
	ExprHandle expr = Grammar_primary(state);
	if (has_diff) {
		expr = expr_make_operator(KIND_DIFF, expr, HANDLE_NULL);
	}
	return expr;
}

static ExprHandle
Grammar_primary(ParserState *state) {
	for (char letter = 'A'; letter <= 'Z'; letter++) {
		if (letter == 'I') continue;
		if (ParserState_match(state, letter)) {
			return expr_make_operand(KIND_VAR, letter);
		}
	}
	if (ParserState_match(state, '0')) {
		if (ParserState_match(state, '.')
			&& ParserState_match(state, 'I')) {
			return expr_make_operand(KIND_CONST, 0);
		}
		panic(ERROR_BAD_DATA);
	}
	for (char nonzero_digit = '1'; nonzero_digit <= '9'; nonzero_digit++) {
		uint8_t match = 0;
		uint8_t val = 0;
		if ((match = ParserState_match(state, nonzero_digit))) {
			val = val*((uint8_t) 10) + match - (uint8_t)'0';
			for (char digit = '0'; digit <= '9'; digit++) {
				while ((match = ParserState_match(state, digit))) {
					val = val*((uint8_t) 10) + match - (uint8_t)'0';
				}
			}
			if (ParserState_match(state, '.')
				&& ParserState_match(state, 'I')) {
				return expr_make_operand(KIND_CONST, val);
			}
			panic(ERROR_BAD_DATA);
		}
	}
	if (ParserState_match(state, '(')) {
		ExprHandle expr = Grammar_term(state);
		if (!ParserState_match(state, ')')) {
			panic(ERROR_BAD_DATA);
		}
		return expr;
	}
	panic(ERROR_BAD_DATA);
}

static ExprHandle
expr_parse(const char *expr) {
	ParserState state = {.tokens = expr, .lentgh = strlen(expr)};
	return Grammar_term(&state);
}

/* Debug **********************************************************************/

static void
expr_graphviz_internal(ExprHandle handle) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return;
	}

	{
#define TRY_WRITE(c) do { label_buf[label_len++] = (c); } while (0)
		int label_len = 0;
		char label_buf[8] = {0};
		CHECK_KIND(8);
		switch (node->kind) {
		case KIND_NULL:  TRY_WRITE('\0'); break;
		// NOTE: We should allow only upper case letters
		case KIND_VAR:   TRY_WRITE(node->name); break;
		case KIND_CONST: {
			uint8_t val = node->val;
			char digits_buf[3] = {0}, *digits_ptr = digits_buf;
			static_assert(UINT8_MAX == 255, "");
			do {
				*digits_ptr++ = val%(uint8_t)10 + (uint8_t)'0';
			} while ((val /= (uint8_t)10));
			while (digits_ptr-- != digits_buf) {
				assert(digits_buf <= digits_ptr);
				TRY_WRITE(*digits_ptr);
			}
			TRY_WRITE('.');
			TRY_WRITE('I');
		}; break;
		case KIND_ADD:   TRY_WRITE('+'); break;
		case KIND_MUL:   TRY_WRITE(' '); break;
		case KIND_TRANS: TRY_WRITE('\''); break;
		case KIND_DIFF:  TRY_WRITE('d'); break;
		case KIND_INNER: TRY_WRITE(':'); break;
		}
		printf("\tn%05d [label=\"%s\"];\n", handle.value, label_buf);
#undef TRY_WRITE
	}
	if (expr_is_valid(node->arg0)) {
		printf("\tn%05d -> n%05d;\n", handle.value, node->arg0.value);
		expr_graphviz_internal(node->arg0);
	}
	if (expr_is_valid(node->arg1)) {
		printf("\tn%05d -> n%05d;\n", handle.value, node->arg1.value);
		expr_graphviz_internal(node->arg1);
	}
}

// https://forum.graphviz.org/t/binary-tree-force-lonely-node-to-be-left-or-right/1159
static void
expr_graphviz(ExprHandle handle) {
	bool ok = expr_write(handle, str_buf_arg0, sizeof str_buf_arg0);
	assert(ok);
	printf("digraph debuggraph {\n");
	printf("\tlabel=\"%s\";\n", str_buf_arg0);
	expr_graphviz_internal(handle);
	printf("}\n");
}

static void
expr_stat_internal(ExprHandle handle, uint16_t *operator_count, uint16_t *operand_count) {
	ExprNodeRef node = expr_get_node(handle);
	CHECK_KIND(8);
	switch (node->kind) {
	case KIND_NULL:
		break;
	case KIND_CONST:
	case KIND_VAR:
		(*operand_count)++;
		break;
	case KIND_MUL:
	case KIND_ADD:
	case KIND_INNER:
		expr_stat_internal(node->arg0, operator_count, operand_count);
		expr_stat_internal(node->arg1, operator_count, operand_count);
		(*operator_count)++;
		break;
	case KIND_TRANS:
		expr_stat_internal(node->arg0, operator_count, operand_count);
		(*operator_count)++;
		break;
	case KIND_DIFF:
		expr_stat_internal(node->arg1, operator_count, operand_count);
		(*operator_count)++;
		break;
	}
	// TODO: duplicated count? This requires a hash table.
	return;
}

static void
expr_stat(ExprHandle handle) {
	uint16_t operator_count = 0, operand_count = 0;
	expr_stat_internal(handle, &operator_count, &operand_count);
	uint32_t sum = operator_count + operand_count;
	printf("#operand=%d #operator=%d sum=%d\n", operand_count, operator_count, sum);
}

static void
expr_print_matrixcalculus_internal(ExprHandle handle) {
	ExprNodeRef node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return;
	}

	bool node_is_inner = node->kind == KIND_INNER;

	int node_precedence = kind_precedence(node_is_inner ? KIND_MUL : node->kind);
	int arg0_precedence = kind_precedence(expr_get_node(node->arg0)->kind);
	int arg1_precedence = kind_precedence(expr_get_node(node->arg1)->kind);
	int node_or_trans_precedence = node_is_inner ? kind_precedence(KIND_TRANS) : node_precedence;
	bool arg0_print_parenthesis = arg0_precedence > node_or_trans_precedence;
	bool arg1_print_parenthesis = arg1_precedence > node_precedence;

	if (node_is_inner) printf("tr(");
		if (arg0_print_parenthesis) printf("(");
		expr_print_matrixcalculus_internal(node->arg0);
		if (arg0_print_parenthesis) printf(")");
	if (node_is_inner) printf("'");

	CHECK_KIND(8);
	switch (node->kind) {
	case KIND_NULL:  assert(false); break; // This shuld never happen...
	// NOTE: We should allow only upper case letters
	case KIND_VAR:   printf("%c", node->name); break;
	case KIND_CONST: printf("%d*", node->val); break;
	case KIND_ADD:   printf("+"); break;
	case KIND_INNER:
	case KIND_MUL:
		             printf("*"); break;
	case KIND_TRANS: printf("\'"); break;
	case KIND_DIFF:  panic(ERROR_BAD_ARG); break;
	}

	if (node_is_inner) {}
		if (arg1_print_parenthesis) printf("(");
		expr_print_matrixcalculus_internal(node->arg1);
		if (arg1_print_parenthesis) printf(")");
	if (node_is_inner) printf(")");
}

static void
expr_print_matrixcalculus(ExprHandle handle) {
	expr_print_matrixcalculus_internal(handle);
	printf("\n");
}

/******************************************************************************/

static ExprHandle
expr_derivative(ExprHandle handle) {
	ExprHandle res = handle;
#define V if (trace_execution)
	// TODO: inline some of the functions here...

	V printf("Steps for the derivation of\n");
	V expr_print(res);
	V printf("Equivalent expression to check result on https://www.matrixcalculus.org\n");
	V expr_print_matrixcalculus(res);

	V printf("Step 1. Differential application;\n");
	res = expr_differentiate(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 2. Distribution;\n");
	res = expr_distr(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 3. Bring out differentials;\n");
	res = expr_expose_differentials(res);
	V { expr_print(res); expr_stat(res); }

	// TODO: implement sorting.
	// TODO: mettere expr_graphviz nel repl di debug...

	V printf("Step 4. Accumulation.\n");
	res = expr_distr(res);
	res = expr_accumulate(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 5. Factorization.\n");
	// FIXME: not invariant to associativity.
	res = expr_factor(res);
	V { expr_print(res); expr_stat(res); }

	return res;
#undef V
}

// TODO: write a pool allocator for the nodes, this, by making a new copy every
// time, is the simplest way handle memory management for now.
// https://www.gingerbill.org/article/2019/02/16/memory-allocation-strategies-004/

static bool
test_derivative(const char *expr, const char *res) {
	ExprHandle lhs = expr_derivative(expr_parse(expr));
	ExprHandle rhs = expr_parse(res);
	bool ok = expr_structural_equal(lhs, rhs);
	if (!ok) {
		printf("Derivative of %s is not %s but is\n", expr, res);
		expr_print(lhs);
	}
	return ok;
}

int
main(int argc, char const *argv[]) {
#ifdef REPL
	// TODO: print the ^D description on POSIX systems.
	// TODO: test that memory does not leak with a long for loop.
	printf(
		"Press '^Z + Enter' on a line by itself to exit.\n"
		"This is an example expression \"G:(E F (X+B) (C+X))'\" from which you\n"
		"can infer the syntax for the expressions.\n"
	);
	ExprHandle res = HANDLE_NULL;
	char str_buf[256] = {0};
	trace_execution = true;
	while (printf("vJp> "), fgets(str_buf, sizeof str_buf, stdin) == str_buf) {
		// TODO: emit a warning if there really is any input left.
		fseek(stdin, 0, SEEK_END);
		res = expr_parse(str_buf);
		// TODO: check that the expression is parsed entirely with ParserState_is_at_end
		res = expr_derivative(res);
		memset(node_pool, 0, sizeof node_pool);
		node_pool_watermark = 1;
	}
	if (ferror(stdin)) {
		return 1;
	}
	return 0;
#elif defined(TEST)
	trace_execution = false;
	int num_bad = 0;
	num_bad += !test_derivative("G:(E F (X+B) (C+X))'", "((C+X) G E F+G E F (X+B))'");
	num_bad += !test_derivative("G:(X B+C (X D))", "(G B'+C' G D')");
	num_bad += !test_derivative("X:G", "G");
	num_bad += !test_derivative("X:10.I", "10.I");
	num_bad += !test_derivative("A", "0.I");
	num_bad += !test_derivative("A:G", "0.I");
	num_bad += !test_derivative("A:G+A:G", "0.I");
	num_bad += !test_derivative("G:(10.I+X)", "G");
	num_bad += !test_derivative("G:(X+X)", "2.I G");
	num_bad += !test_derivative("G:(A X+A X)", "2.I A G");
	num_bad += !test_derivative(
		"(A 5.I B'+(A B' 6.I'+C 0.I G)+3.I A' 2.I+3.I 4.I W+2.I' 3.I):X",
		"11.I A B'+6.I A'+12.I W+6.I");
	num_bad += !test_derivative("((A C+B A)+(B A+B A)+B+B):X", "A C+3.I B A+2.I B");
	// num_bad += !test_derivative("", ""); // TODO: test parse empty string
	// TODO: testing for errors now is not really possible. To make it feasible
	// "error nodes" should be pre allocated in the node_pool and make them
	// point to themselves, in this way any self pointg node is considered as a
	// terminal and HANDLE_NULL is a terminal that contains "error success".
	// num_bad += !test_error("dX:dX");
	// num_bad += !test_error("A:dX dX");
	// num_bad += !test_structural_equal("A+(B+C)", "(A+B)+C");
	{
		const char *lhs_str = "A+(B+C)";
		const char *rhs_str = "(A+B)+C";
		ExprHandle lhs = expr_parse(lhs_str);
		ExprHandle rhs = expr_parse(rhs_str);
		bool ok = expr_structural_equal(lhs, rhs);
		if (!ok) {
			printf("%s is not structurally equal to %s\n", lhs_str, rhs_str);
		}
		num_bad += !ok;
	}
	if (!num_bad) {
		printf("OK!\n");
	}
	return num_bad;
#else
	printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	// Queste due espressioni sono equivalenti ma www.matrixcalculus.org ritorna
	// due espressioni sintatticamente diverse!
	// tr(G'*(X+6*inv(I)*I)*(X-5*inv(I)*I))
	// tr(G'*(X*X + 6*inv(I)*I*X - X*5*inv(I)*I - 30*inv(I)*I))
	// Un altro caso:
	// tr(G'*((C+X)*G*E*F+G*E*F*(X+B))')
	// Matrixcalculus supports fractions but it does not do simplifications
	// tr(G'*(matrix(5.5)+X))

	return 0;
#endif
}
