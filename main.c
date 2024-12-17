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

// TODO: Add support for constants of the matrix multiplication ring. Note that
// this may add some complication to the algorithm, especially for factoring
// integer. Given the fact that factoring integer is rarely going to be used we
// are going to implement this feature later.
typedef enum Kind {
	KIND_NULL,
	KIND_VAR,
	KIND_ADD,
	KIND_MUL,
	KIND_TRANS,
	KIND_DIFF,
	KIND_INNER,
} Kind;
#define KIND_COUNT 7
#define CHECK_KIND(n) _Static_assert(KIND_COUNT == (n), \
	"the number of elements in the Kind enumeration has changed")

// The lower the number the higher the precedence (think of it as first, second,
// third, ...)
static int
kind_precedence(Kind kind) {
	CHECK_KIND(7);
	switch (kind) {
	case KIND_NULL:  return 0;
	case KIND_VAR:   return 0;
	case KIND_ADD:   return 5;
	case KIND_MUL:   return 3;
	case KIND_TRANS: return 2;
	case KIND_DIFF:  return 1;
	case KIND_INNER: return 4;
	}
	panic(ERROR_BAD_ARG);
}

typedef struct ExprHandle { uint16_t value; } ExprHandle;
#define HANDLE_MAX_VALUE UINT16_MAX
#define HANDLE_NULL ((ExprHandle){0})

// NOTE: For debuging it would be better to have a string instead of a single
// character, also probably generating names automatically like A000, A001,
// ecc... Would make things more ergonomic. If names are generated automatically
// a numerical ID shall be used instead.
// NOTE: there are two functions that need to know where the differential are
// down the tree (i.e. expr_differentiate_internal and
// expr_expose_differentials) the req_grad bit could be added by
// expr_distr_internal when it builds the tree from the bottom so that it is a
// property calculated once and for all (note that the various functions still
// need to propagte this information arround when contructing new nodes.)
typedef struct ExprNode {
	uint8_t kind;
	char name;
	// char padding[1];
	ExprHandle arg0;
	ExprHandle arg1;
} ExprNode;

static ExprNode node_pool[HANDLE_MAX_VALUE + 1];
static uint16_t node_pool_watermark = 1;
static char str_buf_arg0[COUNTOF(node_pool)*STRLEN("()+()") + 1];
static char str_buf_arg1[COUNTOF(node_pool)*STRLEN("()+()") + 1];
static char diff_var_name = 'X';

static bool trace_execution;

// NOTE: the struture is so small that I could also return it by value...
static const ExprNode *
expr_get_node(ExprHandle handle) {
	return node_pool + handle.value;
}

static char
expr_char(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	CHECK_KIND(7);
	switch (node->kind) {
	case KIND_NULL:  return '\0';
	// NOTE: We should allow only upper case letters
	case KIND_VAR:   return node->name;
	case KIND_ADD:   return '+';
	case KIND_MUL:   return ' ';
	case KIND_TRANS: return '\'';
	case KIND_DIFF:  return 'd';
	case KIND_INNER: return ':';
	}
	panic(ERROR_BAD_ARG);
}

static bool
expr_write_internal(ExprHandle handle, char **buf, size_t *len) {
#define TRY_WRITE(c) do { \
			if (*len == 0) return false; \
			else *(*buf)++ = (c), (*len)--; \
		} while (0)

	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return true;
	}

	int node_precedence = kind_precedence(node->kind);
	int arg0_precedence = kind_precedence(expr_get_node(node->arg0)->kind);
	int arg1_precedence = kind_precedence(expr_get_node(node->arg1)->kind);
	bool arg0_print_parenthesis = arg0_precedence > node_precedence;
	bool arg1_print_parenthesis = arg1_precedence > node_precedence;

	if (arg0_print_parenthesis) {
		TRY_WRITE('(');
	}
	if (!expr_write_internal(node->arg0, buf, len)) {
		return false;
	}
	if (arg0_print_parenthesis) {
		TRY_WRITE(')');
	}

	TRY_WRITE(expr_char(handle));

	if (arg1_print_parenthesis) {
		TRY_WRITE('(');
	}
	if (!expr_write_internal(node->arg1, buf, len)) {
		return false;
	}
	if (arg1_print_parenthesis) {
		TRY_WRITE(')');
	}
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
	return !expr_is_equal(handle, HANDLE_NULL);
}

// TODO: find a way to order commutative operators.
static bool
expr_structural_equal(ExprHandle arg0, ExprHandle arg1) {
	bool ok = false;
	ok = expr_write(arg0, str_buf_arg0, sizeof str_buf_arg0); assert(ok);
	ok = expr_write(arg1, str_buf_arg1, sizeof str_buf_arg1); assert(ok);
	return strcmp(str_buf_arg0, str_buf_arg1) == 0;
}

static ExprHandle
expr_make_node(Kind kind, char name, ExprHandle arg0, ExprHandle arg1) {
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
		.name = name,
		.arg0 = arg0,
		.arg1 = arg1,
	};
	return res;
}

static ExprHandle
expr_make_operand(char name) {
	return expr_make_node(KIND_VAR, name, HANDLE_NULL, HANDLE_NULL);
}

static ExprHandle
expr_make_operator(Kind kind, ExprHandle arg0, ...) {
	if (kind != KIND_ADD
		&& kind != KIND_MUL
		&& kind != KIND_TRANS
		&& kind != KIND_DIFF
		&& kind != KIND_INNER) {
		panic(ERROR_BAD_ARG);
	}
	ExprHandle arg1 = HANDLE_NULL;
	if (kind != KIND_TRANS && kind != KIND_DIFF) {
		va_list args;
		va_start(args, arg0);
		arg1 = va_arg(args, ExprHandle);
		va_end(args);
	} else if (kind == KIND_DIFF) {
		// This is done to make the print function work seamlessly with unary
		// operators that have to appear on the left of their argument. Think of
		// arg0 as the lhs and arg1 as the rhs of a binary operator.
		memswp(&arg0, &arg1, sizeof arg0);
	}
	return expr_make_node(kind, '\0', arg0, arg1);
}

static ExprHandle
expr_copy(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}
	ExprHandle arg0_copy = expr_copy(node->arg0);
	ExprHandle arg1_copy = expr_copy(node->arg1);
	return expr_make_node(node->kind, node->name, arg0_copy, arg1_copy);
}

// TODO: this function should return a constant 0 if it does not find any
// variable to differentiare w.r.t.
static ExprHandle
expr_differentiate_internal(ExprHandle handle, bool *req_grad) {
	const ExprNode *node = expr_get_node(handle);

	CHECK_KIND(7);
	if (node->kind == KIND_NULL) {
		*req_grad = false;
		return HANDLE_NULL;
	}
	if (node->kind == KIND_VAR) {
		if (node->name == diff_var_name) {
			*req_grad = true;
			return expr_make_operator(KIND_DIFF, expr_copy(handle));
		}
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
		return expr_make_operator(KIND_TRANS, arg0_der);
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
	return expr_differentiate_internal(handle, &req_grad);
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
	const ExprNode *node = expr_get_node(handle);
	const ExprNode *arg0_node = expr_get_node(node->arg0);
	const ExprNode *arg1_node = expr_get_node(node->arg1);

	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}

	// TODO: transpose interact with the inner product in the following ways
	// (A:B)' => A:B and A':B' => A:B
	// Can this break factoring later? I should think of a test case for this
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
				expr_make_operator(KIND_TRANS, expr_distr_internal(arg0_node->arg0, changed)),
				expr_make_operator(KIND_TRANS, expr_distr_internal(arg0_node->arg1, changed))
			);
		}
		if (arg0_node->kind == KIND_MUL) {
			// (A B)' => B'A'
			*changed = true;
			return expr_make_operator(KIND_MUL,
				expr_make_operator(KIND_TRANS, expr_distr_internal(arg0_node->arg1, changed)),
				expr_make_operator(KIND_TRANS, expr_distr_internal(arg0_node->arg0, changed))
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

	if (node->kind == KIND_VAR) {
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

static void
expr_stat_internal(ExprHandle handle, uint16_t *operator_count, uint16_t *operand_count) {
	const ExprNode *node = expr_get_node(handle);
	CHECK_KIND(7);
	switch (node->kind) {
	case KIND_NULL:
		break;
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

// TODO: to avoid having to remember which unary operations have the argument on
// the left or the right a expr_get_argument function could be handy to avoid
// trivial mistakes.

static bool
expr_has_differential(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return false;
	}
	if (node->kind == KIND_DIFF) {
		return true;
	}
	return expr_has_differential(node->arg0)
		|| expr_has_differential(node->arg1);
}

// TODO: use the DFS trick to avoid having to call expr_has_differential.
// NOTE: this function requires a distribution round afterwards for the nested
// transpositions. It could be applied every time we move an operation from the
// other side of the inner product to reduce the number of copies of the entire
// tree afterwards.
static ExprHandle
expr_expose_differentials(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
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
		const ExprNode *arg1_node = NULL;
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
						expr_make_operator(KIND_TRANS, expr_copy(C))
					);
					arg1 = B;
				} else {
					// A:B C => A C':B
					A = expr_make_operator(KIND_MUL,
						expr_make_operator(KIND_TRANS, expr_copy(B)),
						A
					);
					arg1 = C;
				}
			} else if (arg1_node->kind == KIND_TRANS) {
				// A:B' => A':B
				A = expr_make_operator(KIND_TRANS, A);
				arg1 = B;
			} else goto panic;
		}
		assert(arg1_node->kind == KIND_DIFF);
		if (expr_get_node(arg1_node->arg1)->name != diff_var_name)
			panic(ERROR_BAD_DATA);
		ExprHandle new_arg1 = expr_copy(arg1);
		// A:dX
		return expr_make_operator(KIND_INNER, A, new_arg1);
	}

panic:
	// The expression needs to be in "distributed normal form" and contain a
	// differentiated variable in each inner product.
	panic(ERROR_BAD_DATA);
}

static ExprHandle
expr_factor_internal(ExprHandle handle, bool *changed) {
	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}

	const ExprNode *arg0_node = expr_get_node(node->arg0);
	const ExprNode *arg1_node = expr_get_node(node->arg1);
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
				)
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
				)
			);
		}
	}

	if (node->kind == KIND_VAR) {
		return expr_copy(handle);
	}

	return expr_make_node(node->kind, node->name,
		expr_factor_internal(node->arg0, changed),
		expr_factor_internal(node->arg1, changed)
	);
}

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

typedef struct ParserState {
	const char *tokens;
	size_t lentgh;
	size_t current;
} ParserState;

static bool
ParserState_is_at_end(const ParserState *state) {
	return state->lentgh == state->current;
}

static char ParserState_match(ParserState *state, char type) {
	// NOTE: in we always pass 0 terminated strings this check is unnecessary
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
 *     primary      -> VAR | "(" term ")";
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
		expr = expr_make_operator(KIND_TRANS, expr);
	}
	return expr;
}

static ExprHandle
Grammar_differential(ParserState *state) {
	bool has_diff = ParserState_match(state, 'd');
	ExprHandle expr = Grammar_primary(state);
	if (has_diff) {
		expr = expr_make_operator(KIND_DIFF, expr);
	}
	return expr;
}

static ExprHandle
Grammar_primary(ParserState *state) {
	for (char letter = 'A'; letter <= 'Z'; letter++) {
		if (ParserState_match(state, letter)) {
			return expr_make_operand(letter);
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

static void
expr_print_matrixcalculus_internal(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
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

	if (node_is_inner || node->kind == KIND_MUL) printf("*");
	else printf("%c", expr_char(handle));

	if (node_is_inner) ;
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

static ExprHandle
expr_derivative(ExprHandle handle) {
	ExprHandle res = handle;
#define V if (trace_execution)

	V printf("Steps for the derivation of\n");
	V expr_print(res);
	V printf("Equivalent expression to check result on https://www.matrixcalculus.org\n");
	V expr_print_matrixcalculus(handle);

	V printf("Step 1. Differential application;\n");
	res = expr_differentiate(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 2. Distribution;\n");
	res = expr_distr(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 3. Bring out differentials;\n");
	res = expr_expose_differentials(res);
	V { expr_print(res); expr_stat(res); }

	V printf("Step 4. Factorization.\n");
	res = expr_distr(res);
	res = expr_factor(res);
	V { expr_print(res); expr_stat(res); }

	const ExprNode *node = expr_get_node(res);
	// A(X):dX
	assert(
		node->kind == KIND_INNER
			? expr_get_node(node->arg1)->kind == KIND_DIFF
			: node->kind == KIND_NULL
	);
	res = node->arg0;

	V printf("Result\n");
	V expr_print(res);

	return res;
#undef V
}

// TODO: write a pool allocator for the nodes, this, by making a new copy every
// time, is the simplest way handle memory management for now.
// https://www.gingerbill.org/article/2019/02/16/memory-allocation-strategies-004/

// TODO: print graphviz?
// https://graphviz.org/Gallery/directed/Genetic_Programming.html
// https://forum.graphviz.org/t/binary-tree-force-lonely-node-to-be-left-or-right/1159

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
		// TODO: check that the expression is parsed entirely with
		// ParserState_is_at_end
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
	bool all_ok = true;
	all_ok &= test_derivative("G:(E F (X+B) (C+X))'", "((C+X) G E F+G E F (X+B))'");
	all_ok &= test_derivative("G:(X B+C (X D))", "(G B'+C' G D')");
	all_ok &= test_derivative("X:G", "G");
	// TODO: testing for errors now is not really possible. To make it feasible
	// "error nodes" should be pre allocated in the node_pool and make them
	// point to themselves, in this way any self pointg node is considered as a
	// terminal and HANDLE_NULL is a terminal that contains "error success".
	// all_ok &= test_error("dX:dX");
	// all_ok &= test_error("A:dX dX");
	{
		const char *lhs_str = "A+(B+C)";
		const char *rhs_str = "(A+B)+C";
		ExprHandle lhs = expr_parse(lhs_str);
		ExprHandle rhs = expr_parse(rhs_str);
		bool ok = expr_structural_equal(lhs, rhs);
		if (!ok) {
			printf("%s is not structurally equal to %s\n", lhs_str, rhs_str);
		}
		all_ok &= ok;
	}
	if (all_ok) {
		printf("OK!\n");
		return 0;
	}
	return 1;
#else
	// printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	// Queste due espressioni sono equivalenti ma www.matrixcalculus.org ritorna
	// due espressioni sintatticamente diverse!
	// tr(G'*(X+6*inv(I)*I)*(X-5*inv(I)*I))
	// tr(G'*(X*X + 6*inv(I)*I*X - X*5*inv(I)*I - 30*inv(I)*I))
	// Unaltro caso:
	// tr(G'*((C+X)*G*E*F+G*E*F*(X+B))')

	trace_execution = true;
	ExprHandle res = HANDLE_NULL;
	// FIXME: Since we can't factor for constants we can't find the derivarive
	// of the following two functions.
	res = expr_parse("G:(X+X)"); expr_print(res); expr_stat(res);
	res = expr_parse("G:(A X+A X)"); expr_print(res); expr_stat(res);
	// res = expr_parse("G:(E F (X+B) (C+X))'"); expr_print(res); expr_stat(res);
	// res = expr_parse("G:(X B+C (X D))"); expr_print(res); expr_stat(res);
	res = expr_derivative(res);
	return 0;
#endif
}