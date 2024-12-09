#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdnoreturn.h>

noreturn void exit(int);
#include <stdio.h>
size_t strlen(const char *);

#define SWAP(x, y, T) do { T tmp = x; x = y; y = tmp; } while (0)

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
	// https://stackoverflow.com/questions/2200277/detecting-debugger-on-mac-os-x
	__builtin_debugtrap();
#elif defined(__linux__)
	// https://stackoverflow.com/questions/3596781/how-to-detect-if-the-current-process-is-being-run-by-gdb
	__builtin_trap();
#else
#endif
	exit(1);
}

typedef enum Kind {
	KIND_NULL,
	KIND_CONST,
	KIND_VAR,
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
	case KIND_CONST: return 0;
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
	char name;       // To distinguish constants.
	// char padding[1];
	ExprHandle arg0;
	ExprHandle arg1;
} ExprNode;

static ExprNode node_pool[HANDLE_MAX_VALUE + 1];

// NOTE: the struture is so small that I could also return it by value...
static const ExprNode *
expr_get_node(ExprHandle handle) {
	return node_pool + handle.value;
}

static char
expr_char(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	CHECK_KIND(8);
	switch (node->kind) {
	case KIND_NULL:  return '\0';
	case KIND_CONST:
		if (node->name == 'X') panic(ERROR_BAD_DATA);
		return node->name; // NOTE: We should allow only upper case letters
	case KIND_VAR:   return 'X';
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
	static char buf[1024] = {0};
	bool ok = expr_write(handle, buf, sizeof buf);
	if (!ok) buf[1023] = 0;
	printf("%s%s\n", buf, ok ? "" : "...");
}

static uint16_t node_pool_watermark = 1;

// Basically pointer equality
static bool
expr_is_equal(ExprHandle arg0, ExprHandle arg1) {
	return arg0.value == arg1.value;
}

static bool
expr_is_valid(ExprHandle handle) {
	return !expr_is_equal(handle, HANDLE_NULL);
}

// TODO: use the output of expr_write on arg0 and arg1 and compare it with
// int strcmp(const char *, const char *). This avoids problems related to
// associativity.
// TODO: find a way to order commutative operators.
static bool
expr_structural_equal(ExprHandle arg0, ExprHandle arg1) {
	const ExprNode *arg0_node = expr_get_node(arg0);
	const ExprNode *arg1_node = expr_get_node(arg1);
	if (!expr_is_valid(arg0) && !expr_is_valid(arg1)) {
		return true;
	} else if (arg0_node->kind != arg1_node->kind) {
		return false;
	} else if (arg0_node->kind == KIND_VAR || arg0_node->kind == KIND_CONST) {
		return expr_char(arg0) == expr_char(arg1);
	} else {
		return expr_structural_equal(arg0_node->arg0, arg1_node->arg0)
			&& expr_structural_equal(arg0_node->arg1, arg1_node->arg1);
	}
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
expr_make_operand(Kind kind, ...) {
	if (kind != KIND_CONST && kind != KIND_VAR) {
		return HANDLE_NULL;
	}
	char name = 'X';
	if (kind == KIND_CONST) {
		va_list args;
		va_start(args, kind);
		name = va_arg(args, int);
		va_end(args);
	}
	return expr_make_node(kind, name, HANDLE_NULL, HANDLE_NULL);
}

static ExprHandle
expr_make_operator(Kind kind, ExprHandle arg0, ...) {
	if (kind != KIND_ADD
		&& kind != KIND_MUL
		&& kind != KIND_TRANS
		&& kind != KIND_DIFF
		&& kind != KIND_INNER) {
		return HANDLE_NULL;
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
		SWAP(arg0, arg1, ExprHandle);
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

static ExprHandle
expr_differentiate_internal(ExprHandle handle, bool *req_grad) {
	const ExprNode *node = expr_get_node(handle);

	CHECK_KIND(8);
	if (node->kind == KIND_NULL) {
		*req_grad = false;
		return HANDLE_NULL;
	}
	if (node->kind == KIND_CONST) {
		*req_grad = false;
		return HANDLE_NULL;
	}
	if (node->kind == KIND_VAR) {
		*req_grad = true;
		return expr_make_operator(KIND_DIFF, expr_copy(handle));
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
		// Differential nodes should only be applied to variables node.
		panic(ERROR_BAD_DATA);
	}

	panic(ERROR_BAD_DATA);
}

static ExprHandle
expr_differentiate(ExprHandle handle) {
	bool req_grad = true;
	return expr_differentiate_internal(handle, &req_grad);
}

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

	if (node->kind == KIND_CONST || node->kind == KIND_VAR) {
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
		printf("\t"); expr_print(res);
		// TODO: free old_res
		old_res = res;
	}
	return res;
}

static void
expr_stat_internal(ExprHandle handle, uint16_t *operator_count, uint16_t *operand_count) {
	const ExprNode *node = expr_get_node(handle);
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
	// TODO: duplicated count?
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

// TODO: if a node (different from HANDLE_NULL) point to itself it can cause
// function recursive function to stack overflow. This case should be catched
// and reported. Is there any other potential infinite loop (without recursion?)

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

	// NOTE: the algorithm could support the differential being in the left or
	// right argument of the inner product, but for simplicity we are going to
	// assume that it can only be on the right. But the inner product is
	// commutative so it would not be that hard to implement.
	if (node->kind == KIND_INNER) {
		ExprHandle A = expr_copy(node->arg0);
		ExprHandle arg1 = node->arg1;
		const ExprNode *arg1_node = NULL;
		while (arg1_node = expr_get_node(arg1), arg1_node->kind != KIND_DIFF) {
			ExprHandle B = arg1_node->arg0;
			ExprHandle C = arg1_node->arg1;
			if (arg1_node->kind == KIND_MUL) {
				// NOTE: should I check if both branches have a differential and
				// in case panic?
				bool diff_on_the_lhs = expr_has_differential(B);
				if (diff_on_the_lhs) {
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
		ExprHandle new_arg1 = expr_copy(arg1);
		return expr_make_operator(KIND_INNER, A, new_arg1);
	}

panic:
	// The expression needs to be in "distributed normal form" and contain a
	// differentiated variable in each inner product.
	panic(ERROR_BAD_DATA);
}

// If we assume the expression to be in "distributive normal form" the problem
// is one of multivariate polynomial factoring. Grobner basis are a good topic
// to take inspiration from (keeping in mind that matrix multiplication is not
// commutative.) The polynomial need to be ordered in some way so that all
// common factors appear near each other and are easy to factor. There also need
// to be a way to factor expressions like X+X in X(I+I).
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
			if (expr_structural_equal(arg0_node->arg0, arg1_node->arg0)) {
				*changed = true;
				return expr_make_operator(prod_kind,
					expr_copy(arg0_node->arg0),
					expr_make_operator(KIND_ADD,
						expr_factor_internal(arg0_node->arg1, changed),
						expr_factor_internal(arg1_node->arg1, changed))
				);
			}
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

	if (node->kind == KIND_CONST || node->kind == KIND_VAR) {
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
		printf("\t"); expr_print(res);
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
 *     term    -> factor ("+" factor)*;
 *     factor  -> unary (" " unary)*;
 *     unary   -> primary "'"*;
 *     primary -> VAR | "(" term ")";
 */
static ExprHandle Grammar_term    (ParserState *);
static ExprHandle Grammar_inner   (ParserState *);
static ExprHandle Grammar_factor  (ParserState *);
static ExprHandle Grammar_unary   (ParserState *);
static ExprHandle Grammar_primary (ParserState *);

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
	ExprHandle expr = Grammar_unary(state);
	while (ParserState_match(state, ' ')) {
		ExprHandle right = Grammar_unary(state);
		expr = expr_make_operator(KIND_MUL, expr, right);
	}
	return expr;
}

static ExprHandle
Grammar_unary(ParserState *state) {
	ExprHandle expr = Grammar_primary(state);
	while (ParserState_match(state, '\'')) {
		expr = expr_make_operator(KIND_TRANS, expr);
	}
	return expr;
}

static ExprHandle
Grammar_primary(ParserState *state) {
	for (int i = 0; i < 'Z' - 'A'; i++) {
		char letter = 'A'+i;
		if (ParserState_match(state, letter)) {
			return expr_make_operand(letter == 'X' ? KIND_VAR : KIND_CONST, letter);
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

// TODO: add global logging flag...

static ExprHandle
expr_derivative(ExprHandle handle, bool verbose) {
	ExprHandle res = handle;
#define V if (verbose)
#define S V { expr_print(res); expr_stat(res); }
	V printf("Step 1. Differential application;\n");
	res = expr_differentiate(res); S
	V printf("Step 2. Distribution;\n");
	res = expr_distr(res); S
	V printf("Step 3. Bring out differentials;\n");
	res = expr_expose_differentials(res); S
	V printf("Step 4. Factorization.\n");
	res = expr_distr(res); S
	res = expr_factor(res); S
	const ExprNode *node = expr_get_node(res);
	// A(X):dX
	assert(
		node->kind == KIND_INNER
			? expr_get_node(node->arg1)->kind == KIND_DIFF
			: node->kind == KIND_NULL
	);
	return node->arg0;
#undef V
#undef S
}

// Alla luce del fatto che quella che io chiamo forma normale distribuita non è
// atro che un polinomio multivariato, il KIND_VAR è quello di tutte le
// matrici che compaiono finora e quelle che hanno il nome 'X' saranno trattate
// come quelle per le duali differenziamo. Le matrici costanti devono assumere
// il significato diverso di essere una matrice identità moltiplicata per una
// costante intera. expr_differentiate deve essere riscritta per prendere come
// argomento la variabile rispetto al quale differenziare (a cui poi verranno
// applicati i differenziali).

// TODO: write a pool allocator for the nodes, this, by making a new copy every
// time, is the simplest way handle memory management for now.
// https://www.gingerbill.org/article/2019/02/16/memory-allocation-strategies-004/

// TODO: print graphviz?
// TODO: stampare stringa per verificare il risultato su https://matrixcalculus.org

int
main(int argc, char const *argv[]) {
#ifdef REPL
	// TODO: print the ^D description on POSIX systems.
	// TODO: test that memory does not leak with a long for loop.
	// TODO: make a function expr_write che scrive un'espressione come stringa,
	// questo mi permetterà di serializzare un espressione e rendere l'utilizzo
	// di arene più fattibili.
	printf(
		"Press '^Z + Enter' on a line by itself to exit.\n"
		"This is an example expression \"G:(E F (X+B) (C+X))'\" from which you\n"
		"can infer the syntax for the expressions.\n"
	);
	ExprHandle res = HANDLE_NULL;
	char str_buf[256] = {0};
	while (printf("vJp> "), gets_s(str_buf, sizeof str_buf) == str_buf) {
		res = expr_parse(str_buf);            expr_print(res); expr_stat(res);
		// TODO: check that the expression is parsed entirely with
		// ParserState_is_at_end
		res = expr_derivative(res, true);
		memset(node_pool, 0, sizeof node_pool);
		node_pool_watermark = 1;
	}
	if (ferror(stdin)) {
		return 1;
	}
	return 0;
#elif defined(TEST)
	{
		const char *lhs_str = "G:(E F (X+B) (C+X))'";
		const char *rhs_str = "((C+X) G E F+G E F (X+B))'";
		ExprHandle lhs = expr_derivative(expr_parse(lhs_str), false);
		ExprHandle rhs = expr_parse(rhs_str);
		bool ok = expr_structural_equal(lhs, rhs);
		if (!ok) {
			printf("Derivative of %s is not %s but is\n", lhs_str, rhs_str);
			expr_print(lhs);
			return 1;
		}
	}
	{
		const char *lhs_str = "G:(X B+C (X D))";
		const char *rhs_str = "(G B'+C' G D')";
		ExprHandle lhs = expr_derivative(expr_parse(lhs_str), false);
		ExprHandle rhs = expr_parse(rhs_str);
		bool ok = expr_structural_equal(expr_derivative(lhs, false), rhs);
		if (!ok) {
			printf("Derivative of %s is not %s but is\n", lhs_str, rhs_str);
			expr_print(lhs);
			return 1;
		}
	}
	{
		const char *lhs_str = "A+(B+C)";
		const char *rhs_str = "(A+B)+C";
		ExprHandle lhs = expr_parse(lhs_str);
		ExprHandle rhs = expr_parse(rhs_str);
		bool ok = expr_structural_equal(lhs, rhs);
		if (!ok) {
			printf("%s is not structurally equal to %s\n", lhs_str, rhs_str);
			return 1;
		}
	}
	return 0;
#else
	// printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	ExprHandle res = HANDLE_NULL;
	// tr(G'*(E*F*(X+B)*(C+X))')
	res = expr_parse("G:(E F (X+B) (C+X))'"); expr_print(res); expr_stat(res);
	// res = expr_parse("G:(X B+C (X D))"); expr_print(res); expr_stat(res);
	res = expr_derivative(res, true);
	return 0;
#endif
}