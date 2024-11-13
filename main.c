#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdnoreturn.h>

noreturn void exit(int);
#include <stdio.h>

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
#if defined(_WIN64)
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
// expr_distr_internal when it buidls the tree from the bottom so that it is a
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
		// NOTE: G is also a somewhat reserved name. I could pre-allocate a
		// special node that is only introduced only via the function that
		// introduces inner products in the tree. This combined with numerical
		// IDs should be solid enough.
		if (node->name == 'X') panic(ERROR_BAD_DATA);
		return node->name; // We should allow only upper case letters
	case KIND_VAR:   return 'X';
	case KIND_ADD:   return '+';
	case KIND_MUL:   return ' ';
	case KIND_TRANS: return '\'';
	case KIND_DIFF:  return 'd';
	case KIND_INNER: return ':';
	}
	panic(ERROR_BAD_ARG);
}

static void
expr_print_internal(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return;
	}

	int node_precedence = kind_precedence(node->kind);
	int arg0_precedence = kind_precedence(expr_get_node(node->arg0)->kind);
	int arg1_precedence = kind_precedence(expr_get_node(node->arg1)->kind);
	bool arg0_print_parenthesis = arg0_precedence > node_precedence;
	bool arg1_print_parenthesis = arg1_precedence > node_precedence;

	if (arg0_print_parenthesis) {
		printf("(");
	}
	expr_print_internal(node->arg0);
	if (arg0_print_parenthesis) {
		printf(")");
	}
	printf("%c", expr_char(handle));
	if (arg1_print_parenthesis) {
		printf("(");
	}
	expr_print_internal(node->arg1);
	if (arg1_print_parenthesis) {
		printf(")");
	}
}

static void
expr_print(ExprHandle handle) {
	expr_print_internal(handle);
	printf("\n");
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

static bool
expr_structural_equal(ExprHandle arg0, ExprHandle arg1) {
	const ExprNode *arg0_node = expr_get_node(arg0);
	const ExprNode *arg1_node = expr_get_node(arg1);
	if (!expr_is_valid(arg0) && !expr_is_valid(arg1)) {
		return true;
	} else if (arg0_node->kind != arg1_node->kind) {
		return false;
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
	if (node->kind == KIND_MUL) {
		if (arg0_req_grad && arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_MUL, arg0_der, expr_copy(node->arg1)),
				expr_make_operator(KIND_MUL, expr_copy(node->arg0), arg1_der)
			);
		}
		if (arg0_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_MUL, arg0_der, expr_copy(node->arg1));
		}
		if (arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_MUL, expr_copy(node->arg0), arg1_der);
		}
	}
	if (node->kind == KIND_DIFF) {
		// Differential nodes should only be applied to variables node.
		panic(ERROR_BAD_DATA);
	}
	// TODO: merge with the multiplication branch
	if (node->kind == KIND_INNER) {
		if (arg0_req_grad && arg1_req_grad) {
			// TODO: implement this.
			// TODO: since we hace introduced scalars a unification algorithm
			// for matrix dimensions is needed.
			// d(X:Y) = dX:Y+X:dY
			panic(ERROR_BAD_DATA);
		}
		if (arg0_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_INNER, arg0_der, expr_copy(node->arg1));
		}
		if (arg1_req_grad) {
			*req_grad = true;
			return expr_make_operator(KIND_INNER, expr_copy(node->arg0), arg1_der);
		}
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

// Sicuramente andrà fatto un un while finché non si può più raggruppare.
// Problema 1. ordinare le sequenze di nodi addizione
// Problema 2. espressioni come X + X vanno raggruppate come X (I + I)?
static ExprHandle
expr_groupby_internal(ExprHandle handle, bool *changed) {
	// Assimiamo la forma normale distribuita?
	// F' E' G' C':dX+F' E' G' X':dX+X' F' E' G':dX+B' F' E' G':dX
	// (F' E' G' C'+F' E' G' X'+X' F' E' G'+B' F' E' G'):dX
	// ((C G E F)'+(X G E F)'+(G E F X)'+(G E F B)'):dX
	// (C G E F+X G E F+G E F X+G E F B)':dX
	// ((C+X) G E F+G E F (X+B))':dX
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
					expr_groupby_internal(arg0_node->arg0, changed),
					expr_groupby_internal(arg1_node->arg0, changed)
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
						expr_groupby_internal(arg0_node->arg1, changed),
						expr_groupby_internal(arg1_node->arg1, changed))
				);
			}
			if (expr_structural_equal(arg0_node->arg1, arg1_node->arg1)) {
				*changed = true;
				return expr_make_operator(prod_kind,
					expr_make_operator(KIND_ADD,
						expr_groupby_internal(arg0_node->arg0, changed),
						expr_groupby_internal(arg1_node->arg0, changed)
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
					expr_groupby_internal(arg1_node->arg0, changed),
					expr_groupby_internal(arg0_node->arg0, changed)
				)
			);
		}
	}

	if (node->kind == KIND_CONST || node->kind == KIND_VAR || node->kind == KIND_DIFF) {
		return expr_copy(handle);
	}

	// FIXME: Here the differetial looses (if we do not check before) the argument but why???
	return expr_make_operator(node->kind,
		expr_groupby_internal(node->arg0, changed),
		expr_groupby_internal(node->arg1, changed)
	);
}

static ExprHandle
expr_groupby(ExprHandle handle) {
	bool changed = true;
	ExprHandle res = HANDLE_NULL, old_res = handle;
	while (changed) {
		changed = false;
		res = expr_groupby_internal(old_res, &changed);
		printf("\t"); expr_print(res);
		// TODO: free old_res
		old_res = res;
	}
	return res;
}

// TODO: write a pool allocator for the nodes, this, by making a new copy every
// time, is the simplest way handle memory management for now.

// TODO: print graphviz?
// TODO: stampare stringa per verificare il risultato su https://matrixcalculus.org

int main(int argc, char const *argv[]) {
	// printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	ExprHandle lhs = expr_make_operator(KIND_ADD,
		expr_make_operand(KIND_VAR),
		expr_make_operand(KIND_CONST, 'B')
	);
	ExprHandle rhs = expr_make_operator(KIND_ADD,
		expr_make_operand(KIND_CONST, 'C'),
		expr_make_operand(KIND_VAR)
	);
	ExprHandle res = expr_make_operator(KIND_MUL,
		expr_make_operator(KIND_MUL,
			expr_make_operand(KIND_CONST, 'E'),
			expr_make_operand(KIND_CONST, 'F')
		),
		expr_make_operator(KIND_MUL, lhs, rhs)
	);
	res = expr_make_operator(KIND_INNER,
		expr_make_operand(KIND_CONST, 'G'),
		expr_make_operator(KIND_TRANS, res)
	); expr_print(res); expr_stat(res);
	res = expr_differentiate(res); expr_print(res); expr_stat(res);
	res = expr_distr(res); expr_print(res); expr_stat(res);
	res = expr_expose_differentials(res); expr_print(res); expr_stat(res);
	res = expr_distr(res); expr_print(res); expr_stat(res);
	res = expr_groupby(res); expr_print(res); expr_stat(res);

	// TODO: implement the grouping algorithm (assuming distributive normal
	// form?)

	// For grouping commutative operations like the addition and Hadamard's
	// product requires sorting the operand. Grouping is the opposite of
	// distribution so the set of rules I have to care about are the same (just
	// inverted.) So we have grouping for transpose and addition and
	// multiplication and groupping for the two multiplications (inner and
	// matrix) w.r.t addition.

	// TODO: transpose interact with the inner product in the following ways
	// (A:B)' => A:B and A':B' => A:B

	return 0;
}