#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdnoreturn.h>

noreturn void exit(int);
int printf(const char *, ...);

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
		printf("unable to allocate memory");
	case ERROR_BAD_ARG:
		printf("a bad argument was passed");
	case ERROR_BAD_DATA:
		printf("data in a node has violated an invariant");
	}
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
	case KIND_CONST: return 0;
	case KIND_VAR:   return 0;
	case KIND_ADD:   return 4;
	case KIND_MUL:   return 3;
	case KIND_TRANS: return 2;
	case KIND_DIFF:  return 1;
	}
	panic(ERROR_BAD_ARG);
}

typedef struct ExprHandle { uint16_t value; } ExprHandle;
#define HANDLE_MAX_VALUE UINT16_MAX
#define HANDLE_NULL ((ExprHandle){0})

// NOTE: For debuging it would be better to have a string instead of a single
// character, also probably generating names automatically like A000, A001,
// ecc... Would make things more ergonomic.
typedef struct ExprNode {
	Kind kind;
	char name;       // To distinguish constants.
	char padding[3];
	ExprHandle arg0;
	ExprHandle arg1;
} ExprNode;

static ExprNode node_pool[HANDLE_MAX_VALUE + 1];

static ExprNode *
expr_get_node(ExprHandle handle) {
	return node_pool + handle.value;
}

static char
expr_char(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	CHECK_KIND(7);
	switch (node->kind) {
	case KIND_NULL:  return '\0';
	case KIND_CONST: return node->name; // We should allow only upper case letters
	case KIND_VAR:   return 'X';
	case KIND_ADD:   return '+';
	case KIND_MUL:   return ' ';
	case KIND_TRANS: return '\'';
	case KIND_DIFF:  return 'd';
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

static ExprHandle
expr_make_node(Kind kind, char name, ExprHandle arg0, ExprHandle arg1) {
	if (node_pool_watermark == HANDLE_MAX_VALUE) {
		panic(ERROR_BAD_ALLOC);
	}
	ExprHandle res = (ExprHandle){node_pool_watermark++};
	node_pool[res.value] = (ExprNode) {
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
		&& kind != KIND_DIFF) {
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

static bool
expr_is_valid(ExprHandle handle) {
	return handle.value != HANDLE_NULL.value;
}

static ExprHandle
expr_differentiate_internal(ExprHandle handle, bool *req_grad) {
	ExprNode *node = expr_get_node(handle);

	CHECK_KIND(7);
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

	if (node->kind == KIND_MUL) {
		bool arg0_is_add = arg0_node->kind == KIND_ADD;
		bool arg1_is_add = arg1_node->kind == KIND_ADD;
		if (arg0_is_add && arg1_is_add) {
			// (A+B) (C+D) => (A+B) C+(A+B) D => A C+B C+A D+B D
			ExprHandle A = expr_distr_internal(arg0_node->arg0, changed);
			ExprHandle B = expr_distr_internal(arg0_node->arg1, changed);
			ExprHandle C = expr_distr_internal(arg1_node->arg0, changed);
			ExprHandle D = expr_distr_internal(arg1_node->arg1, changed);

			ExprHandle AC = expr_make_operator(KIND_MUL, A, C);
			ExprHandle BC = expr_make_operator(KIND_MUL, B, C);
			ExprHandle AD = expr_make_operator(KIND_MUL, A, D);
			ExprHandle BD = expr_make_operator(KIND_MUL, B, D);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_ADD, AC, BC),
				expr_make_operator(KIND_ADD, AD, BD)
			);
		}
		if (arg0_is_add) {
			// (A+B) C => A C+B C
			ExprHandle C = expr_distr_internal(node->arg1, changed);
			ExprHandle A = expr_distr_internal(arg0_node->arg0, changed);
			ExprHandle B = expr_distr_internal(arg0_node->arg1, changed);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_MUL, A, C),
				expr_make_operator(KIND_MUL, B, C)
			);
		}
		if (arg1_is_add) {
			// A (C+D) => A C+A D
			ExprHandle A = expr_distr_internal(node->arg0, changed);
			ExprHandle C = expr_distr_internal(arg1_node->arg0, changed);
			ExprHandle D = expr_distr_internal(arg1_node->arg1, changed);
			*changed = true;
			return expr_make_operator(KIND_ADD,
				expr_make_operator(KIND_MUL, A, C),
				expr_make_operator(KIND_MUL, A, D)
			);
		}
	}

	if (node->kind == KIND_CONST || node->kind == KIND_VAR) {
		return expr_copy(handle);
	}
	return expr_make_node(node->kind, node->name,
		expr_distr_internal(node->arg0, changed), expr_distr_internal(node->arg1, changed)
	);
}

static ExprHandle
expr_distr(ExprHandle handle) {
	bool changed = true;
	ExprHandle res = HANDLE_NULL, old_res = handle;
	while (changed) {
		changed = false;
		res = expr_distr_internal(old_res, &changed);
		// TODO: free old_res
		old_res = res;
	}
	return res;
}

static void
expr_stat_internal(ExprHandle handle, uint16_t *operator_count, uint16_t *operand_count) {
	ExprNode *node = expr_get_node(handle);
	CHECK_KIND(7);
	switch (node->kind) {
	case KIND_NULL:
		break;
	case KIND_CONST:
	case KIND_VAR:
		(*operand_count)++;
		break;
	case KIND_MUL:
	case KIND_ADD:
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
	printf("#operand=%d #operator=%d\n", operand_count, operator_count);
}

// TODO: write a pool allocator for the nodes, this, by making a new copy every
// time, is the simplest way handle memory management for now.

int main(int argc, char const *argv[]) {
	printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	ExprHandle lhs = expr_make_operator(
		KIND_ADD, 
		expr_make_operand(KIND_VAR),
		expr_make_operand(KIND_CONST, 'B')
	);
	ExprHandle rhs = expr_make_operator(
		KIND_ADD,
		expr_make_operand(KIND_CONST, 'C'),
		expr_make_operand(KIND_VAR)
	);
	ExprHandle res = expr_make_operator(
		KIND_MUL,
		expr_make_operator(
			KIND_MUL,
			expr_make_operand(KIND_CONST, 'E'),
			expr_make_operand(KIND_CONST, 'F')
		),
		expr_make_operator(KIND_MUL, lhs, rhs)
	);
	res = expr_make_operator(KIND_TRANS, res); expr_print(res); expr_stat(res);
	res = expr_differentiate(res); expr_print(res); expr_stat(res);
	res = expr_distr(res); expr_print(res); expr_stat(res);
	return 0;
}