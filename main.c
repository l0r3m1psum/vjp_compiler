#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>

int printf(const char *, ...);

#define SWAP(T, x, y) do { T tmp = x; x = y; y = tmp; } while (0)

typedef enum Kind {
	KIND_NULL,
	KIND_CONST,
	KIND_VAR,
	KIND_ADD,
	KIND_MUL,
	KIND_TRANS,
} Kind;
#define KIND_COUNT 6
#define CHECK_KIND(n) static_assert(KIND_COUNT == (n), \
	"the number of elements in the Kind enumeration has changed")

// The lower the number the higher the precedence (think of it as first, second,
// third, ...)
CHECK_KIND(6);
int kind_precedence[KIND_COUNT] = {
	[KIND_NULL] = 0,
	[KIND_CONST] = 0,
	[KIND_VAR] = 0,
	[KIND_ADD] = 3,
	[KIND_MUL] = 2,
	[KIND_TRANS] = 1,
};

typedef uint16_t ExprHandle;
#define HANDLE_MAX UINT16_MAX
#define HANDLE_NULL ((ExprHandle){0})

// NOTE: For debuging it would be better to have a string instead of a single
// character, also probably generating names automatically linke A000, A001,
// ecc... Would make things more ergonomic.
typedef struct ExprNode {
	Kind kind;
	char name;
	bool req_grad;
	ExprHandle arg0;
	ExprHandle arg1;
} ExprNode;

static ExprNode node_pool[HANDLE_MAX + 1];

static ExprNode *
expr_get_node(ExprHandle handle) {
	return node_pool + handle;
}

static char
expr_char(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	CHECK_KIND(6);
	switch (node->kind) {
	case KIND_NULL:
		return '\0';
	case KIND_CONST:
		return node->name;
	case KIND_VAR:
		return node->name;
	case KIND_ADD:
		return '+';
	case KIND_MUL:
		return '*';
	case KIND_TRANS:
		return '\'';
	}
	__assume(0);
}

static void
expr_print(ExprHandle handle) {
	const ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return;
	}

	int node_precedence = kind_precedence[node->kind];
	int arg0_precedence = kind_precedence[expr_get_node(node->arg0)->kind];
	int arg1_precedence = kind_precedence[expr_get_node(node->arg1)->kind];
	bool arg0_print_parenthesis = arg0_precedence > node_precedence;
	bool arg1_print_parenthesis = arg1_precedence > node_precedence;

	if (arg0_print_parenthesis) {
		printf("(");
	}
	expr_print(node->arg0);
	if (arg0_print_parenthesis) {
		printf(")");
	}
	printf("%c", expr_char(handle));
	if (arg1_print_parenthesis) {
		printf("(");
	}
	expr_print(node->arg1);
	if (arg1_print_parenthesis) {
		printf(")");
	}
}

static ExprHandle node_pool_watermark = 1;

static ExprHandle
expr_make_node(Kind kind, char name, ExprHandle arg0, ExprHandle arg1) {
	assert(node_pool_watermark != HANDLE_MAX);
	ExprHandle res = node_pool_watermark++;
	node_pool[res] = (ExprNode) {
		.kind = kind,
		.name = name,
		.arg0 = arg0,
		.arg1 = arg1,
	};
	return res;
}

static ExprHandle
expr_make_operand(Kind kind, char name) {
	if (kind != KIND_CONST && kind != KIND_VAR) {
		return HANDLE_NULL;
	}
	return expr_make_node(kind, name, HANDLE_NULL, HANDLE_NULL);
}

static ExprHandle
expr_make_operator(Kind kind, ExprHandle arg0, ...) {
	if (kind != KIND_ADD && kind != KIND_MUL && kind != KIND_TRANS) {
		return HANDLE_NULL;
	}
	ExprHandle arg1 = HANDLE_NULL;
	if (kind != KIND_TRANS) {
		va_list args;
		va_start(args, arg0);
		arg1 = va_arg(args, ExprHandle);
		va_end(args);
	}
	// To make the print function work seamlessly with the transpose think of
	// arg0 as the lhs and arg1 as the rhs of a binary operator, then the
	// transpose only has a left argument. For other unary operators you can
	// just swap arg0 and arg1.
	return expr_make_node(kind, '\0', arg0, arg1);
}

// 1. traverse the tree to understand which branches require the gradient.
// 2. immagine that you are applying the differential to the expression,
//    starting from the top and goint to the bottom.
static void
expr_set_req_grad(ExprHandle handle) {
	ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return;
	}
	if (node->kind == KIND_VAR) {
		node->req_grad = true;
		return;
	}
	if (node->kind == KIND_CONST) {
		node->req_grad = false;
		return;
	}
	expr_set_req_grad(node->arg0);
	expr_set_req_grad(node->arg1);
	ExprNode *arg0_node = expr_get_node(node->arg0);
	ExprNode *arg1_node = expr_get_node(node->arg1);
	if (arg0_node->req_grad || arg1_node->req_grad) {
		node->req_grad = true;
	}
}

static ExprHandle
expr_differentiate(ExprHandle handle) {
	ExprNode *node = expr_get_node(handle);
	if (node->kind == KIND_NULL) {
		return HANDLE_NULL;
	}
}

int main(int argc, char const *argv[]) {
	// printf("sizeof (ExprNode) = %zu\n", sizeof (ExprNode));

	ExprHandle lhs = expr_make_operator(
		KIND_ADD, 
		expr_make_operand(KIND_VAR, 'X'),
		expr_make_operand(KIND_CONST, 'B')
	);
	ExprHandle rhs = expr_make_operator(
		KIND_ADD,
		expr_make_operand(KIND_CONST, 'C'),
		expr_make_operand(KIND_CONST, 'D')
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
	res = expr_make_operator(KIND_TRANS, res);
	expr_print(res);
	expr_set_req_grad(res);

	return 0;
}