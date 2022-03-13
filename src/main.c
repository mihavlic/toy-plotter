#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void eat_shit_and_die(char* err) {
    fputs(err, stderr);
    fputs("\n", stderr);
    exit(-1);
}

typedef enum {
    Sub,
    Add,
    Mul,
    Abs,
    Exp,
    Num,
    Greater,
    Lower,
    GreaterEqual,
    LowerEqual,
    Equal,
    VarX,
    VarY,
    LParen,
    RParen,
} TokenKind;

struct NodeStruct;

typedef struct {
    struct NodeStruct* n1;
    struct NodeStruct* n2;
    TokenKind op;
} BinOp;

typedef struct {
    struct NodeStruct* n;
    TokenKind op;
} MonOp;

typedef enum {
    Number,
    BinaryOp,
    MonoOp,
    // TODO handle general variables (user-defined, PI, etc)
    VariableX,
    VariableY,
} NodeTag;

typedef struct NodeStruct {
    NodeTag tag;
    union {
        double num;
        BinOp b_op;
        MonOp m_op;
    } data;
} Node;

typedef struct {
    TokenKind kind;
    double value;
} Token;

typedef struct {
    Token* start;
    Token* current;
    Token* end;
} Lexer;

typedef struct {
    void* cursor;
    void* allocation;
    void* end;
}  ArrayList;

void list_init(ArrayList* list, int capacity) {
    if (capacity <= 0) {
        list->cursor = NULL;
        list->allocation = NULL;
        list->end = NULL;
        return;
    }

    void* alloc = malloc(capacity);

    list->cursor = alloc;
    list->allocation = alloc;
    list->end = alloc + capacity;
}

void list_free(ArrayList* list) {
    if (list->allocation) {
        free(list->allocation);
    }
}

void list_reserve(ArrayList* list, int reserve) {
    void* cursor = list->cursor;
    void* allocation = list->allocation;
    int capacity = list->end - list->allocation;
    // careful! this is the offset of the last element, not actually what would be considered 'length'
    int len = cursor - allocation;

    if (capacity < len + reserve + 1) {
        // the allocated block is not big enough for the upcoming insertion
        // double the size, copy the contents, and free the old block
        int new_cap = capacity*2;
        if (new_cap < reserve) {
            new_cap = reserve;
        }

        void* new = malloc(new_cap);
        memcpy(new, allocation, len);
        free(allocation);

        list->cursor = new + len;
        list->allocation = new;
        list->end = new + capacity*2;
    }
}

void push_token(ArrayList* list, Token* token) {
    list_reserve(list, sizeof(Token));
    *(Token*)list->cursor = *token;
    list->cursor += sizeof(Token);
}

ArrayList lex(char* src) {
#define OPERATOR_CASE(what, tokenKind) \
case what: \
    tok.kind = tokenKind; \
    break;

#define OPERATOR_CASE_DOUBLE(what, what2, tokenKind_single, tokenKind_double) \
case what: \
    if (nxt == what2) { \
        tok.kind = tokenKind_double; \
        if (nxt == 0) break; \
        cur = nxt; \
        nxt = *++c; \
    } else { \
        tok.kind = tokenKind_single; \
    } \
    break;

    ArrayList tokens = {};

    // early exit if string is empty
    if (*src == '\0') {
        // tokens is nulled which is interpreted as being empty
        return tokens;
    }

    list_init(&tokens, 32*sizeof(Token));

    char* start = 0;

    // the cur,next pair starts with cur being a space so that the numeric lexing part can
    // operate on the next variable including the first character, this way there is only one part that
    // pushes the token
    char* c = src;
    char cur = ' ';
    char nxt = *src;

    while (1) {
        Token tok = {};

        int is_numeric = ('0' <= nxt && nxt <= '9') || nxt == '.';
        if (is_numeric) {
            if (start == 0) {
                start = c;
            }
        } else if (start != 0) {
            // strtod modifies the end pointer, I don't think I want that?
            char* end = c + 1;
            double val = strtod(start, &end);

            tok.kind = Num;
            tok.value = val;

            start = 0;
            goto finish_token;
        }

        switch (cur) {
            case ' ':
            case '\n':
            case '\t':
                goto next_char;
            OPERATOR_CASE('-', Sub)
            OPERATOR_CASE('+', Add)
            OPERATOR_CASE('*', Mul)
            OPERATOR_CASE('|', Abs)
            OPERATOR_CASE('^', Exp)
            OPERATOR_CASE('=', Equal)
            OPERATOR_CASE('x', VarX)
            OPERATOR_CASE('y', VarY)
            OPERATOR_CASE('(', LParen)
            OPERATOR_CASE(')', RParen)
            OPERATOR_CASE_DOUBLE('>', '=', Greater, GreaterEqual)
            OPERATOR_CASE_DOUBLE('<', '=', Lower, LowerEqual)
            default:
                eat_shit_and_die("Unknown char");
                goto end;
        }

        finish_token:
        push_token(&tokens, &tok);

        next_char:
        // end of string
        if (nxt == 0) break;

        cur = nxt;
        nxt = *++c;
    }

    end:
    return tokens;
}

char* token_name(TokenKind tok) {
#define FORMAT_CASE(what) \
case what: \
    return #what;

    switch (tok) {
        FORMAT_CASE(Sub)
        FORMAT_CASE(Add)
        FORMAT_CASE(Mul)
        FORMAT_CASE(Abs)
        FORMAT_CASE(Exp)
        FORMAT_CASE(Num)
        FORMAT_CASE(Greater)
        FORMAT_CASE(Lower)
        FORMAT_CASE(GreaterEqual)
        FORMAT_CASE(LowerEqual)
        FORMAT_CASE(Equal)
        FORMAT_CASE(VarX)
        FORMAT_CASE(VarY)
        FORMAT_CASE(LParen)
        FORMAT_CASE(RParen)
        default:
            eat_shit_and_die("Unhandled operator");
            return "Error";
    }
}

int operator_precedence(Token* token) {
    switch (token->kind) {
    case Sub:
    case Add:
        return 4;
    case Mul:
        return 2;
    case Exp:
        return 1;
    case Greater:
    case Lower:
    case GreaterEqual:
    case LowerEqual:
        return 3;
    case Equal:
        return 5;
    default:
        return -1;
    }
}

#define NEW(type) \
    (type*)malloc(sizeof(type))

Node* parse_expr(Token** start, Token* end, int precedence);

void ensure_token(Token** start, Token* end, TokenKind token) {
    if ((*start == end) || ((*start)->kind != token)) {
        eat_shit_and_die("Expected token missing");
    }
}

Node* parse_monoop(Token** start, Token* end) {
    if (*start == end) {
        eat_shit_and_die("Expected expression");
    }

    Node* node;
    Token* cur = *start;
    (*start)++;

    switch (cur->kind) {
    case Sub:
        node = NEW(Node);
        node->tag = MonoOp;
        node->data.m_op.op = Sub;
        node->data.m_op.n = parse_monoop(start, end);
        break;
    case Num:
        node = NEW(Node);
        node->tag = Number;
        node->data.num = cur->value;
        break;
    case LParen:
        node = parse_expr(start, end, 8);
        ensure_token(start, end, RParen);
        break;
    case Abs:
        node = NEW(Node);
        node->tag = MonoOp;
        node->data.m_op.op = Abs;
        node->data.m_op.n = parse_expr(start, end, 8);
        ensure_token(start, end, Abs);
        break;
    case VarX:
        node = NEW(Node);
        node->tag = VariableX;
        break;
    case VarY:
        node = NEW(Node);
        node->tag = VariableY;
        break;
    default:
        eat_shit_and_die("Unexpected character in mono op");
        return 0;
    }

    return node;
}

Node* parse_expr(Token** start, Token* end, int precedence) {

    Node* lhs = parse_monoop(start, end);

    while (*start != end) {
        Token* op = *start;
        int prec = operator_precedence(op);

        if ((prec != -1) && prec < precedence) {
            (*start)++;
            Node* node = NEW(Node);
            node->tag = BinaryOp;
            node->data.b_op.op = op->kind;
            node->data.b_op.n1 = lhs;
            node->data.b_op.n2 = parse_expr(start, end, prec - 1);

            lhs = node;
            continue;
        }

        break;
    }

    return lhs;
}

void traverse_ast(Node* node) {
    switch (node->tag) {
    case Number:
        printf("(%f)", node->data.num);
        break;
    case MonoOp:
        printf("(%s ", token_name(node->data.m_op.op));
        traverse_ast(node->data.m_op.n);
        printf(") ");
        break;
    case BinaryOp:
        printf("(%s ", token_name(node->data.b_op.op));
        traverse_ast(node->data.b_op.n1);
        printf(", ");
        traverse_ast(node->data.b_op.n2);
        printf(") ");
        break;
    case VariableX:
        printf("X");
        break;
    case VariableY:
        printf("Y");
        break;
    default:
        eat_shit_and_die("I've left a node type unhandled");
    }
}

int main(int argc, char *argv[]) {
    if (argc > 1) {
        char* what = argv[1];
        printf("%s\n", what);

        ArrayList tokens = lex(what);

        for (Token* tok = (Token*)tokens.allocation; tok != (Token*)tokens.cursor; tok++) {
            if (tok->kind == Num) {
                printf("%f ", tok->value);
            } else {
                char* string = token_name(tok->kind);
                printf("%s ", string);
            }
        }
        printf("\n");

        Token* start = (Token*)tokens.allocation;
        Node* root = parse_expr(&start, (Token*)tokens.cursor, 8);
        traverse_ast(root);

        list_free(&tokens);

        printf("\n");
    }
}
