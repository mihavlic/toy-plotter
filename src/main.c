#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define _USE_MATH_DEFINES // to get math constants
#include <math.h>
#include <unistd.h>

#include <ncurses.h>

_Noreturn void eat_shit_and_die(char* err) {
    fputs(err, stderr);
    fputs("\n", stderr);
    exit(-1);
}

typedef enum {
    Sub,
    Add,
    Mul,
    Div,
    Abs,
    Exp,
    Num,
    Greater,
    Lower,
    GreaterEq,
    LowerEq,
    Eq,
    LParen,
    RParen,
    // functions
    Log,
    Log2,
    Ln,
    Sqrt,
    // constants
    X,
    Y,
    E,
    Pi,
    // identifier
    Ident,
} TokenKind;

// I see why tuples are useful
struct BuiltinMapping {
    const char* name;
    TokenKind builtin;
} builtins[] = {
    {"log", Log},
    {"log2", Log2},
    {"ln", Ln},
    {"sqrt", Sqrt},
    {"x", X},
    {"y", Y},
    {"e", E},
    {"pi", Pi},
    {NULL, 0},
};

struct NodeStruct;

typedef struct {
    struct NodeStruct* n1;
    struct NodeStruct* n2;
    TokenKind op;
} BinOpNode;

typedef struct {
    struct NodeStruct* n;
    TokenKind op;
} MonoOpNode;

typedef struct {
    struct NodeStruct* n;
    TokenKind kind;
} FunctionNode;

typedef enum {
    Number,
    BinaryOp,
    MonoOp,
    BuiltinFunction,
    BuiltinConstant,
    Identifier,
} NodeKind;

typedef struct NodeStruct {
    NodeKind tag;
    union {
        float num;
        BinOpNode b_op;
        MonoOpNode m_op;
        FunctionNode function;
        TokenKind constant;
        char* identifier;
    } data;
} Node;

typedef struct {
    TokenKind kind;
    union {
        float number;
        char* identifier;
    } data;
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

typedef struct {
    enum {
        LStart,
        LNumberIdent,
        LNumber,
        LIdent,
        LGreater,
        LLower,
    } state;
    char* ident_buf;
    char* ident_buf_cursor;
    char* ident_buf_end;
} LexerState;

#define LEXER_SINGLE_CHAR_TOKEN(char, token) \
case char: \
    tok.kind = token; \
    break;

// this comment is here for historic purposes
/* // ends upon reaching 'end' or encountering a '\0', if string is null terminated, end is not required to meaningful
// the parser can be buffered as it preserves state between invocation, but a number or an identifier cannot span
// two invocation as currently the lexer uses direct pointer indices that are not valid between allocations
//      FIXME either copy characters into a separate buffer or take into account buffering and try to stitch together two identifiers after they are tokenized
//      probably gonna take option 1 as we already need to copy out identifiers */
void lex(const char** next, const char* end, ArrayList* tokens, LexerState* state, bool flush) {
    const char* start = *next;
    Token tok;

    while (1) {
        char cur = '\0';
        // be careful not to dereference src if it's at the end, output null if it is
        if (*next != end && **next != '\0') {
            cur = **next;
            *next += 1;
        }

        switch (state->state) {
            case LStart:
                switch (cur) {
                case ' ':
                case '\n':
                case '\t':
                    start = *next;
                    goto skip_token;
                LEXER_SINGLE_CHAR_TOKEN('-', Sub)
                LEXER_SINGLE_CHAR_TOKEN('+', Add)
                LEXER_SINGLE_CHAR_TOKEN('*', Mul)
                LEXER_SINGLE_CHAR_TOKEN('/', Div)
                LEXER_SINGLE_CHAR_TOKEN('|', Abs)
                LEXER_SINGLE_CHAR_TOKEN('^', Exp)
                LEXER_SINGLE_CHAR_TOKEN('=', Eq)
                LEXER_SINGLE_CHAR_TOKEN('(', LParen)
                LEXER_SINGLE_CHAR_TOKEN(')', RParen)
                case '<':
                    state->state = LLower;
                    goto skip_token;
                case '>':
                    state->state = LGreater;
                    goto skip_token;
                case '\0':
                    return;
                default:
                    // a..z A..Z
                    if ((cur >= 'a' && cur <= 'z') || (cur >= 'Z' && cur <= 'Z')) {
                        state->state = LIdent;
                        goto skip_token;
                    // 0..9 .
                    } else if ((cur >= '0' && cur <= '9') || cur == '.') {
                        state->state = LNumber;
                        goto skip_token;
                    } else {
                        eat_shit_and_die("Invalid character in lexer");
                    }
                }
                break;
            case LNumber:
                if (!((cur >= '0' && cur <= '9') || cur == '.')) {
                    if (*next - start > state->ident_buf_end - state->ident_buf_cursor) {
                        eat_shit_and_die("Number is too long");
                    }
                    
                    // since we have already moved src* to another character after the current one, we need revert
                    if (cur != '\0') {
                        (*next)--;
                    }

                    while (start != *next) {
                        *state->ident_buf_cursor++ = *start++;
                    }
                    // if the next character is null, the string will possibly be split between
                    // two invocations, in that case just return, in the next invocation
                    // we again check that the next character is a string and if it is we continue,
                    // eventually encountering a non-string character, then we make this check again
                    // and either delay the parsing again (if at null) or finalize the string
                    // at the end of parsing, the lexer is invoked one last time with null
                    // to flush any string left unparsed
                    if (cur == '\0' && !flush) {
                        return;
                    }
                    
                    // ^ this above is boilerplate for multi-char tokens :(

                    float number = strtof(state->ident_buf, &state->ident_buf_cursor);

                    // reset ident_buf_cursor since it's not needed anymore 
                    state->ident_buf_cursor = state->ident_buf;
                    
                    tok.kind = Num;
                    tok.data.number = number;
                    break;
                } else {
                    goto skip_token;
                }
            case LIdent:
                if (!((cur >= 'a' && cur <= 'z') || (cur >= 'Z' && cur <= 'Z'))) {
                    if (*next - start > state->ident_buf_end - state->ident_buf_cursor) {
                        eat_shit_and_die("Ident is too long");
                    }
                    
                    // since we have already moved src* to another character after the current one, we need revert
                    if (cur != '\0') {
                        (*next)--;
                    }

                    while (start != *next) {
                        *state->ident_buf_cursor++ = *start++;
                    }

                    if (cur == '\0' && !flush) {
                        return;
                    }
                    
                    int len = state->ident_buf_cursor - state->ident_buf;
                    // reset ident_buf_cursor since it's not needed anymore 
                    state->ident_buf_cursor = state->ident_buf;

                    int found = -1;
                    // iterate through the array of builtin strings
                    for (int i = 0; builtins[i].name != NULL; i++) {
                        // iterate through the characters
                        for (int j = 0; ; j++) {
                            // since the characters in the state->ident_buf are not null terminated
                            // we just check that at the moment that the builtin string ends, the index of the null
                            // char is the same as the len (index + one for the null byte == len)
                            if (builtins[i].name[j] == '\0' && j == len) {
                                if (j == len) {
                                    found = i;
                                    goto found;
                                }
                                break;
                            }
                            // check that the characters are the same for both
                            // the moment they're not, bail
                            if (builtins[i].name[j] != state->ident_buf[j]) {
                                break;
                            }
                        }
                    }

                    found:
                    if (found != -1) {
                        tok.kind = builtins[found].builtin;
                        break;
                    }
                    
                    // TODO replace with a smarter allocation scheme
                    // copy to a null terminated string
                    char* string = malloc(len+1);
                    memcpy(string, state->ident_buf, len);
                    string[len] = '\0';

                    tok.kind = Ident;
                    tok.data.identifier = string;
                    break;
                } else {
                    goto skip_token; 
                }
            case LGreater:
                if (cur == '=') {
                    tok.kind = GreaterEq;
                // posibly split across invocations
                } else if (cur == '\0') {
                    return;
                } else {
                    tok.kind = Greater;
                    // need to roll back the char because we just encountered it but it wasn't a part of >=
                    *next -= 1;
                }
                break;
            case LLower:
                if (cur == '=') {
                    tok.kind = LowerEq;
                // posibly split across invocations
                } else if (cur == '\0') {
                    return;
                } else {
                    tok.kind = Lower;
                    // need to roll back the char because we just encountered it but it wasn't a part of <=
                    *next -= 1;
                }
                break;
            default:
                eat_shit_and_die("Unhandled lexer state");
        }

        list_reserve(tokens, sizeof(Token));
        *(Token*)tokens->cursor = tok;
        tokens->cursor += sizeof(Token);

        state->state = LStart;
        start = *next;

        skip_token:
            continue;
    }
}

char* token_name(TokenKind tok) {
#define FORMAT_CASE(what) \
case what: \
    return #what;

    switch (tok) {
        FORMAT_CASE(Sub)
        FORMAT_CASE(Add)
        FORMAT_CASE(Mul)
        FORMAT_CASE(Div)
        FORMAT_CASE(Abs)
        FORMAT_CASE(Exp)
        FORMAT_CASE(Num)
        FORMAT_CASE(Greater)
        FORMAT_CASE(Lower)
        FORMAT_CASE(GreaterEq)
        FORMAT_CASE(LowerEq)
        FORMAT_CASE(Eq)
        FORMAT_CASE(LParen)
        FORMAT_CASE(RParen)
        FORMAT_CASE(Log)
        FORMAT_CASE(Log2)
        FORMAT_CASE(Ln)
        FORMAT_CASE(Sqrt)
        FORMAT_CASE(X)
        FORMAT_CASE(Y)
        FORMAT_CASE(E)
        FORMAT_CASE(Pi)
        FORMAT_CASE(Ident)
        default:
            eat_shit_and_die("Unhandled operator");
    }
}

int operator_precedence(Token* token) {
    switch (token->kind) {
    case Sub:
    case Add:
        return 4;
    case Div:
        return 3;
    case Mul:
        return 2;
    case Exp:
        return 1;
    case Greater:
    case Lower:
    case GreaterEq:
    case LowerEq:
    case Eq:
        return 5;
    default:
        return -1;
    }
}

#define NEW(type) (type*)malloc(sizeof(type))

Node* parse_expr(Token** start, Token* end, int precedence);

void ensure_token(Token** start, Token* end, TokenKind token) {
    if ((*start == end) || ((*start)->kind != token)) {
        eat_shit_and_die("Expected token missing");
    }
    (*start)++;
}

Node* parse_monoop(Token** start, Token* end) {
    if (*start == end) {
        eat_shit_and_die("Expected expression");
    }

    Node* node;
    Node* temp_node;
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
        node->data.num = cur->data.number;
        
        // implements implicit multiplication: 4x becomes 4*x
        // possibly replace this with a peephole substitution to add the implicit multiply into the tokens
        // TODO along with this replace some exponentiations with chained multiplications
        if (*start != end) {
            switch ((*start)->kind) {
                case Num:
                case LParen:
                case Abs:
                case Log:
                case Log2:
                case Ln:
                case Sqrt:
                case X:
                case Y:
                case E:
                case Pi:
                    temp_node = node;
                    node = NEW(Node);
                    node->tag = BinaryOp;
                    node->data.b_op.op = Mul;
                    node->data.b_op.n1 = temp_node;
                    node->data.b_op.n2 = parse_expr(start, end, 2 - 1);
                    break;
                default:
                    break;
            }
        }
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
    case Log:
    case Log2:
    case Ln:
    case Sqrt:
        node = NEW(Node);
        node->tag = BuiltinFunction;
        node->data.function.kind = cur->kind;
        // only multiplication and exponentiation are more associative that function call
        // parentheses are not required
        node->data.function.n = parse_expr(start, end, 2);
        break;
    case X:
    case Y:
    case E:
    case Pi:
        node = NEW(Node);
        node->tag = BuiltinConstant;
        node->data.constant = cur->kind;
        break;
    case Ident:
        eat_shit_and_die("Currently usupported");
    default:
        eat_shit_and_die("Unexpected character in mono op");
    }
    
    return node;
}

Node* parse_expr(Token** start, Token* end, int precedence) {

    Node* lhs = parse_monoop(start, end);

    while (*start != end) {
        Token* op = *start;
        int prec = operator_precedence(op);

        if ((prec != -1) && (prec <= precedence)) {
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
        printf(")");
        break;
    case BinaryOp:
        printf("(%s ", token_name(node->data.b_op.op));
        traverse_ast(node->data.b_op.n1);
        printf(", ");
        traverse_ast(node->data.b_op.n2);
        printf(")");
        break;
    case BuiltinFunction:
        printf("(%s ", token_name(node->data.function.kind));
        traverse_ast(node->data.function.n);
        printf(")");
        break;
    case BuiltinConstant:
        printf("%s", token_name(node->data.constant));
        break;
    case Identifier:
        printf("$%s", node->data.identifier);
        break;
    default:
        eat_shit_and_die("I've left a node type unhandled");
    }
}

#define LANE_WIDTH 8
#define LANE_ALIGN 16

#define LANE_MAP(in1, in2, out, code) \
    for (int i = 0; i < LANE_WIDTH; i++) { \
        float a = in1[i]; \
        float b = in2[i]; \
        out[i] = code; \
    }

#define LANE_MAP_SINGLE(in, out, code) \
    for (int i = 0; i < LANE_WIDTH; i++) { \
        float a = in[i]; \
        out[i] = code; \
    }

#define LANE_MAP_NONE(out, code) \
    for (int i = 0; i < LANE_WIDTH; i++) { \
        out[i] = code; \
    }

// lane evaluation macros, did I just make a generic simd thing?

#define MONO_OP_CASE(op, code) \
case op: \
    LANE_MAP_SINGLE(out, out, code); \
    break;

#define BINARY_OP_CASE(op, code) \
case op: \
    LANE_MAP(out, tmp, out, code); \
    break;

#define CONSTANT_CASE(op, code) \
case op: \
    LANE_MAP_NONE(out, code); \
    break;

void array_eval(const Node* node, float x[LANE_WIDTH], float y[LANE_WIDTH], float out[LANE_WIDTH]) {
    _Alignas(LANE_ALIGN) float tmp[LANE_WIDTH];

    switch (node->tag) {
    case Number:
        LANE_MAP_NONE(out, node->data.num);
        break;
    case MonoOp:
        array_eval(node->data.m_op.n, x, y, out);

        switch(node->data.m_op.op) {
            MONO_OP_CASE(Sub, -a)
            MONO_OP_CASE(Abs, fabsf(a))
            default:
                eat_shit_and_die("Unhandled monoop evaluation");
        }
        break;
    case BinaryOp:
        array_eval(node->data.b_op.n1, x, y, out);
        array_eval(node->data.b_op.n2, x, y, tmp);

        switch (node->data.b_op.op) {
            BINARY_OP_CASE(Sub, a-b)
            BINARY_OP_CASE(Add, a+b)
            BINARY_OP_CASE(Mul, a*b)
            BINARY_OP_CASE(Div, a/b)
            BINARY_OP_CASE(Exp, powf(a,b))
            BINARY_OP_CASE(Greater, a > b)
            BINARY_OP_CASE(Lower, a < b)
            BINARY_OP_CASE(GreaterEq, a >= b)
            BINARY_OP_CASE(LowerEq, a <= b)
            BINARY_OP_CASE(Eq, a == b)
            default:
                eat_shit_and_die("Unhandled binop evaluation");
        }
        break;
    // hmm right now functions are exactly line monoops just with different syntax
    case BuiltinFunction:
        array_eval(node->data.function.n, x, y, out);
        
        switch (node->data.function.kind) {
            MONO_OP_CASE(Log, log10f(a))
            MONO_OP_CASE(Log2, log2f(a))
            MONO_OP_CASE(Ln, logf(a))
            MONO_OP_CASE(Sqrt, sqrtf(a))
            default:
                eat_shit_and_die("Unhandled builtin function");
        }
        break;
    case BuiltinConstant:
        switch (node->data.constant) {
            case X:
                LANE_MAP_SINGLE(x, out, a);
                break;
            case Y:
                LANE_MAP_SINGLE(y, out, a);
                break;
            CONSTANT_CASE(E, M_E)
            CONSTANT_CASE(Pi, M_PI)
            default:
                eat_shit_and_die("Unhandled builtin constant");
        }
        break;
    case Identifier:
        eat_shit_and_die("Currently usupported");
    default:
        eat_shit_and_die("I've left a node type unhandled");
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2 || strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
        puts("No equation provided");
        puts("Usage: [binary] [equation] ?[o/no] - force or disable outline algorithm");
        return 0;
    }

    Node* root;
    {
        bool debug = getenv("PARSE_DEBUG") != NULL;
        const char* what = argv[1];
        
        if (debug) {
            printf("%s\n", what);
        }
        
        ArrayList tokens;
        list_init(&tokens, 32*sizeof(Token));

        char buf[32];
        LexerState state = {
            .state = LStart,
            .ident_buf = buf,
            .ident_buf_cursor = buf,
            .ident_buf_end = buf + 32,
        };

        // the end pointer can be whatever if the string is null terminated
        lex(&what, 0, &tokens, &state, true);
        
        if (tokens.cursor == tokens.allocation) {
            eat_shit_and_die("No tokens lexed");
        }

        if (debug) {
            for (Token* tok = (Token*)tokens.allocation; tok != (Token*)tokens.cursor; tok++) {
                switch (tok->kind) {
                case Num:
                    printf("%f ", tok->data.number);
                    break;
                case Ident:
                    printf("$%s ", tok->data.identifier);
                    break;
                default:
                    printf("%s ", token_name(tok->kind));
                    break;
                }
            }
            printf("\n");
        }
        
        Token* start = (Token*)tokens.allocation;
        root = parse_expr(&start, (Token*)tokens.cursor, 8);

        if (debug) {
            printf("\n");
            traverse_ast(root);
            printf("\n");
        }

        list_free(&tokens);
    }

    // -1 disabled, 0 unconfigured, 1 enabled
    int outline = 0;
    if (argc >= 3) {
        if (strcmp(argv[2], "o") == 0) {
            outline = 1;
        } else if (strcmp(argv[2], "no") == 0) {
            outline = -1;
        }
    }

    // heuristic, we probably want this for equation as it gives much better quality
    // transform equation into a subtraction so that we don't lose the gradient of the function
    if (outline != -1 && root->tag == BinaryOp && root->data.b_op.op == Eq) {
        outline = 1;
        root->data.b_op.op = Sub;
    }
    
    initscr();
    noecho(); // dont print typed chars
    cbreak(); // dont handle clear shortcuts and buffering
    keypad(stdscr, TRUE); // handle arrow keys properly
    nodelay(stdscr, TRUE); // don't block on wgetch
    curs_set(0); // hide cursor
    use_tioctl(true); // get the correct terminal size

    // how many value units a single character is
    float zoom = 1.0;
    float x_shift = 0.0;
    float y_shift = 0.0;

    int w = 0;
    int h = 0;
    // for the outlines we need to evaluate a row outside the screen
    // so if outline is enabled these are the dimensions of the buffer but not the screen
    int w_ = w;
    int h_ = h;
    float* buffer = 0;
    chtype* ch_buffer = 0;

    bool dirty = false;

    while (1) {
        int ch = wgetch(stdscr);
        switch (ch) {
        case KEY_UP:
            y_shift += 1.0*zoom;
            dirty = true;
            break;
        case KEY_DOWN:
            y_shift -= 1.0*zoom;
            dirty = true;
            break;
        case KEY_RIGHT:
            x_shift += 1.0*zoom;
            dirty = true;
            break;
        case KEY_LEFT:
            x_shift -= 1.0*zoom;
            dirty = true;
            break;
        case 'w':
            zoom *= 0.9;
            dirty = true;
            break;
        case 'z':
            zoom /= 0.9;
            dirty = true;
            break;
        case 'r':
            zoom = 1.0;
            x_shift = 0.0;
            y_shift = 0.0;
            dirty = true;
            break;
        case 'q':
            endwin();
            return 0;
        default:
            break;
        }

        // cap the zoom so that it doesn't become negative
        zoom = fmax(zoom, 0.0001);

        if (w != COLS || h != LINES) {
            dirty = true;
            w = COLS;
            h = LINES;
            
            if (outline) {
                w_ = w + 1;
                h_ = h + 1;
            } else {
                w_ = w;
                h_ = h;
            }
            
            // round up to a multiple of LANE_WIDTH
            int size = ((w_*h_ + LANE_WIDTH - 1) / LANE_WIDTH) * LANE_WIDTH;

            if (buffer) {
                free(buffer);
                free(ch_buffer);
            }

            buffer = aligned_alloc(LANE_ALIGN, size*sizeof(float));
            ch_buffer = malloc(size*sizeof(chtype));
        }

        if (dirty && w != 0 && h != 0) {
            // not needed since we overwrite everything anyway
            // clear();

            _Alignas(LANE_ALIGN) float x_arr[LANE_WIDTH];
            _Alignas(LANE_ALIGN) float y_arr[LANE_WIDTH];

            // since terminal cells aren't square, we squish the y direction to roughly compensate
            const float y_mul = 2.0;

            float* out = buffer;
            float xf0 = x_shift - w*zoom*0.5;
            float xf;
            float yf = y_shift + h*zoom*0.5*y_mul;
            float d = zoom;

            int i = 0;
            for (int y = 0; y < h_; y++) {
                xf = xf0;
                for (int x = 0; x < w_; x++) {
                    x_arr[i] = xf;
                    y_arr[i] = yf;
                    xf += d;
                    i++;

                    if (i == LANE_WIDTH) {
                        array_eval(root, x_arr, y_arr, out);
                        out += LANE_WIDTH;
                        i = 0;
                    }
                }
                yf -= d*y_mul;
            }
            // finish the rest, out was padded enough so that we don't overrun it
            if (i != 0) {
                array_eval(root, x_arr, y_arr, out);
            }

#define CELL_CHECK(x, y) \
if (((x >= 0) && (x < w_) && (y >= 0) && (y < h_))) { \
    buffer[(x)+(y)*w_] > 0 ? positive++ : negative++; \
}

            if (outline == 1) {
                // stolen from https://www.youtube.com/watch?v=EvvWOaLgKVU
                for (int y = 0; y < h_; y++) {
                    for (int x = 0; x < w_; x++) {
                        int positive = 0;
                        int negative = 0;
                        CELL_CHECK(x,y)
                        CELL_CHECK(x+1,y)
                        CELL_CHECK(x,y+1)
                        CELL_CHECK(x+1,y+1)

                        // ch_buffer[x+y*w_] = (positive > 0 && positive < 3 && negative > 0) ? 'X' : ' ';
                        ch_buffer[x+y*w_] = (positive > 0 && negative > 0) ? 'X' : ' ';
                    }
                }
            } else {
                for (int j = 0; j < w_*h; j++) {
                    ch_buffer[j] = buffer[j] > 0.0 ? 'X' : ' ';
                }
            }

            chtype* ch_out = ch_buffer;
            for (int k = 0; k < h; k++) {
                mvaddchnstr(k, 0, ch_out, w);
                ch_out += w_;
            }

            dirty = false;
            refresh();
        }

        // sleep ~16 ms to get 60 fps, this assumes frame drawing is instant
        usleep(16666);
    }

    return 0;
}
