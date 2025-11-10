/* =======================================================================
   C++-subset parser in a single Bison .y file (win_bison friendly)
   - Hand-written lexer inside (ASCII only)
   - Tokens printed as they are produced
   - Grammar: functions, globals, blocks, if/else, while, for, return, exprs
   - YYSTYPE is int (identifiers/strings/floats hashed; ints keep value; bool 0/1)
   - FIX: dangling-else resolved with precedence (no S/R conflicts)
   ======================================================================= */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* --------- Make older win_bison happy: define YYSTYPE here --------- */
typedef int YYSTYPE;

/* ----------------------- Lexer input buffer ----------------------- */
static char *g_buf = NULL;
static size_t g_len = 0;
static size_t g_pos = 0;

static int g_line = 1, g_col = 1;

extern int yylval;

/* 31-based simple hash into int (for ids/strings/floats) */
static int stringHash(const char* s) {
    unsigned int h = 0;
    for (const unsigned char* p = (const unsigned char*)s; *p; ++p)
        h = (h * 31u + *p) & 0x7fffffff;
    return (int)h;
}

/* Token printing: open bracket at first token, print items comma-separated, close on EOF */
static int tok_print_started = 0;
static int tok_count = 0;
static void print_tok(const char* name, const char* lex, int val) {
    if (!tok_print_started) {
        printf("Tokens:\n[");
        tok_print_started = 1;
        tok_count = 0;
    }
    if (tok_count++ > 0) printf(", ");
    if (lex && *lex)
        printf("%s(\"%s\", v=%d)", name, lex, val);
    else
        printf("%s(v=%d)", name, val);
}

/* Small helpers */
static int at_end(void){ return g_pos >= g_len; }
static int peek(void){ return at_end() ? 0 : g_buf[g_pos]; }
static int peek2(void){ return (g_pos+1<g_len)? g_buf[g_pos+1] : 0; }
static int adv(void){
    if (at_end()) return 0;
    int c = g_buf[g_pos++];
    if (c == '\n'){ g_line++; g_col = 1; } else g_col++;
    return c;
}
static void skip_ws_and_comments(void){
    for(;;){
        int c = peek();
        if (c==' '||c=='\t'||c=='\r'||c=='\n'||c=='\v'||c=='\f'){ adv(); continue; }
        if (c=='/' && peek2()=='/'){ while(peek()!=0 && peek()!='\n') adv(); continue; }
        if (c=='/' && peek2()=='*'){
            adv(); adv();
            for(;;){
                if (at_end()){ fprintf(stderr,"Unterminated block comment at %d:%d\n", g_line,g_col); exit(1); }
                if (peek()=='*' && peek2()=='/'){ adv(); adv(); break; }
                adv();
            }
            continue;
        }
        break;
    }
}

/* forward decls required by bison C interface */
static int  yylex(void);
static void yyerror(const char* s){
    fprintf(stderr, "Parse error at %d:%d: %s\n", g_line, g_col, s);
}
%}

/* ----------------------- Tokens ----------------------- */
/* keywords / types */
%token T_INT T_FLOAT T_BOOL T_CHAR T_STRING T_VOID
%token T_IF T_ELSE T_WHILE T_FOR T_RETURN
/* identifiers & literals */
%token T_IDENTIFIER T_INTLIT T_FLOATLIT T_STRINGLIT T_CHARLIT T_BOOLLIT
/* multichar operators */
%token T_EQUALSOP T_NEQ T_LTE T_GTE T_ANDAND T_OROR T_SHL T_SHR
%token T_PLUSEQ T_MINUSEQ T_MULEQ T_DIVEQ T_MODEQ T_ANDEQ T_OREQ T_XOREQ
%token T_INC T_DEC T_ARROW T_ASSIGNOP

%start program

/* precedence (note: use literal '<' '>' since lexer returns those chars) */
%right T_ASSIGNOP
%left  T_OROR
%left  T_ANDAND
%left  T_EQUALSOP T_NEQ
%left  '<' '>' T_LTE T_GTE
%left  '+' '-'
%left  '*' '/' '%'
%right UPLUS UMINUS '!'  /* unary */

/* --- dangling-else resolution --- */
%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

/* ----------------------- Grammar ----------------------- */
%%
program
    : decl_list
    ;

decl_list
    : /* empty */
    | decl_list declaration
    ;

declaration
    : type T_IDENTIFIER '(' opt_param_list ')' block
      { /* function */ }
    | type T_IDENTIFIER opt_init ';'
      { /* global var */ }
    ;

type
    : T_INT    | T_FLOAT | T_BOOL | T_CHAR | T_STRING | T_VOID
    ;

opt_param_list
    : /* empty */
    | param_list
    ;
param_list
    : param
    | param_list ',' param
    ;
param
    : type T_IDENTIFIER
    ;

opt_init
    : /* empty */
    | T_ASSIGNOP expression
    ;

block
    : '{' stmt_list '}'
    ;
stmt_list
    : /* empty */
    | stmt_list statement
    ;

statement
    : block
    /* if without else gets LOWER_THAN_ELSE precedence so else prefers nearest if */
    | T_IF '(' expression ')' statement               %prec LOWER_THAN_ELSE
    | T_IF '(' expression ')' statement T_ELSE statement
    | T_WHILE '(' expression ')' statement
    | T_FOR '(' for_init ';' opt_expr ';' opt_expr ')' statement
    | T_RETURN opt_expr ';'
    | type T_IDENTIFIER opt_init ';'              /* local decl */
    | opt_expr ';'                                /* expr or empty */
    ;

for_init
    : type T_IDENTIFIER opt_init                  /* decl-init */
    | opt_expr                                    /* expr-init or empty */
    ;

opt_expr
    : /* empty */
    | expression
    ;

/* ----------------- expressions ----------------- */
expression
    : assignment
    ;
assignment
    : T_IDENTIFIER T_ASSIGNOP assignment
    | logic_or
    ;
logic_or
    : logic_and
    | logic_or T_OROR logic_and
    ;
logic_and
    : equality
    | logic_and T_ANDAND equality
    ;
equality
    : relational
    | equality T_EQUALSOP relational
    | equality T_NEQ     relational
    ;
relational
    : additive
    | relational '<'  additive
    | relational '>'  additive
    | relational T_LTE additive
    | relational T_GTE additive
    ;
additive
    : multiplicative
    | additive '+' multiplicative
    | additive '-' multiplicative
    ;
multiplicative
    : unary
    | multiplicative '*' unary
    | multiplicative '/' unary
    | multiplicative '%' unary
    ;
unary
    : '+' unary      %prec UPLUS
    | '-' unary      %prec UMINUS
    | '!' unary
    | postfix
    ;
postfix
    : primary
    | postfix '(' opt_arg_list ')'
    | postfix '[' expression ']'
    ;
opt_arg_list
    : /* empty */
    | arg_list
    ;
arg_list
    : expression
    | arg_list ',' expression
    ;
primary
    : T_INTLIT
    | T_FLOATLIT
    | T_STRINGLIT
    | T_CHARLIT
    | T_BOOLLIT
    | T_IDENTIFIER
    | '(' expression ')'
    ;
%%

/* ======================= Hand-written lexer ======================= */
/* returns tokens above; yylval (int) holds numeric value or hash */

static int match_op2(int a, int b, int* out_tok){
    if (a=='=' && b=='='){ *out_tok=T_EQUALSOP; return 1; }
    if (a=='!' && b=='='){ *out_tok=T_NEQ;      return 1; }
    if (a=='<' && b=='='){ *out_tok=T_LTE;      return 1; }
    if (a=='>' && b=='='){ *out_tok=T_GTE;      return 1; }
    if (a=='&' && b=='&'){ *out_tok=T_ANDAND;   return 1; }
    if (a=='|' && b=='|'){ *out_tok=T_OROR;     return 1; }
    if (a=='<' && b=='<'){ *out_tok=T_SHL;      return 1; }
    if (a=='>' && b=='>' ){*out_tok=T_SHR;      return 1; }
    if (a=='+' && b=='+' ){*out_tok=T_INC;      return 1; }
    if (a=='-' && b=='-' ){*out_tok=T_DEC;      return 1; }
    if (a=='-' && b=='>' ){*out_tok=T_ARROW;    return 1; }
    if (a=='+' && b=='=' ){*out_tok=T_PLUSEQ;   return 1; }
    if (a=='-' && b=='=' ){*out_tok=T_MINUSEQ;  return 1; }
    if (a=='*' && b=='=' ){*out_tok=T_MULEQ;    return 1; }
    if (a=='/' && b=='=' ){*out_tok=T_DIVEQ;    return 1; }
    if (a=='%' && b=='=' ){*out_tok=T_MODEQ;    return 1; }
    if (a=='&' && b=='=' ){*out_tok=T_ANDEQ;    return 1; }
    if (a=='|' && b=='=' ){*out_tok=T_OREQ;     return 1; }
    if (a=='^' && b=='=' ){*out_tok=T_XOREQ;    return 1; }
    return 0;
}

static int is_ident_start(int c){ return isalpha(c) || c=='_'; }
static int is_ident_cont (int c){ return isalnum(c) || c=='_'; }

static void tok_name_for_op2(int tk, char *out, size_t n){
    const char* name = "T_ASSIGNOP";
    switch (tk){
        case T_EQUALSOP: name="T_EQUALSOP"; break;
        case T_NEQ:      name="T_NEQ"; break;
        case T_LTE:      name="T_LTE"; break;
        case T_GTE:      name="T_GTE"; break;
        case T_ANDAND:   name="T_ANDAND"; break;
        case T_OROR:     name="T_OROR"; break;
        case T_SHL:      name="T_SHL"; break;
        case T_SHR:      name="T_SHR"; break;
        case T_INC:      name="T_INC"; break;
        case T_DEC:      name="T_DEC"; break;
        case T_ARROW:    name="T_ARROW"; break;
        case T_PLUSEQ:   name="T_PLUSEQ"; break;
        case T_MINUSEQ:  name="T_MINUSEQ"; break;
        case T_MULEQ:    name="T_MULEQ"; break;
        case T_DIVEQ:    name="T_DIVEQ"; break;
        case T_MODEQ:    name="T_MODEQ"; break;
        case T_ANDEQ:    name="T_ANDEQ"; break;
        case T_OREQ:     name="T_OREQ"; break;
        case T_XOREQ:    name="T_XOREQ"; break;
    }
    strncpy(out, name, n);
    out[n-1] = 0;
}

static int yylex(void){
    skip_ws_and_comments();
    if (at_end()){
        if (tok_print_started){ printf("]\n"); }  /* close token list */
        return 0; /* EOF */
    }

    int c = peek();
    int d = peek2();

    /* multi-char operators */
    {
        int tk=0;
        if (match_op2(c,d,&tk)){
            char lex[3] = { (char)c, (char)d, 0 };
            adv(); adv();
            yylval = 0;
            char nm[32]; tok_name_for_op2(tk,nm,sizeof(nm));
            print_tok(nm, lex, yylval);
            return tk;
        }
    }

    /* '=' (assignment) as standalone */
    if (c=='='){
        adv();
        yylval=0;
        print_tok("T_ASSIGNOP", "=", 0);
        return T_ASSIGNOP;
    }

    /* single-char tokens */
    switch(c){
        case '(': adv(); print_tok("'('", "", 0); return '(';
        case ')': adv(); print_tok("')'", "", 0); return ')';
        case '{': adv(); print_tok("'{'", "", 0); return '{';
        case '}': adv(); print_tok("'}'", "", 0); return '}';
        case '[': adv(); print_tok("'['", "", 0); return '[';
        case ']': adv(); print_tok("']'", "", 0); return ']';
        case ';': adv(); print_tok("';'", "", 0); return ';';
        case ',': adv(); print_tok("','", "", 0); return ',';
        case ':': adv(); print_tok("':'", "", 0); return ':';
        case '.': adv(); print_tok("'.'", "", 0); return '.';
        case '+': adv(); print_tok("'+'", "", 0); return '+';
        case '-': adv(); print_tok("'-'", "", 0); return '-';
        case '*': adv(); print_tok("'*'", "", 0); return '*';
        case '/': adv(); print_tok("'/'", "", 0); return '/';
        case '%': adv(); print_tok("'%'", "", 0); return '%';
        case '<': adv(); print_tok("'<'", "", 0); return '<';
        case '>': adv(); print_tok("'>'", "", 0); return '>';
        case '!': adv(); print_tok("'!'", "", 0); return '!';
        case '&': adv(); print_tok("'&'", "", 0); return '&';
        case '|': adv(); print_tok("'|'", "", 0); return '|';
        case '^': adv(); print_tok("'^'", "", 0); return '^';
        case '~': adv(); print_tok("'~'", "", 0); return '~';
        case '?': adv(); print_tok("'?'", "", 0); return '?';
        case '\'': /* char literal */
        {
            adv();
            int ch = adv();
            if (ch=='\\'){
                int e=adv();
                ch = (e=='n')?'\n':(e=='t')?'\t':(e=='r')?'\r':(e=='\\')?'\\':(e=='\'')?'\'':(e=='"')?'"':e;
            }
            if (adv()!='\''){ fprintf(stderr,"Bad char literal at %d:%d\n", g_line,g_col); exit(1); }
            char buf[2]; buf[0]=(char)ch; buf[1]=0;
            yylval = (unsigned char)ch;
            print_tok("T_CHARLIT", buf, yylval);
            return T_CHARLIT;
        }
        case '"': /* string literal */
        {
            adv();
            char tmp[4096]; int len=0;
            while(!at_end() && peek()!='"'){
                int ch = adv();
                if (ch=='\\'){
                    int e=adv();
                    ch = (e=='n')?'\n':(e=='t')?'\t':(e=='r')?'\r':(e=='\\')?'\\':(e=='\'')?'\'':(e=='"')?'"':e;
                }
                if (len<4095) tmp[len++]=(char)ch;
            }
            if (peek()!='"'){ fprintf(stderr,"Unterminated string at %d:%d\n", g_line,g_col); exit(1); }
            adv();
            tmp[len]=0;
            yylval = stringHash(tmp);
            print_tok("T_STRINGLIT", tmp, yylval);
            return T_STRINGLIT;
        }
    }

    /* number (int or float) */
    if (isdigit(c)){
        char tmp[256]; int len=0; int dot=0; int expn=0;
        while(isdigit(peek())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        if (peek()=='.' && isdigit(peek2())){ dot=1; if (len<255) tmp[len++]=(char)adv(); else adv();
            while(isdigit(peek())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        }
        if (peek()=='e' || peek()=='E'){
            int t2 = g_pos+1<g_len? g_buf[g_pos+1] : 0;
            if (isdigit(peek2()) || ((t2=='+'||t2=='-') && (g_pos+2<g_len && isdigit(g_buf[g_pos+2])))){
                expn=1; if (len<255) tmp[len++]=(char)adv(); else adv();
                if (peek()=='+'||peek()=='-'){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
                if (!isdigit(peek())){ fprintf(stderr,"Malformed exponent at %d:%d\n", g_line,g_col); exit(1); }
                while(isdigit(peek())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
            }
        }
        tmp[len]=0;
        if (dot||expn){
            yylval = stringHash(tmp);
            print_tok("T_FLOATLIT", tmp, yylval);
            return T_FLOATLIT;
        } else {
            yylval = atoi(tmp);
            print_tok("T_INTLIT", tmp, yylval);
            return T_INTLIT;
        }
    }

    /* identifier / keyword */
    if (is_ident_start(c)){
        char tmp[256]; int len=0;
        tmp[len++]=(char)adv();
        while(is_ident_cont(peek())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        tmp[len]=0;

        /* keywords & booleans */
        if (!strcmp(tmp,"int"))    { yylval=0; print_tok("T_INT",    "",0); return T_INT; }
        if (!strcmp(tmp,"float"))  { yylval=0; print_tok("T_FLOAT",  "",0); return T_FLOAT; }
        if (!strcmp(tmp,"bool"))   { yylval=0; print_tok("T_BOOL",   "",0); return T_BOOL; }
        if (!strcmp(tmp,"char"))   { yylval=0; print_tok("T_CHAR",   "",0); return T_CHAR; }
        if (!strcmp(tmp,"string")) { yylval=0; print_tok("T_STRING", "",0); return T_STRING; }
        if (!strcmp(tmp,"void"))   { yylval=0; print_tok("T_VOID",   "",0); return T_VOID; }

        if (!strcmp(tmp,"if"))     { yylval=0; print_tok("T_IF",     "",0); return T_IF; }
        if (!strcmp(tmp,"else"))   { yylval=0; print_tok("T_ELSE",   "",0); return T_ELSE; }
        if (!strcmp(tmp,"while"))  { yylval=0; print_tok("T_WHILE",  "",0); return T_WHILE; }
        if (!strcmp(tmp,"for"))    { yylval=0; print_tok("T_FOR",    "",0); return T_FOR; }
        if (!strcmp(tmp,"return")) { yylval=0; print_tok("T_RETURN", "",0); return T_RETURN; }

        if (!strcmp(tmp,"true"))   { yylval=1; print_tok("T_BOOLLIT","true",1);  return T_BOOLLIT; }
        if (!strcmp(tmp,"false"))  { yylval=0; print_tok("T_BOOLLIT","false",0); return T_BOOLLIT; }

        /* identifier -> hash into yylval */
        yylval = stringHash(tmp);
        print_tok("T_IDENTIFIER", tmp, yylval);
        return T_IDENTIFIER;
    }

    /* unknown */
    fprintf(stderr,"Unexpected character '%c' (0x%02X) at %d:%d\n", peek(), (unsigned)peek(), g_line,g_col);
    adv();
    return yylex(); /* try to continue */
}

/* ======================= Driver: read stdin and parse ======================= */
int yyparse(void); /* generated */

int main(void){
    /* read whole stdin */
    fseek(stdin, 0, SEEK_END);
    long sz = ftell(stdin);
    if (sz < 0) sz = 0;
    fseek(stdin, 0, SEEK_SET);
    g_len = (size_t)sz;
    g_buf = (char*)malloc(g_len + 1);
    if (!g_buf){ fprintf(stderr,"OOM\n"); return 1; }
    size_t n = fread(g_buf,1,g_len,stdin);
    g_buf[n] = 0; g_len = n; g_pos = 0; g_line=1; g_col=1;

    int rc = yyparse();
    if (rc==0){
        printf("Parse OK\n");
    }
    free(g_buf);
    return rc;
}
