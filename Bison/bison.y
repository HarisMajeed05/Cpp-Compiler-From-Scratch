/* =======================================================================
   C++-subset parser in a single Bison .y file (win_bison friendly)
   - Hand-written ASCII lexer inside
   - Prints tokens as they are produced
   - Builds an AST and prints it after parsing
   - Generates ONE .cpp
   ======================================================================= */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ---------------- AST types ---------------- */

typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;

typedef struct VecExpr { Expr** data; int n, cap; } VecExpr;
typedef struct VecStmt { Stmt** data; int n, cap; } VecStmt;
typedef struct VecDecl { Decl** data; int n, cap; } VecDecl;

static void* xmalloc(size_t n){ void* p = malloc(n); if(!p){fprintf(stderr,"OOM\n"); exit(1);} return p; }
static char* xstrdup(const char* s){ size_t n=strlen(s)+1; char* p=(char*)xmalloc(n); memcpy(p,s,n); return p; }

/* Expr kinds */
struct Expr {
    char* kind;
    char* op;
    char* litType;
    char* value;
    VecExpr args;
};

/* Stmt kinds */
struct Stmt {
    char* kind;
    char* typeTok; char* name; Expr* init;      /* VarDecl */
    Expr* ret;                                   /* Return */
    Expr* expr;                                  /* ExprStmt */
    Expr* ifCond; VecStmt ifBlock, elseBlock;    /* If     */
    Expr* whileCond; VecStmt whileBlock;         /* While  */
    int hasInitDecl; char* forInitType; char* forInitName; Expr* forInitExpr;
    Expr* forCond; Expr* forUpdt; VecStmt forBlock;      /* For */
    VecStmt block;                               /* Block  */
};

/* Decl kinds: FnDecl, VarDecl, UsingNS */
struct Decl {
    char* kind;
    /* Fn */
    char* retType; char* fnName;
    struct { char** types; char** names; int n, cap; } params;
    VecStmt body;
    /* Var */
    char* varType; char* varName; Expr* varInit;
    /* Using */
    char* usingNS;
};

/* program root */
static VecDecl g_program = {0};

/* helpers */
static void ve_push(VecExpr* v, Expr* e){ if(v->n==v->cap){ v->cap=v->cap?2*v->cap:8; v->data=(Expr**)realloc(v->data, v->cap*sizeof(*v->data)); } v->data[v->n++]=e; }
static void vs_push(VecStmt* v, Stmt* s){ if(v->n==v->cap){ v->cap=v->cap?2*v->cap:8; v->data=(Stmt**)realloc(v->data, v->cap*sizeof(*v->data)); } v->data[v->n++]=s; }
static void vd_push(VecDecl* v, Decl* d){ if(v->n==v->cap){ v->cap=v->cap?2*v->cap:8; v->data=(Decl**)realloc(v->data, v->cap*sizeof(*v->data)); } v->data[v->n++]=d; }

static void params_push(Decl* d, const char* ty, const char* nm){
    if(d->params.n==d->params.cap){ d->params.cap=d->params.cap?2*d->params.cap:8;
        d->params.types=(char**)realloc(d->params.types,d->params.cap*sizeof(char*));
        d->params.names=(char**)realloc(d->params.names,d->params.cap*sizeof(char*)); }
    d->params.types[d->params.n]=xstrdup(ty);
    d->params.names[d->params.n]=xstrdup(nm);
    d->params.n++;
}

/* constructors */
static Expr* mk_lit(const char* t, const char* v){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Literal"); e->litType=xstrdup(t); e->value=xstrdup(v?v:""); return e; }
static Expr* mk_ident(const char* v){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Identifier"); e->value=xstrdup(v?v:""); return e; }
static Expr* mk_unary(const char* op, Expr* a){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Unary"); e->op=xstrdup(op); ve_push(&e->args,a); return e; }
static Expr* mk_binary(const char* op, Expr* a, Expr* b){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Binary"); e->op=xstrdup(op); ve_push(&e->args,a); ve_push(&e->args,b); return e; }
static Expr* mk_call(Expr* target, VecExpr args){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Call");
    e->value = target && target->kind && strcmp(target->kind,"Identifier")==0 ? xstrdup(target->value): xstrdup("<non-ident>");
    for(int i=0;i<args.n;i++) ve_push(&e->args,args.data[i]); return e; }
static Expr* mk_index(Expr* arr, Expr* idx){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Index"); ve_push(&e->args,arr); ve_push(&e->args,idx); return e; }
static Expr* mk_group(Expr* a){ Expr* e=(Expr*)xmalloc(sizeof(*e)); memset(e,0,sizeof(*e));
    e->kind=xstrdup("Grouping"); ve_push(&e->args,a); return e; }

static Stmt* mk_stmt(const char* k){ Stmt* s=(Stmt*)xmalloc(sizeof(*s)); memset(s,0,sizeof(*s)); s->kind=xstrdup(k); return s; }
static Decl* mk_decl(const char* k){ Decl* d=(Decl*)xmalloc(sizeof(*d)); memset(d,0,sizeof(*d)); d->kind=xstrdup(k); return d; }

/* printers */
static void printExpr(Expr* e, int ind);
static void printStmt(Stmt* s, int ind);
static void printDecl(Decl* d, int ind);

static void pad(int n){ for(int i=0;i<n;i++) putchar(' '); }

static void printExpr(Expr* e, int ind){
    if(!e){ pad(ind); printf("null\n"); return; }
    if(strcmp(e->kind,"Literal")==0){
        pad(ind); printf("Lit(%s:%s)\n", e->litType?e->litType:"?", e->value?e->value:"");
    } else if(strcmp(e->kind,"Identifier")==0){
        pad(ind); printf("Ident(%s)\n", e->value?e->value:"");
    } else if(strcmp(e->kind,"Unary")==0){
        pad(ind); printf("Unary(%s)\n", e->op?e->op:"?"); if(e->args.n) printExpr(e->args.data[0], ind+2);
    } else if(strcmp(e->kind,"Binary")==0){
        pad(ind); printf("Binary(%s)\n", e->op?e->op:"?"); if(e->args.n>0) printExpr(e->args.data[0], ind+2); if(e->args.n>1) printExpr(e->args.data[1], ind+2);
    } else if(strcmp(e->kind,"Call")==0){
        pad(ind); printf("Call(%s) args[\n", e->value?e->value:""); for(int i=0;i<e->args.n;i++) printExpr(e->args.data[i], ind+2); pad(ind); printf("]\n");
    } else if(strcmp(e->kind,"Index")==0){
        pad(ind); printf("Index\n"); if(e->args.n>0) printExpr(e->args.data[0], ind+2); if(e->args.n>1) printExpr(e->args.data[1], ind+2);
    } else if(strcmp(e->kind,"Grouping")==0){
        pad(ind); printf("Grouping\n"); if(e->args.n) printExpr(e->args.data[0], ind+2);
    } else { pad(ind); printf("<?> expr\n"); }
}

static void printStmt(Stmt* s, int ind){
    if(!s){ pad(ind); printf("null\n"); return; }
    if(strcmp(s->kind,"ExprStmt")==0){
        pad(ind); printf("ExprStmt\n"); printExpr(s->expr, ind+2);
    } else if(strcmp(s->kind,"Return")==0){
        pad(ind); printf("Return\n"); printExpr(s->ret, ind+2);
    } else if(strcmp(s->kind,"VarDecl")==0){
        pad(ind); printf("VarDecl(type=%s, name=%s)\n", s->typeTok?s->typeTok:"?", s->name?s->name:"?"); if(s->init) printExpr(s->init, ind+2);
    } else if(strcmp(s->kind,"If")==0){
        pad(ind); printf("If\n"); printExpr(s->ifCond, ind+2);
        pad(ind); printf("Then[\n"); for(int i=0;i<s->ifBlock.n;i++) printStmt(s->ifBlock.data[i], ind+2); pad(ind); printf("]\n");
        pad(ind); printf("Else[\n"); for(int i=0;i<s->elseBlock.n;i++) printStmt(s->elseBlock.data[i], ind+2); pad(ind); printf("]\n");
    } else if(strcmp(s->kind,"While")==0){
        pad(ind); printf("While\n"); printExpr(s->whileCond, ind+2);
        pad(ind); printf("Body[\n"); for(int i=0;i<s->whileBlock.n;i++) printStmt(s->whileBlock.data[i], ind+2); pad(ind); printf("]\n");
    } else if(strcmp(s->kind,"For")==0){
        pad(ind); printf("For\n");
        pad(ind+2); printf("Init: ");
        if(s->hasInitDecl){ printf("Decl %s %s\n", s->forInitType?s->forInitType:"?", s->forInitName?s->forInitName:"?"); if(s->forInitExpr) printExpr(s->forInitExpr, ind+4); }
        else { printf("Expr\n"); if(s->expr) printExpr(s->expr, ind+4); }
        pad(ind+2); printf("Cond\n"); printExpr(s->forCond, ind+4);
        pad(ind+2); printf("Updt\n"); printExpr(s->forUpdt, ind+4);
        pad(ind);   printf("Body[\n"); for(int i=0;i<s->forBlock.n;i++) printStmt(s->forBlock.data[i], ind+2); pad(ind); printf("]\n");
    } else if(strcmp(s->kind,"Block")==0){
        pad(ind); printf("Block[\n"); for(int i=0;i<s->block.n;i++) printStmt(s->block.data[i], ind+2); pad(ind); printf("]\n");
    } else { pad(ind); printf("<?> stmt\n"); }
}

static void printDecl(Decl* d, int ind){
    if(!d){ pad(ind); printf("null\n"); return; }
    if(strcmp(d->kind,"FnDecl")==0){
        pad(ind); printf("FnDecl(type=%s, name=%s) params[\n", d->retType?d->retType:"?", d->fnName?d->fnName:"?");
        for(int i=0;i<d->params.n;i++){ pad(ind+2); printf("%s %s\n", d->params.types[i], d->params.names[i]); }
        pad(ind); printf("] body[\n"); for(int i=0;i<d->body.n;i++) printStmt(d->body.data[i], ind+2); pad(ind); printf("]\n");
    } else if(strcmp(d->kind,"VarDecl")==0){
        pad(ind); printf("VarDecl(type=%s, name=%s)\n", d->varType?d->varType:"?", d->varName?d->varName:"?"); if(d->varInit) printExpr(d->varInit, ind+2);
    } else if(strcmp(d->kind,"UsingNS")==0){
        pad(ind); printf("UsingNS(%s)\n", d->usingNS?d->usingNS:"?");
    } else { pad(ind); printf("<?> decl\n"); }
}

/* 31-based simple hash (for token prints) */
static int stringHash(const char* s) {
    unsigned int h = 0; for (const unsigned char* p=(const unsigned char*)s; *p; ++p) h = (h * 31u + *p) & 0x7fffffff; return (int)h;
}

/* ---------------- Lexer input buffer ---------------- */
static char *g_buf = NULL; static size_t g_len = 0; static size_t g_pos = 0; static int g_line = 1, g_col = 1;

/* Token printing */
static int tok_print_started = 0; static int tok_count = 0;
static void print_tok(const char* name, const char* lex, int val) {
    if (!tok_print_started) { printf("Tokens:\n["); tok_print_started = 1; tok_count = 0; }
    if (tok_count++ > 0) printf(", ");
    if (lex && *lex) printf("%s(\"%s\", v=%d)", name, lex, val);
    else             printf("%s(v=%d)", name, val);
}

/* Small helpers */
static int at_end(void){ return g_pos >= g_len; }
static int peekc(void){ return at_end()?0:g_buf[g_pos]; }
static int peek2(void){ return (g_pos+1<g_len)? g_buf[g_pos+1] : 0; }
static int adv(void){ if (at_end()) return 0; int c=g_buf[g_pos++]; if(c=='\n'){g_line++; g_col=1;} else g_col++; return c; }
static void skip_ws_and_comments(void){
    for(;;){
        int c = peekc();
        if (c==' '||c=='\t'||c=='\r'||c=='\n'||c=='\v'||c=='\f'){ adv(); continue; }
        if (c=='/' && peek2()=='/'){ while(peekc()!=0 && peekc()!='\n') adv(); continue; }
        if (c=='/' && peek2()=='*'){ adv(); adv(); for(;;){ if (at_end()){ fprintf(stderr,"Unterminated block comment at %d:%d\n", g_line,g_col); exit(1); } if (peekc()=='*' && peek2()=='/'){ adv(); adv(); break; } adv(); } continue; }
        break;
    }
}

/* forward decls */
static int  yylex(void);
static void yyerror(const char* s){ fprintf(stderr, "Parse error at %d:%d: %s\n", g_line, g_col, s); }

/* Bison will declare yylval with this union */
%}

/* ----------------------- Bison types & tokens ----------------------- */
%union {
    int   ival;
    char* sval;
    Expr* expr;
    Stmt* stmt;
    Decl* decl;
    VecExpr* veexpr;
    VecStmt* vestmt;
}

/* keywords / types */
%token T_INT T_FLOAT T_BOOL T_CHAR T_STRING T_VOID
%token T_IF T_ELSE T_WHILE T_FOR T_RETURN
%token T_USING T_NAMESPACE
/* preprocessor */
%token <sval> T_INCLUDE
%token <sval> T_PPDIRECTIVE

/* identifiers & literals (with payloads) */
%token <sval> T_IDENTIFIER T_STRINGLIT T_CHARLIT T_FLOATLIT
%token <ival> T_INTLIT T_BOOLLIT

/* multichar operators */
%token T_EQUALSOP T_NEQ T_LTE T_GTE T_ANDAND T_OROR T_SHL T_SHR
%token T_PLUSEQ T_MINUSEQ T_MULEQ T_DIVEQ T_MODEQ T_ANDEQ T_OREQ T_XOREQ
%token T_INC T_DEC T_ARROW
%token T_ASSIGNOP

%start program

/* precedence */
%right T_ASSIGNOP
%left  T_OROR
%left  T_ANDAND
%left  T_EQUALSOP T_NEQ
%left  '<' '>' T_LTE T_GTE
%left  '+' '-'
%left  '*' '/' '%'
%right UPLUS UMINUS '!'  /* unary */

/* dangling-else */
%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

/* nonterminal types */
%type <decl>   declaration function globalvar using_directive
%type <sval>   type
%type <vestmt> stmt_list block
%type <stmt>   statement
%type <expr>   expression assignment logic_or logic_and equality relational additive multiplicative unary postfix primary
%type <veexpr> opt_arg_list arg_list
%type <expr>   opt_expr
%type <stmt>   for_init
%type <expr>   init_opt

%%

program
    : decl_list
    {
        printf("AST:\n[\n");
        for(int i=0;i<g_program.n;i++) printDecl(g_program.data[i], 2);
        printf("]\n");
    }
    ;

decl_list
    : /* empty */
    | decl_list declaration
    ;

declaration
    : function
    | globalvar
    | using_directive
    | T_INCLUDE          { /* ignore in AST (already printed as token) */ }
    | T_PPDIRECTIVE      { /* ignore in AST */ }
    ;

using_directive
    : T_USING T_NAMESPACE T_IDENTIFIER ';'
      {
        Decl* d = mk_decl("UsingNS");
        d->usingNS = xstrdup($3);
        vd_push(&g_program, d);
        $$ = d;
      }
    ;

function
    : type T_IDENTIFIER '(' /*params skipped*/ ')' block
      {
        Decl* d = mk_decl("FnDecl");
        d->retType = xstrdup($1);
        d->fnName  = xstrdup($2);
        for(int i=0;i<$5->n;i++) vs_push(&d->body, $5->data[i]); /* block is $5 here */
        vd_push(&g_program, d);
        $$ = d;
      }
    ;

globalvar
    : type T_IDENTIFIER init_opt ';'
      {
        Decl* d = mk_decl("VarDecl");
        d->varType = xstrdup($1);
        d->varName = xstrdup($2);
        d->varInit = $3;   /* initializer expression or NULL */
        vd_push(&g_program, d);
        $$ = d;
      }
    ;

type
    : T_INT     { $$ = xstrdup("int"); }
    | T_FLOAT   { $$ = xstrdup("float"); }
    | T_BOOL    { $$ = xstrdup("bool"); }
    | T_CHAR    { $$ = xstrdup("char"); }
    | T_STRING  { $$ = xstrdup("string"); }
    | T_VOID    { $$ = xstrdup("void"); }
    ;

block
    : '{' stmt_list '}'         { $$ = $2; }
    ;

stmt_list
    : /* empty */               { $$ = (VecStmt*)xmalloc(sizeof(VecStmt)); $$->data=NULL; $$->n=0; $$->cap=0; }
    | stmt_list statement       { $$ = $1; vs_push($$, $2); }
    ;

statement
    : block
      { Stmt* s=mk_stmt("Block"); for(int i=0;i<$1->n;i++) vs_push(&s->block, $1->data[i]); $$=s; }
    | T_IF '(' expression ')' statement               %prec LOWER_THAN_ELSE
      { Stmt* s=mk_stmt("If"); s->ifCond=$3;
        if(strcmp($5->kind,"Block")==0) { for(int i=0;i<$5->block.n;i++) vs_push(&s->ifBlock,$5->block.data[i]); }
        else { vs_push(&s->ifBlock,$5); }
        $$=s;
      }
    | T_IF '(' expression ')' statement T_ELSE statement
      { Stmt* s=mk_stmt("If"); s->ifCond=$3;
        if(strcmp($5->kind,"Block")==0){ for(int i=0;i<$5->block.n;i++) vs_push(&s->ifBlock,$5->block.data[i]); } else { vs_push(&s->ifBlock,$5); }
        if(strcmp($7->kind,"Block")==0){ for(int i=0;i<$7->block.n;i++) vs_push(&s->elseBlock,$7->block.data[i]); } else { vs_push(&s->elseBlock,$7); }
        $$=s;
      }
    | T_WHILE '(' expression ')' statement
      { Stmt* s=mk_stmt("While"); s->whileCond=$3;
        if(strcmp($5->kind,"Block")==0){ for(int i=0;i<$5->block.n;i++) vs_push(&s->whileBlock,$5->block.data[i]); } else { vs_push(&s->whileBlock,$5); }
        $$=s;
      }
    | T_FOR '(' for_init ';' opt_expr ';' opt_expr ')' statement
      { Stmt* s=mk_stmt("For");
        if($3 && $3->kind && strcmp($3->kind,"VarDecl")==0){ s->hasInitDecl=1; s->forInitType=$3->typeTok; s->forInitName=$3->name; s->forInitExpr=$3->init; }
        else { s->expr=$3?$3->expr:NULL; }
        s->forCond=$5;
        s->forUpdt=$7;
        if(strcmp($9->kind,"Block")==0){ for(int i=0;i<$9->block.n;i++) vs_push(&s->forBlock,$9->block.data[i]); } else { vs_push(&s->forBlock,$9); }
        $$=s;
      }
    | T_RETURN opt_expr ';'
      { Stmt* s=mk_stmt("Return"); s->ret=$2; $$=s; }
    | type T_IDENTIFIER init_opt ';'
      { Stmt* s=mk_stmt("VarDecl"); s->typeTok=xstrdup($1); s->name=xstrdup($2); s->init=$3; $$=s; }
    | opt_expr ';'
      { Stmt* s=mk_stmt("ExprStmt"); s->expr=$1; $$=s; }
    ;

for_init
    : type T_IDENTIFIER init_opt
      { Stmt* s=mk_stmt("VarDecl"); s->typeTok=xstrdup($1); s->name=xstrdup($2); s->init=$3; $$=s; }
    | opt_expr
      { Stmt* s=mk_stmt("ExprStmt"); s->expr=$1; $$=s; }
    ;

opt_expr
    : /* empty */           { $$ = NULL; }
    | expression            { $$ = $1; }
    ;

init_opt
    : /* empty */            { $$ = NULL; }
    | T_ASSIGNOP expression  { $$ = $2;   }
    ;

/* ----------------- expressions ----------------- */
expression : assignment { $$ = $1; } ;

assignment
    : T_IDENTIFIER T_ASSIGNOP assignment   { $$ = mk_binary("T_ASSIGNOP", mk_ident($1), $3); }
    | logic_or                             { $$ = $1; }
    ;

logic_or
    : logic_and                            { $$ = $1; }
    | logic_or T_OROR logic_and            { $$ = mk_binary("T_OROR", $1, $3); }
    ;

logic_and
    : equality                             { $$ = $1; }
    | logic_and T_ANDAND equality          { $$ = mk_binary("T_ANDAND", $1, $3); }
    ;

equality
    : relational                           { $$ = $1; }
    | equality T_EQUALSOP relational       { $$ = mk_binary("T_EQUALSOP",$1,$3); }
    | equality T_NEQ relational            { $$ = mk_binary("T_NEQ",$1,$3); }
    ;

relational
    : additive                             { $$ = $1; }
    | relational '<'  additive             { $$ = mk_binary("T_LT",$1,$3); }
    | relational '>'  additive             { $$ = mk_binary("T_GT",$1,$3); }
    | relational T_LTE additive            { $$ = mk_binary("T_LTE",$1,$3); }
    | relational T_GTE additive            { $$ = mk_binary("T_GTE",$1,$3); }
    ;

additive
    : multiplicative                       { $$ = $1; }
    | additive '+' multiplicative          { $$ = mk_binary("T_PLUS",$1,$3); }
    | additive '-' multiplicative          { $$ = mk_binary("T_MINUS",$1,$3); }
    ;

multiplicative
    : unary                                { $$ = $1; }
    | multiplicative '*' unary             { $$ = mk_binary("T_STAR",$1,$3); }
    | multiplicative '/' unary             { $$ = mk_binary("T_SLASH",$1,$3); }
    | multiplicative '%' unary             { $$ = mk_binary("T_PERCENT",$1,$3); }
    ;

unary
    : '+' unary      %prec UPLUS           { $$ = mk_unary("T_PLUS",$2); }
    | '-' unary      %prec UMINUS          { $$ = mk_unary("T_MINUS",$2); }
    | '!' unary                            { $$ = mk_unary("T_BANG",$2); }
    | postfix                               { $$ = $1; }
    ;

postfix
    : primary                              { $$ = $1; }
    | postfix '(' opt_arg_list ')'
      { $$ = mk_call($1, *$3); }
    | postfix '[' expression ']'
      { $$ = mk_index($1, $3); }
    ;

opt_arg_list
    : /* empty */                          { $$ = (VecExpr*)xmalloc(sizeof(VecExpr)); $$->data=NULL; $$->n=0; $$->cap=0; }
    | arg_list                             { $$ = $1; }
    ;

arg_list
    : expression                           { $$ = (VecExpr*)xmalloc(sizeof(VecExpr)); $$->data=NULL; $$->n=0; $$->cap=0; ve_push($$, $1); }
    | arg_list ',' expression              { $$ = $1; ve_push($$, $3); }
    ;

primary
    : T_INTLIT                             { char buf[64]; snprintf(buf,sizeof(buf),"%d",$1); $$ = mk_lit("T_INTLIT", buf); }
    | T_FLOATLIT                           { $$ = mk_lit("T_FLOATLIT", $1); }
    | T_STRINGLIT                          { $$ = mk_lit("T_STRINGLIT", $1); }
    | T_CHARLIT                            { $$ = mk_lit("T_CHARLIT", $1); }
    | T_BOOLLIT                            { $$ = mk_lit("T_BOOLLIT", $1 ? "true":"false"); }
    | T_IDENTIFIER                         { $$ = mk_ident($1); }
    | '(' expression ')'                   { $$ = mk_group($2); }
    ;

%%

/* ======================= Hand-written lexer ======================= */

static int is_ident_start(int c){ return isalpha(c) || c=='_'; }
static int is_ident_cont (int c){ return isalnum(c) || c=='_'; }

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
    strncpy(out, name, n); out[n-1] = 0;
}

static int yylex(void){
    skip_ws_and_comments();
    if (at_end()){
        if (tok_print_started){ printf("]\n"); }  /* close token list */
        return 0; /* EOF */
    }

    int c = peekc();
    int d = peek2();

    /* ---------- preprocessor lines starting with '#' ---------- */
    if (c=='#'){
        adv(); /* consume '#' */
        while(peekc()==' '||peekc()=='\t') adv();

        char dir[64]; int di=0;
        while(isalpha(peekc()) && di<63){ dir[di++]=(char)adv(); }
        dir[di]=0;
        while(peekc()==' '||peekc()=='\t') adv();

        if (strcmp(dir,"include")==0){
            char hdr[512]; int hi=0;
            int q = peekc();
            if (q=='<' || q=='"'){
                int end = (q=='<')?'>':'"';
                adv(); /* consume opener */
                while(!at_end() && peekc()!=end && hi<511){ hdr[hi++]=(char)adv(); }
                hdr[hi]=0;
                if (peekc()==end) adv();
            } else {
                while(!at_end() && peekc()!='\n' && hi<511){ hdr[hi++]=(char)adv(); }
                hdr[hi]=0;
            }
            while(!at_end() && peekc()!='\n') adv();
            if(peekc()=='\n') adv();

            print_tok("T_INCLUDE", hdr, stringHash(hdr));
            yylval.sval = xstrdup(hdr);
            return T_INCLUDE;
        } else {
            char rest[1024]; int ri=0;
            while(!at_end() && peekc()!='\n' && ri<1023){ rest[ri++]=(char)adv(); }
            rest[ri]=0;
            if(peekc()=='\n') adv();

            print_tok("T_PPDIRECTIVE", rest, stringHash(rest));
            yylval.sval = xstrdup(rest);
            return T_PPDIRECTIVE;
        }
    }

    /* multi-char operators */
    {
        int tk=0;
        if (match_op2(c,d,&tk)){
            char lex[3] = { (char)c, (char)d, 0 };
            adv(); adv();
            char nm[32]; tok_name_for_op2(tk,nm,sizeof(nm));
            print_tok(nm, lex, 0);
            return tk;
        }
    }

    /* '=' as standalone */
    if (c=='='){ adv(); print_tok("T_ASSIGNOP", "=", 0); return T_ASSIGNOP; }

    /* single-char tokens */
    switch(c){
        case '(': adv(); print_tok("'('","",0); return '(';
        case ')': adv(); print_tok("')'","",0); return ')';
        case '{': adv(); print_tok("'{'","",0); return '{';
        case '}': adv(); print_tok("'}'","",0); return '}';
        case '[': adv(); print_tok("'['","",0); return '[';
        case ']': adv(); print_tok("']'","",0); return ']';
        case ';': adv(); print_tok("';'","",0); return ';';
        case ',': adv(); print_tok("','","",0); return ',';
        case ':': adv(); print_tok("':'","",0); return ':';
        case '.': adv(); print_tok("'.'","",0); return '.';
        case '+': adv(); print_tok("'+'","",0); return '+';
        case '-': adv(); print_tok("'-'","",0); return '-';
        case '*': adv(); print_tok("'*'","",0); return '*';
        case '/': adv(); print_tok("'/'","",0); return '/';
        case '%': adv(); print_tok("'%'","",0); return '%';
        case '<': adv(); print_tok("'<'","",0); return '<';
        case '>': adv(); print_tok("'>'","",0); return '>';
        case '!': adv(); print_tok("'!'","",0); return '!';
        case '&': adv(); print_tok("'&'","",0); return '&';
        case '|': adv(); print_tok("'|'","",0); return '|';
        case '^': adv(); print_tok("'^'","",0); return '^';
        case '~': adv(); print_tok("'~'","",0); return '~';
        case '?': adv(); print_tok("'?'","",0); return '?';

        case '\'': { /* char literal */
            adv();
            int ch = adv();
            if (ch=='\\'){
                int e=adv();
                ch = (e=='n')?'\n':(e=='t')?'\t':(e=='r')?'\r':(e=='\\')?'\\':(e=='\'')?'\'':(e=='"')?'"':e;
            }
            if (adv()!='\''){ fprintf(stderr,"Bad char literal at %d:%d\n", g_line,g_col); exit(1); }
            char buf[2]; buf[0]=(char)ch; buf[1]=0;
            print_tok("T_CHARLIT", buf, (unsigned char)ch);
            yylval.sval = xstrdup(buf);
            return T_CHARLIT;
        }

        case '"': { /* string literal */
            adv();
            char tmp[4096]; int len=0;
            while(!at_end() && peekc()!='"'){
                int ch = adv();
                if (ch=='\\'){
                    int e=adv();
                    ch = (e=='n')?'\n':(e=='t')?'\t':(e=='r')?'\r':(e=='\\')?'\\':(e=='\'')?'\'':(e=='"')?'"':e;
                }
                if (len<4095) tmp[len++]=(char)ch;
            }
            if (peekc()!='"'){ fprintf(stderr,"Unterminated string at %d:%d\n", g_line,g_col); exit(1); }
            adv();
            tmp[len]=0;
            print_tok("T_STRINGLIT", tmp, stringHash(tmp));
            yylval.sval = xstrdup(tmp);
            return T_STRINGLIT;
        }
    }

    /* number (int or float) */
    if (isdigit(c)){
        char tmp[256]; int len=0; int dot=0; int expn=0;
        while(isdigit(peekc())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        if (peekc()=='.' && isdigit(peek2())){ dot=1; if (len<255) tmp[len++]=(char)adv(); else adv();
            while(isdigit(peekc())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        }
        if (peekc()=='e' || peekc()=='E'){
            int t2 = g_pos+1<g_len? g_buf[g_pos+1] : 0;
            if (isdigit(peek2()) || ((t2=='+'||t2=='-') && (g_pos+2<g_len && isdigit(g_buf[g_pos+2])))){
                expn=1; if (len<255) tmp[len++]=(char)adv(); else adv();
                if (peekc()=='+'||peekc()=='-'){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
                if (!isdigit(peekc())){ fprintf(stderr,"Malformed exponent at %d:%d\n", g_line,g_col); exit(1); }
                while(isdigit(peekc())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
            }
        }
        tmp[len]=0;
        if (dot||expn){
            print_tok("T_FLOATLIT", tmp, stringHash(tmp));
            yylval.sval = xstrdup(tmp);
            return T_FLOATLIT;
        } else {
            int v = atoi(tmp);
            print_tok("T_INTLIT", tmp, v);
            yylval.ival = v;
            return T_INTLIT;
        }
    }

    /* identifier / keyword */
    if (is_ident_start(c)){
        char tmp[256]; int len=0;
        tmp[len++]=(char)adv();
        while(is_ident_cont(peekc())){ if (len<255) tmp[len++]=(char)adv(); else adv(); }
        tmp[len]=0;

        if (!strcmp(tmp,"int"))    { print_tok("T_INT",    "",0); return T_INT; }
        if (!strcmp(tmp,"float"))  { print_tok("T_FLOAT",  "",0); return T_FLOAT; }
        if (!strcmp(tmp,"bool"))   { print_tok("T_BOOL",   "",0); return T_BOOL; }
        if (!strcmp(tmp,"char"))   { print_tok("T_CHAR",   "",0); return T_CHAR; }
        if (!strcmp(tmp,"string")) { print_tok("T_STRING", "",0); return T_STRING; }
        if (!strcmp(tmp,"void"))   { print_tok("T_VOID",   "",0); return T_VOID; }

        if (!strcmp(tmp,"if"))     { print_tok("T_IF",     "",0); return T_IF; }
        if (!strcmp(tmp,"else"))   { print_tok("T_ELSE",   "",0); return T_ELSE; }
        if (!strcmp(tmp,"while"))  { print_tok("T_WHILE",  "",0); return T_WHILE; }
        if (!strcmp(tmp,"for"))    { print_tok("T_FOR",    "",0); return T_FOR; }
        if (!strcmp(tmp,"return")) { print_tok("T_RETURN", "",0); return T_RETURN; }

        if (!strcmp(tmp,"using"))     { print_tok("T_USING",     "",0); return T_USING; }
        if (!strcmp(tmp,"namespace")) { print_tok("T_NAMESPACE", "",0); return T_NAMESPACE; }

        if (!strcmp(tmp,"true"))   { print_tok("T_BOOLLIT","true",1);  yylval.ival=1; return T_BOOLLIT; }
        if (!strcmp(tmp,"false"))  { print_tok("T_BOOLLIT","false",0); yylval.ival=0; return T_BOOLLIT; }

        print_tok("T_IDENTIFIER", tmp, stringHash(tmp));
        yylval.sval = xstrdup(tmp);
        return T_IDENTIFIER;
    }

    fprintf(stderr,"Unexpected character '%c' (0x%02X) at %d:%d\n", peekc(), (unsigned)peekc(), g_line,g_col);
    adv();
    return yylex(); /* continue */
}

/* ======================= Driver ======================= */
int yyparse(void); /* generated */

int main(void){
    fseek(stdin, 0, SEEK_END); long sz = ftell(stdin); if (sz < 0) sz = 0;
    fseek(stdin, 0, SEEK_SET);
    g_len = (size_t)sz; g_buf = (char*)malloc(g_len + 1);
    if (!g_buf){ fprintf(stderr,"OOM\n"); return 1; }
    size_t n = fread(g_buf,1,g_len,stdin);
    g_buf[n] = 0; g_len = n; g_pos = 0; g_line=1; g_col=1;

    int rc = yyparse();
    if (rc==0){ printf("Parse OK\n"); }
    free(g_buf);
    return rc;
}
