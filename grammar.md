# Mini C++-like Language Grammar (BNF/EBNF)

This document defines the syntax of the simplified C++-like language supported by the lexer and parser implementation.  
It includes lexical rules, grammar, operator precedence, error types, and notes.

---

## Grammar Specification

```ebnf
(* ======================== *)
(* 1. Lexical Structure     *)
(* ======================== *)

letter        ::= "A"… "Z" | "a"… "z"
digit         ::= "0"… "9"
underscore    ::= "_"
non_ascii     ::= any UTF-8 codepoint > U+007F
ident_start   ::= letter | underscore | non_ascii
ident_cont    ::= ident_start | digit

IDENT         ::= ident_start ident_cont*

INTLIT        ::= digit+
HEXLIT        ::= "0x" hex_digit+ | "0X" hex_digit+
FLOATLIT      ::= digit+ "." digit+ ( ("e" | "E") ("+" | "-")? digit+ )?
                | digit+ ("e" | "E") ("+" | "-")? digit+
STRINGLIT     ::= '"' ( escape | ~["\\] )* '"'
CHARLIT       ::= "'" ( escape | ~['\\\n\r] ) "'"
BOOLLIT       ::= "true" | "false"

escape        ::= "\\" ( '"' | "\\" | "n" | "t" | "r" | "0"
                       | ("u" hex hex hex hex)
                       | ("U" hex hex hex hex hex hex hex hex) )
hex_digit     ::= "0"… "9" | "a"… "f" | "A"… "F"

LINE_COMMENT  ::= "//" [^\n]*
BLOCK_COMMENT ::= "/*" .*? "*/"

WS            ::= (" " | "\t" | "\r" | "\n" | "\v" | "\f")+

(* Keywords *)
Keyword       ::= "fn" | "return" | "if" | "else" | "for" | "while" 
                | "break" | "continue" | "switch" | "case" | "default" 
                | "do" | "const" | "let" | "var"
                | "int" | "float" | "double" | "bool" | "string" 
                | "char" | "void" | "long" | "short"

(* Operators & Punctuation *)
Punctuator    ::= "(" | ")" | "{" | "}" | "[" | "]" | ";" | "," | ":" | "."
                | "+" | "-" | "*" | "/" | "%"
                | "=" | "+=" | "-=" | "*=" | "/=" | "%="
                | "==" | "!=" | "<" | "<=" | ">" | ">="
                | "&&" | "||" | "!" | "~" | "&" | "|" | "^" | "<<" | ">>"
                | "?" 

(* ======================== *)
(* 2. Program Structure     *)
(* ======================== *)

Program       ::= (TopDecl | ";")* EOF

TopDecl       ::= FuncDecl "."
                | VarDecl  "."

(* ======================== *)
(* 3. Declarations          *)
(* ======================== *)

FuncDecl      ::= "fn" Type IDENT "(" ParamList? ")" Block
ParamList     ::= Param ( "," Param )*
Param         ::= Type IDENT

VarDecl       ::= Type IDENT ( "=" Expr )?
Type          ::= "int" | "float" | "double" | "bool" | "string" | "char"
                | "void" | "long" | "short"

(* ======================== *)
(* 4. Statements            *)
(* ======================== *)

Stmt          ::= Block
                | IfStmt
                | WhileStmt
                | ForStmt
                | ReturnStmt
                | BreakStmt
                | ContinueStmt
                | LocalVarDeclStmt
                | ExprStmt

Block         ::= "{" Stmt* "}"

IfStmt        ::= "if" "(" Expr ")" Stmt ( "else" Stmt )?

WhileStmt     ::= "while" "(" Expr ")" Stmt

ForStmt       ::= "for" "(" ForInit? ";" ForCond? ";" ForUpdate? ")" Stmt
ForInit       ::= LocalVarDecl | Expr
ForCond       ::= Expr
ForUpdate     ::= Expr

ReturnStmt    ::= "return" Expr? ";"
BreakStmt     ::= "break" ";"
ContinueStmt  ::= "continue" ";"

LocalVarDeclStmt ::= LocalVarDecl ";"
LocalVarDecl  ::= Type IDENT ( "=" Expr )?

ExprStmt      ::= Expr ";"

(* ======================== *)
(* 5. Expressions           *)
(* ======================== *)

Expr          ::= Assign
Assign        ::= Or ( AssignOp Assign )?
AssignOp      ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="

Or            ::= And ( "||" And )*
And           ::= Eq  ( "&&" Eq )*
Eq            ::= Rel ( ("==" | "!=") Rel )*
Rel           ::= Add ( ("<" | ">" | "<=" | ">=") Add )*
Add           ::= Mul ( ("+" | "-") Mul )*
Mul           ::= Unary ( ("*" | "/" | "%") Unary )*

Unary         ::= ("+" | "-" | "!") Unary
                | Postfix

Postfix       ::= Primary ( CallArgs | Index )*
CallArgs      ::= "(" ArgList? ")"
ArgList       ::= Expr ( "," Expr )*
Index         ::= "[" Expr "]"

Primary       ::= INTLIT | HEXLIT | FLOATLIT | STRINGLIT 
                | CHARLIT | BOOLLIT | IDENT 
                | "(" Expr ")"
