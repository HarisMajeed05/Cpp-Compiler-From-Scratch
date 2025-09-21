This is the EBNF/BNF for the language accepted by the provided parser:
Top-level: function definitions and global var declarations
Statements: {}, if/else, while, for, return, expression/empty
Expressions: =, ||, &&, == !=, < > <= >=, + -, * / %, unary + - !, calls (), indexing [], grouping ( )
Literals: int, float, bool, char, string
Identifiers: ASCII [A-Za-z_][A-Za-z0-9_]*
Comments: // ... and /* ... */ are ignored by the lexer

1) Program & Declarations
program         ::= { declaration } EOF ;
declaration     ::= function
                  | global_var_decl ";" ;
function        ::= type identifier "(" [ parameter_list ] ")" block ;
parameter_list  ::= parameter { "," parameter } ;
parameter       ::= type identifier ;
global_var_decl ::= type identifier [ "=" expression ] ;

2) Statements
statement       ::= block
                  | "if" "(" expression ")" statement [ "else" statement ]
                  | "while" "(" expression ")" statement
                  | for_statement
                  | "return" [ expression ] ";"
                  | var_decl ";"
                  | [ expression ] ";"                 (* expression stmt or empty ";" *)
block           ::= "{" { statement } "}" ;
var_decl        ::= type identifier [ "=" expression ] ;
for_statement   ::= "for" "("
                      ( var_decl ";"                   (* decl-init * )
                      | [ expression ] ";" )           (* expr-init or empty * )
                      [ expression ] ";"               (* condition optional * )
                      [ expression ]                   (* update optional * )
                    ")" statement ;

3) Types
type            ::= "int" | "float" | "bool" | "char" | "string" | "void" ;

4) Expressions
expression      ::= assignment ;
assignment      ::= identifier "=" assignment          (* right-associative * )
                  | logic_or ;
logic_or        ::= logic_and { "||" logic_and } ;
logic_and       ::= equality  { "&&" equality  } ;
equality        ::= relational { ("==" | "!=") relational } ;
relational      ::= additive  { ("<" | ">" | "<=" | ">=") additive } ;
additive        ::= multiplicative { ("+" | "-") multiplicative } ;
multiplicative  ::= unary { ("*" | "/" | "%") unary } ;
unary           ::= ("+" | "-" | "!") unary
                  | postfix ;
postfix         ::= primary { call_suffix | index_suffix } ;
call_suffix     ::= "(" [ argument_list ] ")" ;
index_suffix    ::= "[" expression "]" ;
argument_list   ::= expression { "," expression } ;
primary         ::= literal
                  | identifier
                  | "(" expression ")" ;


5) Lexical (tokens)
identifier      ::= letter { letter | digit | "_" } ;
letter          ::= "A"…"Z" | "a"…"z" | "_" ;
digit           ::= "0"…"9" ;
int_lit         ::= digit { digit } ;
float_lit       ::= digit { digit }
                    [ "." digit { digit } ]
                    [ ("e" | "E") [ "+" | "-" ] digit { digit } ]
                  | digit { digit } "." digit { digit }
                    [ ("e" | "E") [ "+" | "-" ] digit { digit } ] ;
bool_lit        ::= "true" | "false" ;
char_lit        ::= "'" char_body "'" ;
char_body       ::= escape | any_char_except("'", "\\", newline) ;
string_lit      ::= "\"" { string_char } "\"" ;
string_char     ::= escape | any_char_except("\"", "\\", newline) ;
escape          ::= "\\" ( "n" | "t" | "r" | "\\" | "'" | "\"" | any_char ) ;
literal         ::= int_lit | float_lit | bool_lit | char_lit | string_lit ;
