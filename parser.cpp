// Tiny C++-subset Lexer(s) + Recursive-Descent Parser (choice: manual or regex)
// Now also prints the token list before the AST.
//
// Compile:
//   g++ -std=c++17 -O2 -Wall -Wextra parser.cpp -o parser
//
// Scope (C++ subset):
//   - Types: int, float, bool, char, string, void
//   - Decls: function defs, var decl/assign
//   - Stmts: block { }, if/else, while, for, return, expr;
//   - Exprs: assignment (=), ||, &&, == !=, < > <= >=, + -, * / %,
//            unary + - !, calls (), indexing [], parentheses ()
//   - Literals: int, float, string "..." (with simple escapes), true/false, char 'a'
//   - Identifiers: ASCII [A-Za-z_][A-Za-z0-9_]*
//   - Comments: // line, /* block */

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cctype>
#include <stdexcept>
#include <sstream>
#include <limits>
#include <regex>
#include <memory>

using namespace std;

// ------------------------------ Tokens ------------------------------

struct Token
{
    string type;   // e.g., T_INT, T_IDENTIFIER, T_PARENL, ...
    string lexeme; // text
    int line = 1;
    int col = 1;
};

static string escapeLexeme(const string &s)
{
    string out;
    out.reserve(s.size());
    for (char c : s)
    {
        switch (c)
        {
        case '\"':
            out += "\\\"";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        case '\\':
            out += "\\\\";
            break;
        default:
            out.push_back(c);
            break;
        }
    }
    return out;
}

static string showToken(const Token &t)
{
    if (t.lexeme.empty())
        return t.type;
    // Put common lexical tokens with a quoted payload
    if (t.type == "T_IDENTIFIER" || t.type == "T_STRINGLIT" || t.type == "T_CHARLIT" || t.type == "T_BOOLLIT")
    {
        return t.type + "(\"" + escapeLexeme(t.lexeme) + "\")";
    }
    // Numbers and punctuation just show raw lexeme
    return t.type + "(" + escapeLexeme(t.lexeme) + ")";
}

struct LexError : runtime_error
{
    using runtime_error::runtime_error;
};

// Common keyword/operator tables (C++-subset)
static const unordered_map<string, string> KEYWORDS = {
    {"int", "T_INT"}, {"float", "T_FLOAT"}, {"bool", "T_BOOL"}, {"char", "T_CHAR"}, {"string", "T_STRING"}, {"void", "T_VOID"}, {"if", "T_IF"}, {"else", "T_ELSE"}, {"while", "T_WHILE"}, {"for", "T_FOR"}, {"return", "T_RETURN"}, {"true", "T_BOOLLIT"}, {"false", "T_BOOLLIT"}};

static const vector<pair<string, string>> MULTI_OPS = {
    {"==", "T_EQUALSOP"}, {"!=", "T_NEQ"}, {"<=", "T_LTE"}, {">=", "T_GTE"}, {"&&", "T_ANDAND"}, {"||", "T_OROR"}, {"<<", "T_SHL"}, {">>", "T_SHR"}, {"+=", "T_PLUSEQ"}, {"-=", "T_MINUSEQ"}, {"*=", "T_MULEQ"}, {"/=", "T_DIVEQ"}, {"%=", "T_MODEQ"}, {"&=", "T_ANDEQ"}, {"|=", "T_OREQ"}, {"^=", "T_XOREQ"}, {"++", "T_INC"}, {"--", "T_DEC"}, {"->", "T_ARROW"}};

static const unordered_map<char, string> SINGLE = {
    {'(', "T_PARENL"}, {')', "T_PARENR"}, {'{', "T_BRACEL"}, {'}', "T_BRACER"}, {'[', "T_BRACKETL"}, {']', "T_BRACKETR"}, {';', "T_SEMICOLON"}, {',', "T_COMMA"}, {':', "T_COLON"}, {'.', "T_DOT"}, {'+', "T_PLUS"}, {'-', "T_MINUS"}, {'*', "T_STAR"}, {'/', "T_SLASH"}, {'%', "T_PERCENT"}, {'<', "T_LT"}, {'>', "T_GT"}, {'=', "T_ASSIGNOP"}, {'!', "T_BANG"}, {'&', "T_AMP"}, {'|', "T_PIPE"}, {'^', "T_CARET"}, {'~', "T_TILDE"}, {'?', "T_QUESTION"}, {'\'', "T_SQUOTE"}, {'\"', "T_DQUOTE"}};

// ------------------------------ Lexer 1 (manual) ------------------------------

struct Lexer1
{
    string s;
    size_t i = 0;
    int line = 1, col = 1;

    char peek(size_t k = 0) const { return (i + k < s.size()) ? s[i + k] : '\0'; }
    void adv()
    {
        char c = peek();
        if (c == '\0')
            return;
        ++i;
        if (c == '\n')
        {
            ++line;
            col = 1;
        }
        else
        {
            ++col;
        }
    }
    bool starts(const string &t) const
    {
        if (i + t.size() > s.size())
            return false;
        for (size_t k = 0; k < t.size(); ++k)
            if (s[i + k] != t[k])
                return false;
        return true;
    }
    [[noreturn]] void err(const string &msg) const
    {
        ostringstream os;
        os << "Line " << line << ", Col " << col << ": " << msg;
        throw LexError(os.str());
    }
    Token make(const string &t, const string &lex = "") { return Token{t, lex, line, col}; }

    static bool isIdentStart(char c) { return std::isalpha((unsigned char)c) || c == '_'; }
    static bool isIdentCont(char c) { return std::isalnum((unsigned char)c) || c == '_'; }

    void skipWSandComments()
    {
        while (true)
        {
            char c = peek();
            // whitespace
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' || c == '\f')
            {
                adv();
                continue;
            }
            // line comment
            if (c == '/' && peek(1) == '/')
            {
                while (peek() != '\n' && peek() != '\0')
                    adv();
                continue;
            }
            // block comment
            if (c == '/' && peek(1) == '*')
            {
                adv();
                adv();
                while (true)
                {
                    if (peek() == '\0')
                        err("Unterminated block comment");
                    if (peek() == '*' && peek(1) == '/')
                    {
                        adv();
                        adv();
                        break;
                    }
                    adv();
                }
                continue;
            }
            break;
        }
    }

    Token number()
    {
        int L = line, C = col;
        size_t start = i;
        bool isFloat = false;

        while (isdigit((unsigned char)peek()))
            adv();

        if (peek() == '.' && isdigit((unsigned char)peek(1)))
        {
            isFloat = true;
            adv();
            while (isdigit((unsigned char)peek()))
                adv();
        }
        if (peek() == 'e' || peek() == 'E')
        {
            isFloat = true;
            if (isdigit((unsigned char)peek(1)) ||
                ((peek(1) == '+' || peek(1) == '-') && isdigit((unsigned char)peek(2))))
            {
                adv();
                if (peek() == '+' || peek() == '-')
                    adv();
                if (!isdigit((unsigned char)peek()))
                    err("Malformed exponent");
                while (isdigit((unsigned char)peek()))
                    adv();
            }
        }

        string lex = s.substr(start, i - start);
        return Token{isFloat ? "T_FLOATLIT" : "T_INTLIT", lex, L, C};
    }

    Token identOrKeyword()
    {
        int L = line, C = col;
        size_t start = i;
        if (!isIdentStart(peek()))
            err("Identifier expected");
        adv();
        while (isIdentCont(peek()))
            adv();
        string lex = s.substr(start, i - start);
        auto it = KEYWORDS.find(lex);
        if (it != KEYWORDS.end())
            return Token{it->second, lex, L, C};
        return Token{"T_IDENTIFIER", lex, L, C};
    }

    // "string" with simple escapes; 'c' for char
    Token stringOrChar()
    {
        char quote = peek();
        int L = line, C = col;
        adv(); // consume quote
        string buf;
        while (true)
        {
            char c = peek();
            if (c == '\0')
                err("Unterminated string/char literal");
            if (c == quote)
            {
                adv();
                break;
            }
            if (c == '\\')
            {
                adv();
                char e = peek();
                if (e == '\0')
                    err("Unterminated escape");
                switch (e)
                {
                case 'n':
                    buf.push_back('\n');
                    break;
                case 't':
                    buf.push_back('\t');
                    break;
                case 'r':
                    buf.push_back('\r');
                    break;
                case '\\':
                    buf.push_back('\\');
                    break;
                case '\'':
                    buf.push_back('\'');
                    break;
                case '\"':
                    buf.push_back('\"');
                    break;
                default:
                    buf.push_back(e);
                    break;
                }
                adv();
            }
            else
            {
                buf.push_back(c);
                adv();
            }
        }
        if (quote == '\"')
            return Token{"T_STRINGLIT", buf, L, C};
        return Token{"T_CHARLIT", buf, L, C};
    }

    vector<Token> tokenize()
    {
        vector<Token> out;
        while (true)
        {
            skipWSandComments();
            char c = peek();
            if (c == '\0')
                break;

            if (c == '\"' || c == '\'')
            {
                out.push_back(stringOrChar());
                continue;
            }
            if (isdigit((unsigned char)c))
            {
                out.push_back(number());
                continue;
            }
            if (isIdentStart(c))
            {
                out.push_back(identOrKeyword());
                continue;
            }

            bool matched = false;
            for (auto &p : MULTI_OPS)
            {
                if (starts(p.first))
                {
                    out.push_back(Token{p.second, p.first, line, col});
                    for (size_t k = 0; k < p.first.size(); ++k)
                        adv();
                    matched = true;
                    break;
                }
            }
            if (matched)
                continue;

            auto it = SINGLE.find(c);
            if (it != SINGLE.end())
            {
                out.push_back(Token{it->second, string(1, c), line, col});
                adv();
                continue;
            }

            err(string("Unexpected character '") + c + "'");
        }
        out.push_back(Token{"T_EOF", "", line, col});
        return out;
    }
};

// ------------------------------ Lexer 2 (regex) ------------------------------

struct Lexer2
{
    string s;
    size_t i = 0;
    int line = 1, col = 1;

    struct Rule
    {
        string name;
        regex re;
        bool skip = false;
    };

    vector<Rule> rules = {
        {"WS", regex(R"(^[ \t\r\n\v\f]+)"), true},
        {"LCOM", regex(R"(^//[^\n]*)"), false},
        {"BCOM", regex(R"(^/\[\s\S]?\*/)"), false},
        {"STR", regex(R"(^"(?:\\["'\\ntr]|\\.|[^"\\])*")"), false},
        {"CHR", regex(R"(^'(?:\\['"\\ntr]|\\.|[^'\\])')"), false},
        {"HEX", regex(R"(^0[xX][0-9A-Fa-f]+(?![A-Za-z0-9_]))"), false},
        {"DEC", regex(R"(^[0-9]+(?:\.[0-9]+)?(?:[eE][+\-]?[0-9]+)?(?![A-Za-z0-9_]))"), false},
        {"ID", regex(R"(^[A-Za-z_][A-Za-z0-9_]*)"), false},
        {"OP2", regex(R"(^(==|!=|<=|>=|&&|\|\||<<|>>|\+\+|--|->|\+=|-=|\*=|/=|%=|&=|\|=|\^=))"), false},
        {"OP1", regex(R"(^[()\{\}\[\];,:\.\+\-\*\/%<>=!&\|\^\~\?\'\"])"), false}};

    static void advPos(const string &lex, int &line, int &col)
    {
        for (char ch : lex)
        {
            if (ch == '\n')
            {
                ++line;
                col = 1;
            }
            else
            {
                ++col;
            }
        }
    }

    [[noreturn]] void err(const string &msg) const
    {
        ostringstream os;
        os << "Line " << line << ", Col " << col << ": " << msg;
        throw LexError(os.str());
    }

    vector<Token> tokenize()
    {
        vector<Token> out;
        while (i < s.size())
        {
            string rem = s.substr(i);
            bool any = false;

            for (const auto &rule : rules)
            {
                std::match_results<std::string::const_iterator> m;
                if (std::regex_search(rem, m, rule.re, std::regex_constants::match_continuous))
                {
                    string lex = m.str();
                    int L = line, C = col;

                    if (rule.skip)
                    {
                        advPos(lex, line, col);
                        i += lex.size();
                        any = true;
                        break;
                    }

                    if (rule.name == "LCOM" || rule.name == "BCOM")
                    {
                        // ignore comments for parser
                    }
                    else if (rule.name == "STR")
                    {
                        string inside = lex.substr(1, lex.size() - 2);
                        string un;
                        un.reserve(inside.size());
                        for (size_t k = 0; k < inside.size();)
                        {
                            char ch = inside[k++];
                            if (ch != '\\')
                            {
                                un.push_back(ch);
                                continue;
                            }
                            if (k >= inside.size())
                                err("Unterminated escape in string");
                            char e = inside[k++];
                            switch (e)
                            {
                            case 'n':
                                un.push_back('\n');
                                break;
                            case 't':
                                un.push_back('\t');
                                break;
                            case 'r':
                                un.push_back('\r');
                                break;
                            case '\\':
                                un.push_back('\\');
                                break;
                            case '\'':
                                un.push_back('\'');
                                break;
                            case '\"':
                                un.push_back('\"');
                                break;
                            default:
                                un.push_back(e);
                                break;
                            }
                        }
                        out.push_back(Token{"T_STRINGLIT", un, L, C});
                    }
                    else if (rule.name == "CHR")
                    {
                        string inside = lex.substr(1, lex.size() - 2);
                        string un;
                        if (inside.size() == 1 && inside[0] != '\\')
                            un.push_back(inside[0]);
                        else if (!inside.empty() && inside[0] == '\\')
                        {
                            char e = inside.size() > 1 ? inside[1] : '\0';
                            switch (e)
                            {
                            case 'n':
                                un.push_back('\n');
                                break;
                            case 't':
                                un.push_back('\t');
                                break;
                            case 'r':
                                un.push_back('\r');
                                break;
                            case '\\':
                                un.push_back('\\');
                                break;
                            case '\'':
                                un.push_back('\'');
                                break;
                            case '\"':
                                un.push_back('\"');
                                break;
                            default:
                                un.push_back(e);
                                break;
                            }
                        }
                        out.push_back(Token{"T_CHARLIT", un, L, C});
                    }
                    else if (rule.name == "HEX" || rule.name == "DEC")
                    {
                        bool isFloat = (rule.name == "DEC" &&
                                        (lex.find('.') != string::npos || lex.find('e') != string::npos || lex.find('E') != string::npos));
                        out.push_back(Token{isFloat ? "T_FLOATLIT" : "T_INTLIT", lex, L, C});
                    }
                    else if (rule.name == "ID")
                    {
                        auto it = KEYWORDS.find(lex);
                        if (it != KEYWORDS.end())
                            out.push_back(Token{it->second, lex, L, C});
                        else
                            out.push_back(Token{"T_IDENTIFIER", lex, L, C});
                    }
                    else if (rule.name == "OP2")
                    {
                        static const unordered_map<string, string> M2 = {
                            {"==", "T_EQUALSOP"}, {"!=", "T_NEQ"}, {"<=", "T_LTE"}, {">=", "T_GTE"}, {"&&", "T_ANDAND"}, {"||", "T_OROR"}, {"<<", "T_SHL"}, {">>", "T_SHR"}, {"++", "T_INC"}, {"--", "T_DEC"}, {"->", "T_ARROW"}, {"+=", "T_PLUSEQ"}, {"-=", "T_MINUSEQ"}, {"*=", "T_MULEQ"}, {"/=", "T_DIVEQ"}, {"%=", "T_MODEQ"}, {"&=", "T_ANDEQ"}, {"|=", "T_OREQ"}, {"^=", "T_XOREQ"}};
                        auto it = M2.find(lex);
                        if (it == M2.end())
                            err("Unknown operator: " + lex);
                        out.push_back(Token{it->second, lex, L, C});
                    }
                    else if (rule.name == "OP1")
                    {
                        char c = lex[0];
                        auto it = SINGLE.find(c);
                        if (it == SINGLE.end())
                            err(string("Unexpected character '") + c + "'");
                        out.push_back(Token{it->second, string(1, c), L, C});
                    }
                    else
                    {
                        err("Unhandled rule");
                    }

                    advPos(lex, line, col);
                    i += lex.size();
                    any = true;
                    break;
                }
            }

            if (!any)
            {
                ostringstream os;
                os << "Unexpected character '" << rem[0] << "'";
                err(os.str());
            }
        }
        out.push_back(Token{"T_EOF", "", line, col});
        return out;
    }
};

// ------------------------------ AST ------------------------------

struct Expr;
struct Stmt;
struct Decl;
using ExprPtr = shared_ptr<Expr>;
using StmtPtr = shared_ptr<Stmt>;
using DeclPtr = shared_ptr<Decl>;

struct Expr
{
    string kind;    // "Literal", "Identifier", "Unary", "Binary", "Call", "Index", "Grouping"
    string op;      // for unary/binary
    string litType; // T_INTLIT/T_FLOATLIT/T_STRINGLIT/T_BOOLLIT/T_CHARLIT
    string value;   // literal or identifier name
    vector<ExprPtr> args;
};

struct Stmt
{
    string kind; // "ExprStmt", "Return", "If", "While", "For", "Block", "VarDecl"
    // VarDecl
    string typeTok, name;
    ExprPtr init;
    // Return
    ExprPtr ret;
    // ExprStmt
    ExprPtr expr;
    // If
    ExprPtr ifCond;
    vector<StmtPtr> ifBlock, elseBlock;
    // While
    ExprPtr whileCond;
    vector<StmtPtr> whileBlock;
    // For
    bool hasInitDecl = false;
    string forInitType, forInitName;
    ExprPtr forInitExpr;
    ExprPtr forCond;
    ExprPtr forUpdt;
    vector<StmtPtr> forBlock;
    // Block
    vector<StmtPtr> block;
};

struct Decl
{
    string kind; // "FnDecl" or "VarDecl"
    // Fn
    string retType, fnName;
    vector<pair<string, string>> params;
    vector<StmtPtr> body;
    // Var
    string varType, varName;
    ExprPtr varInit;
};

// AST printers
static void printExpr(const ExprPtr &e, int ind = 0)
{
    auto pad = [&](int n)
    { for(int i=0;i<n;i++) cout<<' '; };
    if (!e)
    {
        pad(ind);
        cout << "null\n";
        return;
    }
    if (e->kind == "Literal")
    {
        pad(ind);
        cout << "Lit(" << e->litType << ":" << e->value << ")\n";
    }
    else if (e->kind == "Identifier")
    {
        pad(ind);
        cout << "Ident(" << e->value << ")\n";
    }
    else if (e->kind == "Unary")
    {
        pad(ind);
        cout << "Unary(" << e->op << ")\n";
        printExpr(e->args[0], ind + 2);
    }
    else if (e->kind == "Binary")
    {
        pad(ind);
        cout << "Binary(" << e->op << ")\n";
        printExpr(e->args[0], ind + 2);
        printExpr(e->args[1], ind + 2);
    }
    else if (e->kind == "Call")
    {
        pad(ind);
        cout << "Call(" << e->value << ") args[\n";
        for (auto &a : e->args)
            printExpr(a, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else if (e->kind == "Index")
    {
        pad(ind);
        cout << "Index\n";
        printExpr(e->args[0], ind + 2);
        printExpr(e->args[1], ind + 2);
    }
    else if (e->kind == "Grouping")
    {
        pad(ind);
        cout << "Grouping\n";
        printExpr(e->args[0], ind + 2);
    }
    else
    {
        pad(ind);
        cout << "<?> expr\n";
    }
}

static void printStmt(const StmtPtr &s, int ind = 0)
{
    auto pad = [&](int n)
    { for(int i=0;i<n;i++) cout<<' '; };
    if (!s)
    {
        pad(ind);
        cout << "null\n";
        return;
    }
    if (s->kind == "ExprStmt")
    {
        pad(ind);
        cout << "ExprStmt\n";
        printExpr(s->expr, ind + 2);
    }
    else if (s->kind == "Return")
    {
        pad(ind);
        cout << "Return\n";
        printExpr(s->ret, ind + 2);
    }
    else if (s->kind == "VarDecl")
    {
        pad(ind);
        cout << "VarDecl(type=" << s->typeTok << ", name=" << s->name << ")\n";
        if (s->init)
            printExpr(s->init, ind + 2);
    }
    else if (s->kind == "If")
    {
        pad(ind);
        cout << "If\n";
        printExpr(s->ifCond, ind + 2);
        pad(ind);
        cout << "Then[\n";
        for (auto &x : s->ifBlock)
            printStmt(x, ind + 2);
        pad(ind);
        cout << "]\n";
        pad(ind);
        cout << "Else[\n";
        for (auto &x : s->elseBlock)
            printStmt(x, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else if (s->kind == "While")
    {
        pad(ind);
        cout << "While\n";
        printExpr(s->whileCond, ind + 2);
        pad(ind);
        cout << "Body[\n";
        for (auto &x : s->whileBlock)
            printStmt(x, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else if (s->kind == "For")
    {
        pad(ind);
        cout << "For\n";
        pad(ind + 2);
        cout << "Init: ";
        if (s->hasInitDecl)
        {
            cout << "Decl " << s->forInitType << " " << s->forInitName << "\n";
            if (s->forInitExpr)
                printExpr(s->forInitExpr, ind + 4);
        }
        else
        {
            cout << "Expr\n";
            if (s->expr)
                printExpr(s->expr, ind + 4);
        }
        pad(ind + 2);
        cout << "Cond\n";
        printExpr(s->forCond, ind + 4);
        pad(ind + 2);
        cout << "Updt\n";
        printExpr(s->forUpdt, ind + 4);
        pad(ind);
        cout << "Body[\n";
        for (auto &x : s->forBlock)
            printStmt(x, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else if (s->kind == "Block")
    {
        pad(ind);
        cout << "Block[\n";
        for (auto &x : s->block)
            printStmt(x, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else
    {
        pad(ind);
        cout << "<?> stmt\n";
    }
}