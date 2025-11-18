// Tiny C++-subset Lexer(s) + Recursive-Descent Parser (choice: manual or regex)
// Prints the token list (with lexeme and int value) and then the AST.
// Now: comments produce T_COMMENT tokens and are printed, but are ignored by the parser.
// Also: #include / #... lines tokenized as T_INCLUDE / T_PPDIRECTIVE;
//       `using namespace <id>;` accepted at the top level.
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
//   - Literals: int, float, string "..." (escapes), true/false, char 'a'
//   - Identifiers: ASCII [A-Za-z_][A-Za-z0-9_]*
//   - Comments: // line, /* block */
//   - Preprocessor: #include <...> / "..." (T_INCLUDE) and generic #... (T_PPDIRECTIVE)
//   - Using-directive: using namespace IDENT ;

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
#include <fstream>
#include <iterator>

using namespace std;

static int stringHash(const string &str)
{
    long long h = 0;
    for (unsigned char c : str)
        h = (h * 31 + c) & 0x7fffffff;
    return static_cast<int>(h);
}

struct Token
{
    string type;
    int value = 0;
    string lexeme; // original text / unescaped content
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
        return t.type + "(v=" + to_string(t.value) + ")";
    // payload-in-quotes kinds:
    if (t.type == "T_IDENTIFIER" || t.type == "T_STRINGLIT" ||
        t.type == "T_CHARLIT" || t.type == "T_BOOLLIT" ||
        t.type == "T_COMMENT" || t.type == "T_INCLUDE" ||
        t.type == "T_PPDIRECTIVE")
    {
        return t.type + "(\"" + escapeLexeme(t.lexeme) + "\", v=" + to_string(t.value) + ")";
    }
    return t.type + "(" + escapeLexeme(t.lexeme) + ", v=" + to_string(t.value) + ")";
}

struct LexError : runtime_error
{
    using runtime_error::runtime_error;
};

static const unordered_map<string, string> KEYWORDS = {
    {"int", "T_INT"}, {"float", "T_FLOAT"}, {"bool", "T_BOOL"}, {"char", "T_CHAR"}, {"string", "T_STRING"}, {"void", "T_VOID"}, {"if", "T_IF"}, {"else", "T_ELSE"}, {"while", "T_WHILE"}, {"for", "T_FOR"}, {"return", "T_RETURN"}, {"true", "T_BOOLLIT"}, {"false", "T_BOOLLIT"}, {"using", "T_USING"}, {"namespace", "T_NAMESPACE"}, {"break", "T_BREAK"}};

static const vector<pair<string, string>> MULTI_OPS = {
    {"==", "T_EQUALSOP"}, {"!=", "T_NEQ"}, {"<=", "T_LTE"}, {">=", "T_GTE"}, {"&&", "T_ANDAND"}, {"||", "T_OROR"}, {"<<", "T_SHL"}, {">>", "T_SHR"}, {"**", "T_POW"}, {"+=", "T_PLUSEQ"}, {"-=", "T_MINUSEQ"}, {"*=", "T_MULEQ"}, {"/=", "T_DIVEQ"}, {"%=", "T_MODEQ"}, {"&=", "T_ANDEQ"}, {"|=", "T_OREQ"}, {"^=", "T_XOREQ"}, {"++", "T_INC"}, {"--", "T_DEC"}, {"->", "T_ARROW"}};

static const unordered_map<char, string> SINGLE = {
    {'(', "T_PARENL"}, {')', "T_PARENR"}, {'{', "T_BRACEL"}, {'}', "T_BRACER"}, {'[', "T_BRACKETL"}, {']', "T_BRACKETR"}, {';', "T_SEMICOLON"}, {',', "T_COMMA"}, {':', "T_COLON"}, {'.', "T_DOT"}, {'+', "T_PLUS"}, {'-', "T_MINUS"}, {'*', "T_STAR"}, {'/', "T_SLASH"}, {'%', "T_PERCENT"}, {'<', "T_LT"}, {'>', "T_GT"}, {'=', "T_ASSIGNOP"}, {'!', "T_BANG"}, {'&', "T_AMP"}, {'|', "T_PIPE"}, {'^', "T_CARET"}, {'~', "T_TILDE"}, {'?', "T_QUESTION"}, {'\'', "T_SQUOTE"}, {'\"', "T_DQUOTE"}};

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
    Token make(const string &t, const string &lex = "", int v = 0) { return Token{t, v, lex, line, col}; }

    static bool isIdentStart(char c) { return std::isalpha((unsigned char)c) || c == '_'; }
    static bool isIdentCont(char c) { return std::isalnum((unsigned char)c) || c == '_'; }

    void skipWS()
    {
        while (true)
        {
            char c = peek();
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' || c == '\f')
            {
                adv();
                continue;
            }
            break;
        }
    }

    Token lineComment()
    {
        int L = line, C = col;
        adv();
        adv(); // //
        string buf;
        while (peek() != '\n' && peek() != '\0')
        {
            buf.push_back(peek());
            adv();
        }
        return Token{"T_COMMENT", stringHash(buf), buf, L, C};
    }

    Token blockComment()
    {
        int L = line, C = col;
        adv();
        adv(); // /*
        string buf;
        while (true)
        {
            char c = peek();
            if (c == '\0')
                err("Unterminated block comment");
            if (c == '*' && peek(1) == '/')
            {
                adv();
                adv();
                break;
            }
            buf.push_back(c);
            adv();
        }
        return Token{"T_COMMENT", stringHash(buf), buf, L, C};
    }

    // #include and generic #...
    Token preproc()
    {
        int L = line, C = col;
        // consume '#'
        adv();
        // read the directive name
        while (peek() == ' ' || peek() == '\t')
            adv();
        string name;
        while (isIdentCont(peek()))
        {
            name.push_back(peek());
            adv();
        }

        // Only capture the rest of the line for simplicity
        while (peek() == ' ' || peek() == '\t')
            adv();

        if (name == "include")
        {
            string header;
            if (peek() == '<')
            {
                adv(); // <
                while (peek() != '>' && peek() != '\0' && peek() != '\n')
                {
                    header.push_back(peek());
                    adv();
                }
                if (peek() != '>')
                    err("Unterminated #include <...>");
                adv(); // >
            }
            else if (peek() == '\"')
            {
                adv(); // "
                while (peek() != '\"' && peek() != '\0' && peek() != '\n')
                {
                    header.push_back(peek());
                    adv();
                }
                if (peek() != '\"')
                    err("Unterminated #include \"...\"");
                adv(); // "
            }
            else
            {
                // Fallback: grab to end-of-line
                while (peek() != '\n' && peek() != '\0')
                {
                    header.push_back(peek());
                    adv();
                }
            }
            // eat rest-of-line newline if present
            if (peek() == '\n')
                adv();
            return Token{"T_INCLUDE", stringHash(header), header, L, C};
        }
        else
        {
            string rest;
            while (peek() != '\n' && peek() != '\0')
            {
                rest.push_back(peek());
                adv();
            }
            if (peek() == '\n')
                adv();
            string lex = name + " " + rest;
            return Token{"T_PPDIRECTIVE", stringHash(lex), lex, L, C};
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
        if (isFloat)
            return Token{"T_FLOATLIT", stringHash(lex), lex, L, C};
        int val = 0;
        try
        {
            val = stoi(lex);
        }
        catch (...)
        {
            err("Integer out of range");
        }
        return Token{"T_INTLIT", val, lex, L, C};
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
        {
            if (it->second == "T_BOOLLIT")
                return Token{"T_BOOLLIT", (lex == "true") ? 1 : 0, lex, L, C};
            return Token{it->second, 0, lex, L, C};
        }
        return Token{"T_IDENTIFIER", stringHash(lex), lex, L, C};
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
            return Token{"T_STRINGLIT", stringHash(buf), buf, L, C};
        int v = !buf.empty() ? static_cast<unsigned char>(buf[0]) : 0;
        return Token{"T_CHARLIT", v, buf, L, C};
    }

    vector<Token> tokenize()
    {
        vector<Token> out;
        while (true)
        {
            skipWS();
            char c = peek();
            if (c == '\0')
                break;

            // Preprocessor lines
            if (c == '#')
            {
                out.push_back(preproc());
                continue;
            }

            // Comments (produce tokens)
            if (c == '/' && peek(1) == '/')
            {
                out.push_back(lineComment());
                continue;
            }
            if (c == '/' && peek(1) == '*')
            {
                out.push_back(blockComment());
                continue;
            }

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
                    out.push_back(Token{p.second, 0, p.first, line, col});
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
                out.push_back(Token{it->second, 0, string(1, c), line, col});
                adv();
                continue;
            }

            err(string("Unexpected character '") + c + "'");
        }
        out.push_back(Token{"T_EOF", 0, "", line, col});
        return out;
    }
};

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
        // Preprocessor (#include <...> or "...") and generic #...
        {"INCA", regex(R"(^\#\s*include\s*<[^>\n]+>)")}, // angle
        {"INCS", regex(R"(^\#\s*include\s*"[^"\n]+")")}, // quoted
        {"PP", regex(R"(^\#[^\n]*)")},                   // any other preprocessor line
        {"LCOM", regex(R"(^//[^\n]*)")},
        {"BCOM", regex(R"(^/\*([^*]|\*+[^*/])*\*/)")},
        {"STR", regex(R"(^"(?:\\["'\\ntr]|\\.|[^"\\])*")")},
        {"CHR", regex(R"(^'(?:\\['"\\ntr]|\\.|[^'\\])')")},
        {"HEX", regex(R"(^0[xX][0-9A-Fa-f]+(?![A-Za-z0-9_]))")},
        {"DEC", regex(R"(^[0-9]+(?:\.[0-9]+)?(?:[eE][+\-]?[0-9]+)?(?![A-Za-z0-9_]))")},
        {"ID", regex(R"(^[A-Za-z_][A-Za-z0-9_]*)")},
        {"OP2", regex(R"(^(==|!=|<=|>=|&&|\|\||<<|>>|\+\+|--|->|\*\*|\+=|-=|\*=|/=|%=|&=|\|=|\^=))")},
        {"OP1", regex(R"(^[()\{\}\[\];,:\.\+\-\*\/%<>=!&\|\^\~\?\'\"])")}};

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

                    if (rule.name == "INCA" || rule.name == "INCS")
                    {
                        // Extract header name without delimiters
                        string inner = lex;
                        size_t lt = inner.find('<'), gt = inner.rfind('>');
                        if (lt != string::npos && gt != string::npos && gt > lt)
                            inner = inner.substr(lt + 1, gt - lt - 1);
                        else
                        {
                            size_t q1 = lex.find('"'), q2 = lex.rfind('"');
                            if (q1 != string::npos && q2 != string::npos && q2 > q1)
                                inner = lex.substr(q1 + 1, q2 - q1 - 1);
                        }
                        out.push_back(Token{"T_INCLUDE", stringHash(inner), inner, L, C});
                    }
                    else if (rule.name == "PP")
                    {
                        string inner = lex.substr(1); // after '#'
                        out.push_back(Token{"T_PPDIRECTIVE", stringHash(inner), inner, L, C});
                    }
                    else if (rule.name == "LCOM" || rule.name == "BCOM")
                    {
                        string inner = (rule.name == "LCOM") ? lex.substr(2) : lex.substr(2, lex.size() - 4);
                        out.push_back(Token{"T_COMMENT", stringHash(inner), inner, L, C});
                    }
                    else if (rule.name == "STR")
                    {
                        string inside = lex.substr(1, lex.size() - 2), un;
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
                        out.push_back(Token{"T_STRINGLIT", stringHash(un), un, L, C});
                    }
                    else if (rule.name == "CHR")
                    {
                        string inside = lex.substr(1, lex.size() - 2), un;
                        if (inside.size() == 1 && inside[0] != '\\')
                            un.push_back(inside[0]);
                        else if (!inside.empty() && inside[0] == '\\')
                        {
                            char e = (inside.size() > 1 ? inside[1] : '\0');
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
                        int v = !un.empty() ? static_cast<unsigned char>(un[0]) : 0;
                        out.push_back(Token{"T_CHARLIT", v, un, L, C});
                    }
                    else if (rule.name == "HEX")
                    {
                        long long v = 0;
                        for (size_t j = 2; j < lex.size(); ++j)
                        {
                            char c = lex[j];
                            int hv = (c >= '0' && c <= '9')   ? c - '0'
                                     : (c >= 'a' && c <= 'f') ? 10 + (c - 'a')
                                     : (c >= 'A' && c <= 'F') ? 10 + (c - 'A')
                                                              : 0;
                            v = (v << 4) + hv;
                            if (v > std::numeric_limits<int>::max())
                                err("Integer out of range");
                        }
                        out.push_back(Token{"T_INTLIT", static_cast<int>(v), lex, L, C});
                    }
                    else if (rule.name == "DEC")
                    {
                        bool isFloat = (lex.find('.') != string::npos) || (lex.find('e') != string::npos) || (lex.find('E') != string::npos);
                        if (isFloat)
                            out.push_back(Token{"T_FLOATLIT", stringHash(lex), lex, L, C});
                        else
                        {
                            int v = 0;
                            try
                            {
                                v = stoi(lex);
                            }
                            catch (...)
                            {
                                err("Integer out of range");
                            }
                            out.push_back(Token{"T_INTLIT", v, lex, L, C});
                        }
                    }
                    else if (rule.name == "ID")
                    {
                        auto it = KEYWORDS.find(lex);
                        if (it != KEYWORDS.end())
                        {
                            if (it->second == "T_BOOLLIT")
                                out.push_back(Token{"T_BOOLLIT", (lex == "true") ? 1 : 0, lex, L, C});
                            else
                                out.push_back(Token{it->second, 0, lex, L, C});
                        }
                        else
                            out.push_back(Token{"T_IDENTIFIER", stringHash(lex), lex, L, C});
                    }
                    else if (rule.name == "OP2")
                    {
                        static const unordered_map<string, string> M2 = {
                            {"==", "T_EQUALSOP"}, {"!=", "T_NEQ"}, {"<=", "T_LTE"}, {">=", "T_GTE"}, {"&&", "T_ANDAND"}, {"||", "T_OROR"}, {"<<", "T_SHL"}, {">>", "T_SHR"}, {"++", "T_INC"}, {"--", "T_DEC"}, {"->", "T_ARROW"}, {"**", "T_POW"}, {"+=", "T_PLUSEQ"}, {"-=", "T_MINUSEQ"}, {"*=", "T_MULEQ"}, {"/=", "T_DIVEQ"}, {"%=", "T_MODEQ"}, {"&=", "T_ANDEQ"}, {"|=", "T_OREQ"}, {"^=", "T_XOREQ"}};
                        auto it = M2.find(lex);
                        if (it == M2.end())
                            err("Unknown operator: " + lex);
                        out.push_back(Token{it->second, 0, lex, L, C});
                    }
                    else if (rule.name == "OP1")
                    {
                        char c = lex[0];
                        auto it = SINGLE.find(c);
                        if (it == SINGLE.end())
                            err(string("Unexpected character '") + c + "'");
                        out.push_back(Token{it->second, 0, string(1, c), L, C});
                    }
                    else
                        err("Unhandled rule");

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
        out.push_back(Token{"T_EOF", 0, "", line, col});
        return out;
    }
};

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
    string value;   // keep text for printing
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
    else if (s->kind == "Break")
    {
        pad(ind);
        cout << "Break\n";
    }
    else
    {
        pad(ind);
        cout << "<?> stmt\n";
    }
}

static void printDecl(const DeclPtr &d, int ind = 0)
{
    auto pad = [&](int n)
    { for(int i=0;i<n;i++) cout<<' '; };
    if (!d)
    {
        pad(ind);
        cout << "null\n";
        return;
    }
    if (d->kind == "FnDecl")
    {
        pad(ind);
        cout << "FnDecl(type=" << d->retType << ", name=" << d->fnName << ") params[\n";
        for (auto &p : d->params)
        {
            pad(ind + 2);
            cout << p.first << " " << p.second << "\n";
        }
        pad(ind);
        cout << "] body[\n";
        for (auto &s : d->body)
            printStmt(s, ind + 2);
        pad(ind);
        cout << "]\n";
    }
    else if (d->kind == "VarDecl")
    {
        pad(ind);
        cout << "VarDecl(type=" << d->varType << ", name=" << d->varName << ")\n";
        if (d->varInit)
            printExpr(d->varInit, ind + 2);
    }
    else
    {
        pad(ind);
        cout << "<?> decl\n";
    }
}

// ------------------------------ Parser ------------------------------

struct ParseError : runtime_error
{
    using runtime_error::runtime_error;
};

struct Parser
{
    vector<Token> ts;
    size_t i = 0;
    Parser() = default;
    explicit Parser(vector<Token> toks) : ts(move(toks)) {}

    const Token &peek(size_t k = 0) const
    {
        static Token eof{"T_EOF", 0, "", 0, 0};
        return (i + k < ts.size() ? ts[i + k] : eof);
    }
    bool match(const string &t)
    {
        if (peek().type == t)
        {
            ++i;
            return true;
        }
        return false;
    }
    const Token &expect(const string &t, const string &msg)
    {
        if (!match(t))
            throw ParseError(msg + " at " + toLoc(peek()));
        return ts[i - 1];
    }
    static string toLoc(const Token &t)
    {
        ostringstream os;
        os << "(line " << t.line << ", col " << t.col << ")";
        return os.str();
    }

    bool isTypeTok(const Token &t) const
    {
        static const unordered_set<string> types = {"T_INT", "T_FLOAT", "T_BOOL", "T_CHAR", "T_STRING", "T_VOID"};
        return types.count(t.type) > 0;
    }

    // Skip tokens that should be ignored at the top-level (includes, generic pp, comments)
    bool skipTopNoise()
    {
        bool any = false;
        while (true)
        {
            if (match("T_INCLUDE") || match("T_PPDIRECTIVE") || match("T_COMMENT"))
            {
                any = true;
                continue;
            }

            // using namespace IDENT ;
            if (peek().type == "T_USING" && peek(1).type == "T_NAMESPACE")
            {
                i += 2; // consume using namespace
                if (peek().type != "T_IDENTIFIER")
                    throw ParseError("Expected identifier after 'using namespace' " + toLoc(peek()));
                ++i; // ident
                expect("T_SEMICOLON", "Expected ';' after using-directive");
                any = true;
                continue;
            }
            break;
        }
        return any;
    }

    vector<DeclPtr> parseProgram()
    {
        vector<DeclPtr> out;
        while (peek().type != "T_EOF")
        {
            if (skipTopNoise())
                continue;

            if (isTypeTok(peek()))
            {
                if (peek(1).type == "T_IDENTIFIER" && peek(2).type == "T_PARENL")
                    out.push_back(parseFunc());
                else
                {
                    out.push_back(parseTopVarDecl());
                    expect("T_SEMICOLON", "Expected ';' after variable declaration");
                }
            }
            else
                throw ParseError(string("Expected declaration at ") + toLoc(peek()));
        }
        return out;
    }

    DeclPtr parseFunc()
    {
        string ty = peek().lexeme;
        expect(peek().type, "Expected type");
        Token name = expect("T_IDENTIFIER", "Expected function name");
        expect("T_PARENL", "Expected '('");
        vector<pair<string, string>> params;
        if (peek().type != "T_PARENR")
        {
            while (true)
            {
                if (!isTypeTok(peek()))
                    throw ParseError("Expected parameter type " + toLoc(peek()));
                string pty = peek().lexeme;
                expect(peek().type, "param type");
                Token pid = expect("T_IDENTIFIER", "Expected parameter name");
                params.push_back({pty, pid.lexeme});
                if (!match("T_COMMA"))
                    break;
            }
        }
        expect("T_PARENR", "Expected ')'");
        auto body = parseBlock();

        auto d = make_shared<Decl>();
        d->kind = "FnDecl";
        d->retType = ty;
        d->fnName = name.lexeme;
        d->params = params;
        d->body = move(body);
        return d;
    }

    DeclPtr parseTopVarDecl()
    {
        string ty = peek().lexeme;
        expect(peek().type, "Expected type");
        Token id = expect("T_IDENTIFIER", "Expected variable name");
        ExprPtr init;
        if (match("T_ASSIGNOP"))
            init = parseExpr();
        auto d = make_shared<Decl>();
        d->kind = "VarDecl";
        d->varType = ty;
        d->varName = id.lexeme;
        d->varInit = init;
        return d;
    }

    vector<StmtPtr> parseBlock()
    {
        expect("T_BRACEL", "Expected '{'");
        vector<StmtPtr> out;
        while (peek().type != "T_BRACER")
        {
            // allow comments or preprocessor lines inside blocks, just skip them
            if (match("T_COMMENT") || match("T_PPDIRECTIVE") || match("T_INCLUDE"))
                continue;
            out.push_back(parseStmt());
        }
        expect("T_BRACER", "Expected '}'");
        return out;
    }

    StmtPtr parseStmt()
    {
        if (peek().type == "T_BRACEL")
        {
            auto s = make_shared<Stmt>();
            s->kind = "Block";
            s->block = parseBlock();
            return s;
        }
        if (peek().type == "T_BREAK")
        {
            i++;
            expect("T_SEMICOLON", "Expected ';' after break");
            auto s = make_shared<Stmt>();
            s->kind = "Break";
            return s;
        }

        if (peek().type == "T_IF")
        {
            i++;
            expect("T_PARENL", "Expected '(' after if");
            auto cond = parseExpr();
            expect("T_PARENR", "Expected ')'");
            auto thenS = parseStmt();
            vector<StmtPtr> elseBlk;
            if (match("T_ELSE"))
            {
                auto e = parseStmt();
                elseBlk = (e->kind == "Block") ? e->block : vector<StmtPtr>{e};
            }
            auto s = make_shared<Stmt>();
            s->kind = "If";
            s->ifCond = cond;
            s->ifBlock = (thenS->kind == "Block") ? thenS->block : vector<StmtPtr>{thenS};
            s->elseBlock = move(elseBlk);
            return s;
        }

        if (peek().type == "T_WHILE")
        {
            i++;
            expect("T_PARENL", "Expected '(' after while");
            auto cond = parseExpr();
            expect("T_PARENR", "Expected ')'");
            auto body = parseStmt();
            auto s = make_shared<Stmt>();
            s->kind = "While";
            s->whileCond = cond;
            s->whileBlock = (body->kind == "Block") ? body->block : vector<StmtPtr>{body};
            return s;
        }

        if (peek().type == "T_FOR")
        {
            i++;
            expect("T_PARENL", "Expected '(' after for");
            auto s = make_shared<Stmt>();
            s->kind = "For";
            if (isTypeTok(peek()))
            {
                s->hasInitDecl = true;
                string ty = peek().lexeme;
                expect(peek().type, "type");
                Token id = expect("T_IDENTIFIER", "Expected identifier");
                s->forInitType = ty;
                s->forInitName = id.lexeme;
                if (match("T_ASSIGNOP"))
                    s->forInitExpr = parseExpr();
                expect("T_SEMICOLON", "Expected ';' after for-init");
            }
            else
            {
                if (peek().type != "T_SEMICOLON")
                    s->expr = parseExpr();
                expect("T_SEMICOLON", "Expected ';' after for-init expr");
            }
            if (peek().type != "T_SEMICOLON")
                s->forCond = parseExpr();
            expect("T_SEMICOLON", "Expected ';' after for-cond");
            if (peek().type != "T_PARENR")
                s->forUpdt = parseExpr();
            expect("T_PARENR", "Expected ')'");
            auto body = parseStmt();
            s->forBlock = (body->kind == "Block") ? body->block : vector<StmtPtr>{body};
            return s;
        }

        if (peek().type == "T_RETURN")
        {
            i++;
            ExprPtr e;
            if (peek().type != "T_SEMICOLON")
                e = parseExpr();
            expect("T_SEMICOLON", "Expected ';' after return");
            auto s = make_shared<Stmt>();
            s->kind = "Return";
            s->ret = e;
            return s;
        }

        if (isTypeTok(peek()))
        {
            string ty = peek().lexeme;
            expect(peek().type, "type");
            Token id = expect("T_IDENTIFIER", "Expected identifier");
            ExprPtr init;
            if (match("T_ASSIGNOP"))
                init = parseExpr();
            expect("T_SEMICOLON", "Expected ';' after declaration");
            auto s = make_shared<Stmt>();
            s->kind = "VarDecl";
            s->typeTok = ty;
            s->name = id.lexeme;
            s->init = init;
            return s;
        }

        if (peek().type != "T_SEMICOLON")
        {
            auto e = parseExpr();
            expect("T_SEMICOLON", "Expected ';' after expression");
            auto s = make_shared<Stmt>();
            s->kind = "ExprStmt";
            s->expr = e;
            return s;
        }
        expect("T_SEMICOLON", "Expected ';'");
        auto s = make_shared<Stmt>();
        s->kind = "ExprStmt";
        return s;
    }

    // Expressions (precedence climbing)
    ExprPtr parseExpr() { return parseAssign(); }

    ExprPtr parseAssign()
    {
        auto lhs = parseLOr();
        if (match("T_ASSIGNOP"))
        {
            Token op = ts[i - 1];
            auto rhs = parseAssign(); // right-assoc
            auto e = make_shared<Expr>();
            e->kind = "Binary";
            e->op = op.type;
            e->args = {lhs, rhs};
            return e;
        }
        return lhs;
    }

    ExprPtr parseLOr()
    {
        auto e = parseLAnd();
        while (match("T_OROR"))
        {
            Token op = ts[i - 1];
            auto r = parseLAnd();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseLAnd()
    {
        auto e = parseBOr();
        while (match("T_ANDAND"))
        {
            Token op = ts[i - 1];
            auto r = parseBOr();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseBOr()
    {
        auto e = parseBXor();
        while (match("T_PIPE"))
        {
            Token op = ts[i - 1];
            auto r = parseBXor();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseBXor()
    {
        auto e = parseBAnd();
        while (match("T_CARET"))
        {
            Token op = ts[i - 1];
            auto r = parseBAnd();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseBAnd()
    {
        auto e = parseEq();
        while (match("T_AMP"))
        {
            Token op = ts[i - 1];
            auto r = parseEq();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseEq()
    {
        auto e = parseRel();
        while (peek().type == "T_EQUALSOP" || peek().type == "T_NEQ")
        {
            Token op = peek();
            i++;
            auto r = parseRel();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseRel()
    {
        auto e = parseShift();
        while (peek().type == "T_LT" || peek().type == "T_GT" ||
               peek().type == "T_LTE" || peek().type == "T_GTE")
        {
            Token op = peek();
            i++;
            auto r = parseShift();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseShift()
    {
        auto e = parseAdd();
        while (peek().type == "T_SHL" || peek().type == "T_SHR")
        {
            Token op = peek();
            i++;
            auto r = parseAdd();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseAdd()
    {
        auto e = parseMul();
        while (peek().type == "T_PLUS" || peek().type == "T_MINUS")
        {
            Token op = peek();
            i++;
            auto r = parseMul();
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parseMul()
    {
        auto e = parsePow(); // was: parseUnary()
        while (peek().type == "T_STAR" || peek().type == "T_SLASH" || peek().type == "T_PERCENT")
        {
            Token op = peek();
            i++;
            auto r = parsePow(); // was: parseUnary()
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            e = n;
        }
        return e;
    }

    ExprPtr parsePow()
    {
        auto e = parseUnary();
        if (match("T_POW"))
        {
            Token op = ts[i - 1];
            auto r = parsePow(); // right-associative
            auto n = make_shared<Expr>();
            n->kind = "Binary";
            n->op = op.type;
            n->args = {e, r};
            return n;
        }
        return e;
    }

    ExprPtr parseUnary()
    {
        if (peek().type == "T_PLUS" || peek().type == "T_MINUS" ||
            peek().type == "T_BANG" || peek().type == "T_TILDE")
        { // add '~'
            Token op = peek();
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Unary";
            e->op = op.type;
            e->args = {parseUnary()};
            return e;
        }
        return parsePostfix();
    }

    ExprPtr parsePostfix()
    {
        auto e = parsePrimary();
        while (true)
        {
            if (match("T_PARENL"))
            {
                auto call = make_shared<Expr>();
                call->kind = "Call";
                if (e->kind == "Identifier")
                    call->value = e->value;
                else
                    throw ParseError("Call target must be identifier at " + toLoc(peek()));
                if (peek().type != "T_PARENR")
                {
                    while (true)
                    {
                        call->args.push_back(parseExpr());
                        if (!match("T_COMMA"))
                            break;
                    }
                }
                expect("T_PARENR", "Expected ')'");
                e = call;
                continue;
            }
            if (match("T_BRACKETL"))
            {
                auto idx = make_shared<Expr>();
                idx->kind = "Index";
                idx->args.push_back(e);
                idx->args.push_back(parseExpr());
                expect("T_BRACKETR", "Expected ']'");
                e = idx;
                continue;
            }
            break;
        }
        return e;
    }
    ExprPtr parsePrimary()
    {
        const Token &t = peek();
        if (t.type == "T_INTLIT")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Literal";
            e->litType = "T_INTLIT";
            e->value = t.lexeme;
            return e;
        }
        if (t.type == "T_FLOATLIT")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Literal";
            e->litType = "T_FLOATLIT";
            e->value = t.lexeme;
            return e;
        }
        if (t.type == "T_STRINGLIT")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Literal";
            e->litType = "T_STRINGLIT";
            e->value = t.lexeme;
            return e;
        }
        if (t.type == "T_CHARLIT")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Literal";
            e->litType = "T_CHARLIT";
            e->value = t.lexeme;
            return e;
        }
        if (t.type == "T_BOOLLIT")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Literal";
            e->litType = "T_BOOLLIT";
            e->value = t.lexeme;
            return e;
        }
        if (t.type == "T_IDENTIFIER")
        {
            i++;
            auto e = make_shared<Expr>();
            e->kind = "Identifier";
            e->value = t.lexeme;
            return e;
        }
        if (match("T_PARENL"))
        {
            auto e = make_shared<Expr>();
            e->kind = "Grouping";
            e->args = {parseExpr()};
            expect("T_PARENR", "Expected ')'");
            return e;
        }
        throw ParseError(string("Expected expression at ") + toLoc(t));
    }
};

static void printTokens(const vector<Token> &toks)
{
    cout << "Tokens:\n[";
    bool first = true;
    for (const auto &t : toks)
    {
        if (t.type == "T_EOF")
            break;
        if (!first)
            cout << ", ";
        cout << showToken(t);
        first = false;
    }
    cout << "]\n";
}

// Scope Analysis

struct Expr;
struct Stmt;
struct Decl;
using ExprPtr = shared_ptr<Expr>;
using StmtPtr = shared_ptr<Stmt>;
using DeclPtr = shared_ptr<Decl>;

struct Scope
{
    unordered_map<string, string> vars;
    shared_ptr<Scope> parent;

    Scope(shared_ptr<Scope> parentScope = nullptr)
        : parent(parentScope) {}
};

struct ScopeStack
{
    shared_ptr<Scope> root;
    shared_ptr<Scope> current;
    unordered_map<string, string> functions;

    ScopeStack()
    {
        root = make_shared<Scope>(nullptr);
        current = root;
    }

    void enterScope()
    {
        auto newScope = make_shared<Scope>(current);
        current = newScope;
    }

    void exitScope()
    {
        if (current->parent)
        {
            current = current->parent;
        }
    }

    bool addVar(const string &name, const string &type)
    {
        if (current->vars.count(name))
        {
            return false;
        }
        current->vars[name] = type;
        return true;
    }

    bool varExists(const string &name) const
    {
        auto p = current;
        while (p)
        {
            if (p->vars.count(name))
                return true;
            p = p->parent;
        }
        return false;
    }

    void addFunc(const string &name, const string &retType)
    {
        if (functions.count(name))
        {
            throw runtime_error("ScopeError: FunctionPrototypeRedefinition -> " + name);
        }
        functions[name] = retType;
    }

    bool funcExists(const string &name) const
    {
        return functions.count(name);
    }

    string lookupVarType(const string &name) const
    {
        auto p = current;
        while (p)
        {
            auto it = p->vars.find(name);
            if (it != p->vars.end())
                return it->second;
            p = p->parent;
        }
        throw runtime_error("ScopeError: UndeclaredVariableAccessed -> " + name);
    }
};

struct ScopeAnalyzer
{
    ScopeStack scope;

    void analyzeProgram(const vector<DeclPtr> &decls)
    {
        for (auto &d : decls)
        {
            analyzeDecl(d);
        }
    }

    void analyzeDecl(const DeclPtr &d)
    {
        if (!d)
            return;

        if (d->kind == "VarDecl")
        {
            if (!scope.addVar(d->varName, d->varType))
            {
                throw runtime_error("ScopeError: VariableRedefinition -> " + d->varName);
            }
            if (d->varInit)
            {
                analyzeExpr(d->varInit);
            }
        }
        else if (d->kind == "FnDecl")
        {
            if (scope.funcExists(d->fnName))
            {
                throw runtime_error("ScopeError: FunctionPrototypeRedefinition -> " + d->fnName);
            }
            scope.addFunc(d->fnName, d->retType);
            scope.enterScope();
            for (auto &pr : d->params)
            {
                const auto &ty = pr.first;
                const auto &nm = pr.second;
                if (!scope.addVar(nm, ty))
                {
                    throw runtime_error("ScopeError: VariableRedefinition(param) -> " + nm);
                }
            }

            for (auto &s : d->body)
            {
                analyzeStmt(s);
            }

            scope.exitScope();
        }
    }

    void analyzeStmt(const StmtPtr &s)
    {
        if (!s)
            return;

        if (s->kind == "VarDecl")
        {
            if (!scope.addVar(s->name, s->typeTok))
            {
                throw runtime_error("ScopeError: VariableRedefinition -> " + s->name);
            }
            if (s->init)
            {
                analyzeExpr(s->init);
            }
        }
        else if (s->kind == "ExprStmt")
        {
            analyzeExpr(s->expr);
        }
        else if (s->kind == "Return")
        {
            analyzeExpr(s->ret);
        }
        else if (s->kind == "If")
        {
            analyzeExpr(s->ifCond);
            scope.enterScope();
            for (auto &st : s->ifBlock)
            {
                analyzeStmt(st);
            }
            scope.exitScope();

            if (!s->elseBlock.empty())
            {
                scope.enterScope();
                for (auto &st : s->elseBlock)
                {
                    analyzeStmt(st);
                }
                scope.exitScope();
            }
        }
        else if (s->kind == "While")
        {
            analyzeExpr(s->whileCond);

            scope.enterScope();
            for (auto &st : s->whileBlock)
            {
                analyzeStmt(st);
            }
            scope.exitScope();
        }
        else if (s->kind == "For")
        {
            scope.enterScope();

            if (s->hasInitDecl)
            {
                if (!scope.addVar(s->forInitName, s->forInitType))
                {
                    throw runtime_error("ScopeError: VariableRedefinition(for) -> " + s->forInitName);
                }
                if (s->forInitExpr)
                {
                    analyzeExpr(s->forInitExpr);
                }
            }
            else
            {
                analyzeExpr(s->expr);
            }

            analyzeExpr(s->forCond);
            analyzeExpr(s->forUpdt);
            for (auto &st : s->forBlock)
            {
                analyzeStmt(st);
            }

            scope.exitScope();
        }
        else if (s->kind == "Block")
        {
            scope.enterScope();
            for (auto &st : s->block)
            {
                analyzeStmt(st);
            }
            scope.exitScope();
        }
    }

    void analyzeExpr(const ExprPtr &e)
    {
        if (!e)
            return;

        if (e->kind == "Literal")
        {
            return;
        }
        else if (e->kind == "Identifier")
        {
            if (!scope.varExists(e->value))
            {
                throw runtime_error("ScopeError: UndeclaredVariableAccessed -> " + e->value);
            }
        }
        else if (e->kind == "Call")
        {
            if (!scope.funcExists(e->value))
            {
                throw runtime_error("ScopeError: UndefinedFunctionCalled -> " + e->value);
            }
            for (auto &a : e->args)
            {
                analyzeExpr(a);
            }
        }
        else if (e->kind == "Unary")
        {
            for (auto &a : e->args)
            {
                analyzeExpr(a);
            }
        }
        else if (e->kind == "Binary")
        {
            for (auto &a : e->args)
            {
                analyzeExpr(a);
            }
        }
        else if (e->kind == "Index")
        {
            for (auto &a : e->args)
            {
                analyzeExpr(a);
            }
        }
        else if (e->kind == "Grouping")
        {
            for (auto &a : e->args)
            {
                analyzeExpr(a);
            }
        }
    }
};

int runScopeAnalysis(const vector<DeclPtr> &decls)
{
    try
    {
        ScopeAnalyzer analyzer;
        analyzer.analyzeProgram(decls);
        cout << "\nScope analysis: OK (no errors)\n";
        return 0;
    }
    catch (const runtime_error &e)
    {
        cerr << "\n"
             << e.what() << "\n";
        return 1;
    }
}

// ------------------------------ Type Checker ------------------------------
static inline bool isNumeric(const string &t) { return t == "int" || t == "float" || t == "char"; }
static inline bool isInteger(const string &t) { return t == "int" || t == "char"; }
static inline bool isBoolean(const string &t) { return t == "bool"; }
static inline bool isString(const string &t) { return t == "string"; }

static string numericResult(const string &a, const string &b, const string &op)
{
    if (!isNumeric(a) || !isNumeric(b))
    {
        if (op == "T_PLUS" || op == "T_MINUS")
            throw runtime_error("TypeChkError: AttemptedAddOpOnNonNumeric");
        if (op == "T_STAR" || op == "T_SLASH")
            throw runtime_error("TypeChkError: ExpressionTypeMismatch");
    }
    if (a == "float" || b == "float")
        return "float";
    if (a == "int" || b == "int")
        return "int";
    return "char";
}
struct FunctionSig
{
    string ret;
    vector<string> params;
};

struct TypeScope
{
    vector<unordered_map<string, string>> varScopes;
    unordered_map<string, FunctionSig> fns;

    void enter() { varScopes.push_back({}); }
    void exit()
    {
        if (!varScopes.empty())
            varScopes.pop_back();
    }

    bool addVar(const string &name, const string &ty)
    {
        if (varScopes.empty())
            enter();
        auto &top = varScopes.back();
        if (top.count(name))
            return false;
        top[name] = ty;
        return true;
    }
    string getVar(const string &name) const
    {
        for (auto it = varScopes.rbegin(); it != varScopes.rend(); ++it)
        {
            auto jt = it->find(name);
            if (jt != it->end())
                return jt->second;
        }
        return "";
    }
    bool addFn(const string &name, const FunctionSig &sig)
    {
        return fns.emplace(name, sig).second;
    }
    const FunctionSig *getFn(const string &name) const
    {
        auto it = fns.find(name);
        return (it == fns.end()) ? nullptr : &it->second;
    }
};

struct TypeChecker
{
    TypeScope scope;
    string currentFnRet = "void";
    bool foundReturnInThisFn = false;
    int loopDepth = 0;
    void checkProgram(const vector<DeclPtr> &decls)
    {
        scope.enter();
        for (auto &d : decls)
        {
            if (d->kind == "FnDecl")
            {
                FunctionSig sig;
                sig.ret = d->retType;
                for (auto &p : d->params)
                    sig.params.push_back(p.first);
                if (!scope.addFn(d->fnName, sig))
                {
                    throw runtime_error("TypeChkError: ErroneousVarDecl (function redefinition)");
                }
            }
        }
        for (auto &d : decls)
            checkDecl(d);
        scope.exit();
    }

    void checkDecl(const DeclPtr &d)
    {
        if (d->kind == "VarDecl")
        {
            if (d->varType == "void")
                throw runtime_error("TypeChkError: ErroneousVarDecl");
            if (!scope.addVar(d->varName, d->varType))
                throw runtime_error("TypeChkError: ErroneousVarDecl");
            if (d->varInit)
            {
                string rhsT = typeOf(d->varInit);
                if (!assignCompatible(d->varType, rhsT))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
            }
        }
        else if (d->kind == "FnDecl")
        {
            scope.enter();
            currentFnRet = d->retType;
            foundReturnInThisFn = false;

            for (auto &[pty, pnm] : d->params)
            {
                if (pty == "void")
                    throw runtime_error("TypeChkError: ErroneousVarDecl");
                if (!scope.addVar(pnm, pty))
                    throw runtime_error("TypeChkError: ErroneousVarDecl");
            }
            for (auto &s : d->body)
                checkStmt(s);

            scope.exit();
            if (currentFnRet != "void" && !foundReturnInThisFn)
                throw runtime_error("TypeChkError: ReturnStmtNotFound");
            currentFnRet = "void";
        }
    }
    void checkStmt(const StmtPtr &s)
    {
        if (!s)
        { /* empty stmt node shouldn't happen */
            return;
        }

        if (s->kind == "VarDecl")
        {
            if (s->typeTok == "void")
                throw runtime_error("TypeChkError: ErroneousVarDecl");
            if (!scope.addVar(s->name, s->typeTok))
                throw runtime_error("TypeChkError: ErroneousVarDecl");
            if (s->init)
            {
                string rt = typeOf(s->init);
                if (!assignCompatible(s->typeTok, rt))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
            }
        }
        else if (s->kind == "ExprStmt")
        {
            if (!s->expr)
                throw runtime_error("TypeChkError: EmptyExpression");
            (void)typeOf(s->expr);
        }
        else if (s->kind == "Return")
        {
            string rt = "void";
            if (s->ret)
                rt = typeOf(s->ret);
            if (!assignCompatible(currentFnRet, rt))
                throw runtime_error("TypeChkError: ErroneousReturnType");
            if (currentFnRet == "void" && rt != "void")
                throw runtime_error("TypeChkError: ErroneousReturnType");
            foundReturnInThisFn = true;
        }
        else if (s->kind == "If")
        {
            string ct = typeOf(s->ifCond);
            if (!isBoolean(ct))
                throw runtime_error("TypeChkError: NonBooleanCondStmt");
            scope.enter();
            for (auto &st : s->ifBlock)
                checkStmt(st);
            scope.exit();
            scope.enter();
            for (auto &st : s->elseBlock)
                checkStmt(st);
            scope.exit();
        }
        else if (s->kind == "While")
        {
            string ct = typeOf(s->whileCond);
            if (!isBoolean(ct))
                throw runtime_error("TypeChkError: NonBooleanCondStmt");
            loopDepth++;
            scope.enter();
            for (auto &st : s->whileBlock)
                checkStmt(st);
            scope.exit();
            loopDepth--;
        }
        else if (s->kind == "For")
        {
            scope.enter();
            loopDepth++;
            if (s->hasInitDecl)
            {
                if (s->forInitType == "void")
                    throw runtime_error("TypeChkError: ErroneousVarDecl");
                if (!scope.addVar(s->forInitName, s->forInitType))
                    throw runtime_error("TypeChkError: ErroneousVarDecl");
                if (s->forInitExpr)
                {
                    string rt = typeOf(s->forInitExpr);
                    if (!assignCompatible(s->forInitType, rt))
                        throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                }
            }
            else
            {
                if (s->expr)
                    (void)typeOf(s->expr);
            }
            if (s->forCond)
            {
                string ct = typeOf(s->forCond);
                if (!isBoolean(ct))
                    throw runtime_error("TypeChkError: NonBooleanCondStmt");
            }
            if (s->forUpdt)
                (void)typeOf(s->forUpdt);
            for (auto &st : s->forBlock)
                checkStmt(st);
            loopDepth--;
            scope.exit();
        }
        else if (s->kind == "Block")
        {
            scope.enter();
            for (auto &st : s->block)
                checkStmt(st);
            scope.exit();
        }
        else if (s->kind == "Break")
        {
            if (loopDepth <= 0)
                throw runtime_error("TypeChkError: ErroneousBreak");
        }
        else
        {
            // unknown stmt kind
        }
    }

    string typeOf(const ExprPtr &e)
    {
        if (!e)
            return "void";
        if (e->kind == "Literal")
        {
            if (e->litType == "T_INTLIT")
                return "int";
            if (e->litType == "T_FLOATLIT")
                return "float";
            if (e->litType == "T_BOOLLIT")
                return "bool";
            if (e->litType == "T_CHARLIT")
                return "char";
            if (e->litType == "T_STRINGLIT")
                return "string";
            throw runtime_error("TypeChkError: ExpressionTypeMismatch");
        }
        if (e->kind == "Identifier")
        {
            string t = scope.getVar(e->value);
            if (t.empty())
                throw runtime_error("TypeChkError: ExpressionTypeMismatch");
            return t;
        }
        if (e->kind == "Grouping")
        {
            return typeOf(e->args[0]);
        }
        if (e->kind == "Index")
        {
            string bt = typeOf(e->args[0]);
            string it = typeOf(e->args[1]);
            if (!isInteger(it))
                throw runtime_error("TypeChkError: AttemptedIndexWithNonInt");
            return "int";
        }

        if (e->kind == "Call")
        {
            const FunctionSig *sig = scope.getFn(e->value);
            if (!sig)
                throw runtime_error("TypeChkError: ExpressionTypeMismatch");
            if ((int)e->args.size() != (int)sig->params.size())
                throw runtime_error("TypeChkError: FnCallParamCount");
            for (size_t i = 0; i < sig->params.size(); ++i)
            {
                string at = typeOf(e->args[i]);
                if (!assignCompatible(sig->params[i], at))
                    throw runtime_error("TypeChkError: FnCallParamType");
            }
            return sig->ret;
        }
        if (e->kind == "Unary")
        {
            string a = typeOf(e->args[0]);
            if (e->op == "T_PLUS" || e->op == "T_MINUS")
            {
                if (!isNumeric(a))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                return a;
            }
            else if (e->op == "T_BANG")
            {
                if (!isBoolean(a))
                    throw runtime_error("TypeChkError: AttemptedBoolOpOnNonBools");
                return "bool";
            }
            else if (e->op == "T_TILDE")
            {
                if (!isInteger(a))
                    throw runtime_error("TypeChkError: AttemptedBitOpOnNonNumeric");
                return a;
            }
            else
            {
                throw runtime_error("TypeChkError: ExpressionTypeMismatch");
            }
        }
        if (e->kind == "Binary")
        {
            const string &op = e->op;
            if (op == "T_ASSIGNOP")
            {
                string lt;

                if (e->args[0]->kind == "Identifier")
                {
                    // normal: x = rhs
                    lt = typeOf(e->args[0]);
                }
                else if (e->args[0]->kind == "Index")
                {
                    string it = typeOf(e->args[0]->args[1]);
                    if (!isInteger(it))
                        throw runtime_error("TypeChkError: AttemptedIndexWithNonInt");
                    lt = "int";
                }
                else
                {
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                }

                string rt = typeOf(e->args[1]);
                if (!assignCompatible(lt, rt))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                return lt;
            }

            string a = typeOf(e->args[0]);
            string b = typeOf(e->args[1]);

            // logical
            if (op == "T_ANDAND" || op == "T_OROR")
            {
                if (!isBoolean(a) || !isBoolean(b))
                    throw runtime_error("TypeChkError: AttemptedBoolOpOnNonBools");
                return "bool";
            }

            // equality
            if (op == "T_EQUALSOP" || op == "T_NEQ")
            {
                bool ok = (a == b) || (isNumeric(a) && isNumeric(b));
                if (!ok)
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                return "bool";
            }

            // relational
            if (op == "T_LT" || op == "T_GT" || op == "T_LTE" || op == "T_GTE")
            {
                if (!isNumeric(a) || !isNumeric(b))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                return "bool";
            }

            // arithmetic
            if (op == "T_PLUS" || op == "T_MINUS" || op == "T_STAR" || op == "T_SLASH")
            {
                return numericResult(a, b, op);
            }
            if (op == "T_PERCENT")
            {
                if (!isInteger(a) || !isInteger(b))
                    throw runtime_error("TypeChkError: ExpressionTypeMismatch");
                return a;
            }

            // bitwise
            if (op == "T_AMP" || op == "T_PIPE" || op == "T_CARET")
            {
                if (!isInteger(a) || !isInteger(b))
                    throw runtime_error("TypeChkError: AttemptedBitOpOnNonNumeric");
                return (a == "int" || b == "int") ? "int" : "char";
            }
            if (op == "T_SHL" || op == "T_SHR")
            {
                if (!isInteger(a) || !isInteger(b))
                    throw runtime_error("TypeChkError: AttemptedShiftOnNonInt");
                return a;
            }

            // exponentiation
            if (op == "T_POW")
            {
                if (!isNumeric(a) || !isNumeric(b))
                    throw runtime_error("TypeChkError: AttemptedExponentiationOfNonNumeric");
                return (a == "float" || b == "float") ? "float" : ((a == "int" || b == "int") ? "int" : "char");
            }

            throw runtime_error("TypeChkError: ExpressionTypeMismatch");
        }

        throw runtime_error("TypeChkError: ExpressionTypeMismatch");
    }

    static bool assignCompatible(const string &to, const string &from)
    {
        if (to == from)
            return true;
        if (to == "void")
            return (from == "void");
        if (isNumeric(to) && isNumeric(from))
        {
            if (to == "float")
                return true;
            if (to == "int")
                return (from == "int" || from == "char");
            if (to == "char")
                return (from == "char");
        }
        if (to == "bool" && from == "bool")
            return true;
        if (to == "string" && from == "string")
            return true;
        return false;
    }
};

int runTypeCheck(const vector<DeclPtr> &decls)
{
    try
    {
        TypeChecker tc;

        // check 1
        // for (auto& d : decls) {
        //     if (d->kind == "FnDecl") {
        //         FunctionSig sig;
        //         sig.ret = d->retType;
        //         for (auto& p : d->params) sig.params.push_back(p.first);
        //         if (!tc.scope.addFn(d->fnName, sig))
        //             throw runtime_error("TypeChkError: ErroneousVarDecl (function redefinition)");
        //     }
        // }
        // Now actually check everything
        tc.checkProgram(decls);

        cout << "Type check: OK (no errors)\n";
        return 0;
    }
    catch (const runtime_error &e)
    {
        cerr << e.what() << "\n";
        return 1;
    }
}

static const string SAMPLE_CPP1 = R"(
int h=a+100-20/b*3;
)";
// static const string SAMPLE_CPP = R"(
// #include <iostream>
// int main() {
//     int f=5;
//     int b=10;
//     int z=2;
//     int a = 3;
//     int h = 6 * f + 3 * 2 - b * b * z * z;
//     int c = 10 - (-b * 10 + 12) + (5 - 10);

//     // control flow
//     if (a == 3) { a = a + 1; } else { a = a - 1; }
//     int p = 2 ** 3 ** 2;   // parses as 2 ** (3 ** 2) = 2 ** 9
//     float q = 2.0 ** 3;

//     for (int i = 0; i < 5; i = i + 1) { b = b + i; }
//     int y = (a & b) | (z ^ 3);
//     int s = (b << 2) >> 1;
//     int nb = ~a;
//     return add(a, b);
// }
// // trailing comment
// )";

// static const string SAMPLE_CPP = R"(
// #include <iostream>
// int main() {
//     int f=5;
//     int b=10;
//     int z=2;
//     int a = 3;
//     int h = 6 * f + 3 * 2 - b * b * z * z;
//     int c = 10 - (-b * 10 + 12) + (5 - 10);

//     // control flow
//     if (a == 3) { a = a + 1; } else { a = a - 1; }
//     for (int i = 0; i < 5; i = i + 1) { b = b + i; }

//     // bitwise & shifts already in your grammar
//     int y = (a & b) | (z ^ 3);
//     int s = (b << 2) >> 1;
//     int nb = ~a;

//     // ----- NEW: exponentiation tests -----
//     int   p = 2 ** 3 ** 2;   // parses as 2 ** (3 ** 2)
//     float q = 2.0 ** 3;      // float result
//     int r = true ** 3;    // would trigger AttemptedExponentiationOfNonNumeric

//     return add(a, b);
//     //return 0;
// }
// // trailing comment
// )";

// static const string SAMPLE_CPP = R"(
// #include <iostream>
// int main() {
//     int f=5;
//     int b=10;
//     int z=2;
//     int a = 3;
//     int h = 6 * f + 3 * 2 - b * b * z * z;
//     int c = 10 - (-b * 10 + 12) + (5 - 10);

//     // control flow
//     if (a == 3) { a = a + 1; } else { a = a - 1; }
//     for (int i = 0; i < 5; i = i + 1) { b = b + i; }

//     // bitwise & shifts already in your grammar
//     int y = (a & b) | (z ^ 3);
//     int s = (b << 2) >> 1;
//     int nb = ~a;

//     // ----- NEW: exponentiation tests -----
//     int   p = 2 ** 3 ** 2;   // parses as 2 ** (3 ** 2)
//     float q = 2.0 ** 3;      // float result
//     int r = true ** 3;    // would trigger AttemptedExponentiationOfNonNumeric

//     return add(a, b);
//     //return 0;
// }
// // trailing comment
// )";

static string readFile(const string &path)
{
    ifstream ifs(path, ios::binary);
    if (!ifs)
    {
        throw runtime_error("Could not open file: " + path);
    }
    return string(
        istreambuf_iterator<char>(ifs),
        istreambuf_iterator<char>());
}

int main()
{
    cout << "Run which lexer?\n1) manual (ASCII, C++ subset)\n2) regex (C++ subset)\n> ";
    int choice = 0;
    if (!(cin >> choice))
    {
        cerr << "Invalid input\n";
        return 1;
    }

    string input;
    try
    {
        input = readFile("input.cpp");
    }
    catch (const exception &e)
    {
        cerr << e.what() << "\n";
        return 1;
    }

    try
    {
        vector<Token> toks;

        if (choice == 1)
        {
            Lexer1 lx{input};
            toks = lx.tokenize();
        }
        else if (choice == 2)
        {
            Lexer2 lx{input};
            toks = lx.tokenize();
        }
        else
        {
            cerr << "Please enter 1 or 2\n";
            return 1;
        }

        printTokens(toks);

        Parser p{toks};
        auto decls = p.parseProgram();

        cout << "AST:\n[\n";
        for (auto &d : decls)
            printDecl(d, 2);
        cout << "]\n";

        // your analysis passes
        runScopeAnalysis(decls);
        runTypeCheck(decls);
    }
    catch (const LexError &e)
    {
        cerr << "LexerError: " << e.what() << "\n";
        return 1;
    }
    catch (const ParseError &e)
    {
        cerr << "ParseError: " << e.what() << "\n";
        return 1;
    }

    return 0;
}

// previously chain follow these
// assign -> or(||) -> and(&&) -> eq -> rel -> add -> mul -> unary -> postfix -> primary

// now
// chain follows these steps
// assign
//   -> lor (||)
//     -> land (&&)
//       -> bor (|)
//         -> bxor (^)
//           -> band (&)
//             -> eq (==, !=)
//               -> rel (<, >, <=, >=)
//                 -> shift (<<, >>)
//                   -> add (+, -)
//                     -> mul (*, /, %)
//                       -> pow (**)
//                       -> unary (+, -, !, ~)
//                         -> postfix -> primary
