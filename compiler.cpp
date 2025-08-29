#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <stdexcept>
#include <sstream>
#include <cstdint>
using namespace std;

struct Token
{
    string type;
    string value;
    int line, col;
};

static string show(const Token &t)
{
    if (t.value.empty())
        return t.type;
    if (t.type == "T_IDENTIFIER" || t.type == "T_STRINGLIT" || t.type == "T_COMMENT")
    {
        string esc;
        esc.reserve(t.value.size());
        for (char c : t.value)
            esc += (c == '"') ? "\\\"" : string(1, c);
        return t.type + "(\"" + esc + "\")";
    }
    if (t.type == "T_INTLIT" || t.type == "T_FLOATLIT" || t.type == "T_BOOLLIT")
        return t.type + "(" + t.value + ")";
    return t.type + "(" + t.value + ")";
}

struct LexError : runtime_error
{
    using runtime_error::runtime_error;
};


static bool decodeUTF8(const string &s, size_t pos, uint32_t &cp, size_t &len)
{
    if (pos >= s.size())
    {
        cp = 0;
        len = 0;
        return false;
    }
    unsigned char b0 = (unsigned char)s[pos];
    if (b0 < 0x80)
    {
        cp = b0;
        len = 1;
        return true;
    }
    if ((b0 >> 5) == 0x6 && pos + 1 < s.size())
    {
        unsigned char b1 = (unsigned char)s[pos + 1];
        if ((b1 & 0xC0) != 0x80)
            return false;
        cp = ((b0 & 0x1F) << 6) | (b1 & 0x3F);
        len = 2;
        return true;
    }
    if ((b0 >> 4) == 0xE && pos + 2 < s.size())
    {
        unsigned char b1 = (unsigned char)s[pos + 1], b2 = (unsigned char)s[pos + 2];
        if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80)
            return false;
        cp = ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F);
        len = 3;
        return true;
    }
    if ((b0 >> 3) == 0x1E && pos + 3 < s.size())
    {
        unsigned char b1 = (unsigned char)s[pos + 1], b2 = (unsigned char)s[pos + 2], b3 = (unsigned char)s[pos + 3];
        if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80)
            return false;
        cp = ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F);
        len = 4;
        return true;
    }
    return false;
}

static void appendUTF8(uint32_t cp, string &out)
{
    if (cp <= 0x7F)
        out.push_back((char)cp);
    else if (cp <= 0x7FF)
    {
        out.push_back((char)(0xC0 | ((cp >> 6) & 0x1F)));
        out.push_back((char)(0x80 | (cp & 0x3F)));
    }
    else if (cp <= 0xFFFF)
    {
        out.push_back((char)(0xE0 | ((cp >> 12) & 0x0F)));
        out.push_back((char)(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back((char)(0x80 | (cp & 0x3F)));
    }
    else if (cp <= 0x10FFFF)
    {
        out.push_back((char)(0xF0 | ((cp >> 18) & 0x07)));
        out.push_back((char)(0x80 | ((cp >> 12) & 0x3F)));
        out.push_back((char)(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back((char)(0x80 | (cp & 0x3F)));
    }
    else
    {
        out += "\xEF\xBF\xBD";
    }
}

static bool isIdentStart(uint32_t cp)
{
    if (cp == '_' || cp == '$')
        return true;
    if (cp < 128)
        return std::isalpha((unsigned char)cp);
    return true; 
}
static bool isIdentContinue(uint32_t cp)
{
    if (isIdentStart(cp))
        return true;
    if (cp < 128)
        return std::isdigit((unsigned char)cp);
    return true; 
}

static int hexVal(char c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F')
        return 10 + (c - 'A');
    return -1;
}





struct Lexer
{
    string s;
    size_t i = 0;
    int line = 1, col = 1;

    unordered_map<string, string> kw = {
        {"fn", "T_FUNCTION"}, {"return", "T_RETURN"}, {"if", "T_IF"}, {"else", "T_ELSE"}, {"for", "T_FOR"}, {"while", "T_WHILE"}, {"break", "T_BREAK"}, {"continue", "T_CONTINUE"}, {"int", "T_INT"}, {"float", "T_FLOAT"}, {"bool", "T_BOOL"}, {"string", "T_STRING"}, {"char", "T_CHAR"}, {"void", "T_VOID"}, {"true", "T_BOOLLIT"}, {"false", "T_BOOLLIT"}, {"const", "T_CONST"}, {"let", "T_LET"}, {"var", "T_VAR"}, {"switch", "T_SWITCH"}, {"case", "T_CASE"}, {"default", "T_DEFAULT"}, {"do", "T_DO"}};

    vector<pair<string, string>> multi = {
        {"==", "T_EQUALSOP"}, {"!=", "T_NEQ"}, {"<=", "T_LTE"}, {">=", "T_GTE"}, {"&&", "T_ANDAND"}, {"||", "T_OROR"}, {"<<", "T_SHL"}, {">>", "T_SHR"}, {"++", "T_INC"}, {"--", "T_DEC"}, {"->", "T_ARROW"}, {"+=", "T_PLUSEQ"}, {"-=", "T_MINUSEQ"}, {"*=", "T_MULEQ"}, {"/=", "T_DIVEQ"}, {"%=", "T_MODEQ"}, {"&=", "T_ANDEQ"}, {"|=", "T_OREQ"}, {"^=", "T_XOREQ"}};

    unordered_map<char, string> single = {
        {'(', "T_PARENL"}, {')', "T_PARENR"}, {'{', "T_BRACEL"}, {'}', "T_BRACER"}, {'[', "T_BRACKETL"}, {']', "T_BRACKETR"}, {';', "T_SEMICOLON"}, {',', "T_COMMA"}, {':', "T_COLON"}, {'.', "T_DOT"}, {'+', "T_PLUS"}, {'-', "T_MINUS"}, {'*', "T_STAR"}, {'/', "T_SLASH"}, {'%', "T_PERCENT"}, {'<', "T_LT"}, {'>', "T_GT"}, {'=', "T_ASSIGNOP"}, {'!', "T_BANG"}, {'&', "T_AMP"}, {'|', "T_PIPE"}, {'^', "T_CARET"}, {'~', "T_TILDE"}, {'?', "T_QUESTION"}, {'"', "T_QUOTES"}};

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
            ++col;
    }
    void advN(size_t n)
    {
        while (n--)
            adv();
    }
bool starts(const string &t) const
    {
        if (i + t.size() > s.size())
            return false;
        size_t k = 0;
        while (k < t.size()) 
        {
            if (s[i + k] != t[k])
            {
                return false;
            }
            ++k;
        }
        return true;
    }
    [[noreturn]] void err(const string &msg) const
    {
        ostringstream os;
        os << "Line " << line << ", Col " << col << ": " << msg;
        throw LexError(os.str());
    }

    Token make(const string &t) { return Token{t, "", line, col}; }

    void skipWS(vector<Token> &out)
    {
        while (true)
        {
            char c = peek();
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' || c == '\f')
            {
                adv();
                continue;
            }
            if (c == '/' && peek(1) == '/')
            {
                int sl = line, sc = col;
                string buf;
                while (peek() != '\n' && peek() != '\0')
                {
                    buf.push_back(peek());
                    adv();
                }
                out.push_back(Token{"T_COMMENT", buf, sl, sc});
                continue;
            }
            if (c == '/' && peek(1) == '*')
            {
                int sl = line, sc = col;
                adv();
                adv();
                string buf = "/*";
                while (true)
                {
                    if (peek() == '\0')
                        err("Unterminated block comment");
                    buf.push_back(peek());
                    if (peek() == '*' && peek(1) == '/')
                    {
                        buf.push_back('/');
                        adv();
                        adv();
                        break;
                    }
                    adv();
                }
                out.push_back(Token{"T_COMMENT", buf, sl, sc});
                continue;
            }
            break;
        }
    }


    Token number()
    {
        int sl = line, sc = col;
        size_t start = i;
        if (peek() == '0' && (peek(1) == 'x' || peek(1) == 'X'))
        {
            adv();
            adv();
            if (!isxdigit((unsigned char)peek()))
                err("Invalid hex literal");
            while (isxdigit((unsigned char)peek()))
                adv();
            string lex = s.substr(start, i - start);
            // reject 0xFF变量 style
            uint32_t cp;
            size_t len;
            if (decodeUTF8(s, i, cp, len) && isIdentContinue(cp))
                err("Invalid identifier starting with a number");
            return Token{"T_INTLIT", lex, sl, sc};
        }
        bool sawDot = false, sawExp = false;
        while (isdigit((unsigned char)peek()))
            adv();
        if (peek() == '.' && isdigit((unsigned char)peek(1)))
        {
            sawDot = true;
            adv();
            while (isdigit((unsigned char)peek()))
                adv();
        }
        if (peek() == 'e' || peek() == 'E')
        {
            if (isdigit((unsigned char)peek(1)) || ((peek(1) == '+' || peek(1) == '-') && isdigit((unsigned char)peek(2))))
            {
                sawExp = true;
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
        // reject 123变量 style
        uint32_t cp;
        size_t len;
        if (decodeUTF8(s, i, cp, len) && isIdentContinue(cp))
            err("Invalid identifier starting with a number");
        return Token{(sawDot || sawExp) ? "T_FLOATLIT" : "T_INTLIT", lex, sl, sc};
    }

    
    Token identOrKeyword()
    {
        int sl = line, sc = col;
        size_t start = i;

        uint32_t cp;
        size_t len;
        if (!decodeUTF8(s, i, cp, len) || !isIdentStart(cp))
            err("Identifier expected");
        advN(len); 

        while (true)
        {
            size_t pos = i;
            if (!decodeUTF8(s, pos, cp, len))
                break;
            if (!isIdentContinue(cp))
                break;
            advN(len);
        }

        string lex = s.substr(start, i - start);
        auto it = kw.find(lex);
        if (it != kw.end())
        {
            if (it->second == "T_BOOLLIT")
                return Token{it->second, lex, sl, sc};
            return Token{it->second, "", sl, sc};
        }
        return Token{"T_IDENTIFIER", lex, sl, sc};
    }

    uint32_t readHexDigits(int n)
    {
        uint32_t v = 0;
        int k = 0;
        while (k < n)
        {
            char c = peek();
            int hv = hexVal(c);
            if (hv < 0)
            {
                err("Invalid hex escape");
            }
            v = (v << 4) | (uint32_t)hv;
            adv();
            k++;
        }
        return v;
    }



    void stringLiteral(vector<Token> &out)
    {
        out.push_back(make("T_QUOTES")); 
        adv();                           
        int sl = line, sc = col;
        string buf;
        while (true)
        {
            char c = peek();
            if (c == '\0')
                err("Unterminated string literal");
            if (c == '"')
            { 
                out.push_back(Token{"T_STRINGLIT", buf, sl, sc});
                out.push_back(make("T_QUOTES")); 
                adv();
                break;
            }
            if (c == '\\')
            { 
                adv();
                char e = peek();
                if (e == '\0')
                    err("Unterminated escape in string");
                switch (e)
                {
                case '"':
                    buf.push_back('"');
                    adv();
                    break;
                case '\\':
                    buf.push_back('\\');
                    adv();
                    break;
                case 'n':
                    buf.push_back('\n');
                    adv();
                    break;
                case 't':
                    buf.push_back('\t');
                    adv();
                    break;
                case 'r':
                    buf.push_back('\r');
                    adv();
                    break;
                case 'u':
                {
                    adv();
                    uint32_t cp = readHexDigits(4);
                    appendUTF8(cp, buf);
                    break;
                }
                case 'U':
                { 
                    adv();
                    uint32_t cp = readHexDigits(8);
                    appendUTF8(cp, buf);
                    break;
                }
                default:
                    buf.push_back(e);
                    adv();
                    break; 
                }
            }
            else
            {
                buf.push_back(c);
                adv();
            }
        }
    }

   
};

