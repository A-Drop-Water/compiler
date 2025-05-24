#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <variant>

enum class TokenType {
    // Keywords
    KEYWORD_CONST,
    KEYWORD_INT,
    KEYWORD_VOID,
    KEYWORD_MAIN,
    KEYWORD_IF,
    KEYWORD_ELSE,
    KEYWORD_WHILE,
    KEYWORD_BREAK,
    KEYWORD_CONTINUE,
    KEYWORD_RETURN,
    KEYWORD_GETINT,
    KEYWORD_PRINTF,

    // Identifier
    IDENTIFIER,

    // Constants
    INT_CONST,
    FORMAT_STRING,

    // Operators
    OP_PLUS,          // +
    OP_MINUS,         // -
    OP_MULTIPLY,      // *
    OP_DIVIDE,        // /
    OP_MODULO,        // %
    OP_ASSIGN,        // =
    OP_EQ,            // ==
    OP_NEQ,           // !=
    OP_LT,            // <
    OP_GT,            // >
    OP_LTE,           // <=
    OP_GTE,           // >=
    OP_NOT,           // !
    OP_AND,           // &&
    OP_OR,            // ||

    // Punctuation
    PUNC_SEMICOLON,   // ;
    PUNC_COMMA,       // ,
    PUNC_LPAREN,      // (
    PUNC_RPAREN,      // )
    PUNC_LBRACKET,    // [
    PUNC_RBRACKET,    // ]
    PUNC_LBRACE,      // {
    PUNC_RBRACE,      // }

    // End of File
    END_OF_FILE,

    // Error/Unknown
    UNKNOWN
};

struct Token {
    TokenType type;
    std::variant<std::string, int> value; // Store string for IDENTIFIER, FORMAT_STRING; int for INT_CONST
    int line;

    Token(TokenType t, int l) : type(t), line(l) {}
    Token(TokenType t, std::string val, int l) : type(t), value(val), line(l) {}
    Token(TokenType t, int val, int l) : type(t), value(val), line(l) {}

    std::string toString() const; // For debugging
};

#endif // TOKEN_H
