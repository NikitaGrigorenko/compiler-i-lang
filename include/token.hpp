#ifndef TOKENS_H
#define TOKENS_H

#include <string>
#include <vector>

// Struct to store span of the token
struct Span {
    int lineNum;
    int posBegin, posEnd;
};

// enum with Tokens of all types
enum TokenType {
    TOKEN_ROUTINE,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_COLON,
    TOKEN_INTEGER,
    TOKEN_REAL,
    TOKEN_BOOLEAN,
    TOKEN_RECORD,
    TOKEN_ARRAY,
    TOKEN_IS,
    TOKEN_IN,
    TOKEN_RETURN,
    TOKEN_END,
    TOKEN_VAR,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_MULTIPLY,
    TOKEN_DIVIDE,
    TOKEN_MODULO,
    TOKEN_ASSIGN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_GT,
    TOKEN_LT,
    TOKEN_GE,
    TOKEN_LE,
    TOKEN_EQ,
    TOKEN_NE,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_XOR,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_IDENTIFIER,
    TOKEN_INT_VAL,
    TOKEN_REAL_VAL,
    TOKEN_INVALID,
    TOKEN_SPACE,
    TOKEN_NEW_LINE,
    TOKEN_WHILE,
    TOKEN_FOR,
    TOKEN_LOOP,
    TOKEN_REVERSE,
    TOKEN_DOT,
    TOKEN_BOOLEAN_VAL,
    TOKEN_ELSE,
    TOKEN_NOT,
    TOKEN_TYPE,
    TOKEN_RANGE,
    TOKEN_EOF
};

// Token structure fot storing code, values and Span structure
struct Token {
    TokenType code;
    // Here we use union because these fields might not be accessible for some
    // tokens
    union {
        int intValue;
        double realValue;
        bool booleanValue;
    };
    char *image;
    Span span;
};

// Declaration of Token, tokenList, current position of token and tokenIndex
// (for parse token sequence)
extern Token currentToken;
extern std::vector<Token> tokenSequence;
extern int currentPos;
extern int tokenIndex;

#endif // TOKENS_H