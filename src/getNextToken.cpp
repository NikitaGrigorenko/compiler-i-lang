#include "getNextToken.hpp"
#include "token.hpp"

// Initialize global variables for the current line number and position within the line.
int numberLine = 0;
int numberColon = 0;

// Define a namespace called "yy" to contain the getTokenFromSequence function.
namespace yy {
// getTokenFromSequence function returns the next token from the token sequence.
// It updates the line number (numberLine) and position within the line (numberColon)
// as it processes through the tokens.
// TODO: Terrible code here. Make returning code more elegant)
int getTokenFromSequence() {
    numberLine = tokenSequence[tokenIndex].span.lineNum;
    numberColon = tokenSequence[tokenIndex].span.posBegin;
    if (tokenIndex < tokenSequence.size()) {
        switch (tokenSequence[tokenIndex++].code) {
            case TOKEN_VAR: return 258;         /* VAR  */
            case TOKEN_IS: return 259;          /* IS  */
            case TOKEN_TYPE: return 260;        /* TYPE  */
            case TOKEN_IDENTIFIER: return 261;  /* IDENTIFIER  */
            case TOKEN_INTEGER: return 262;     /* INTEGER  */
            case TOKEN_REAL: return 263;        /* REAL  */
            case TOKEN_BOOLEAN: return 264;     /* BOOLEAN  */
            case TOKEN_RECORD: return 265;      /* RECORD  */
            case TOKEN_END: return 266;         /* END  */
            case TOKEN_ARRAY: return 267;       /* ARRAY  */
            case TOKEN_NEW_LINE: return 268;    /* NEW_LINE  */
            case TOKEN_WHILE: return 269;       /* WHILE  */
            case TOKEN_LOOP: return 270;        /* LOOP  */
            case TOKEN_FOR: return 271;         /* FOR  */
            case TOKEN_IN: return 272;          /* IN  */
            case TOKEN_REVERSE: return 273;     /* REVERSE  */
            case TOKEN_IF: return 274;          /* IF  */
            case TOKEN_THEN: return 275;        /* THEN  */
            case TOKEN_ELSE: return 276;        /* ELSE  */
            case TOKEN_ROUTINE: return 277;     /* ROUTINE  */
            case TOKEN_RETURN: return 278;      /* RETURN  */
            case TOKEN_COLON: return 279;       /* COLON  */
            case TOKEN_LPAREN: return 280;      /* LPAREN  */
            case TOKEN_RPAREN: return 281;      /* RPAREN  */
            case TOKEN_LBRACKET: return 282;    /* LBRACKET  */
            case TOKEN_RBRACKET: return 283;    /* RBRACKET  */
            case TOKEN_DOT: return 284;         /* DOT  */
            case TOKEN_RANGE: return 285;       /* RANGE  */
            case TOKEN_COMMA: return 286;       /* COMMA  */
            case TOKEN_ASSIGN: return 287;      /* ASSIGN  */
            case TOKEN_AND: return 288;         /* AND  */
            case TOKEN_OR: return 289;          /* OR  */
            case TOKEN_XOR: return 290;         /* XOR  */
            case TOKEN_EQ: return 291;          /* EQ  */
            case TOKEN_NE: return 292;          /* NE  */
            case TOKEN_LT: return 293;          /* LT  */
            case TOKEN_GT: return 294;          /* GT  */
            case TOKEN_LE: return 295;          /* LE  */
            case TOKEN_GE: return 296;          /* GE  */
            case TOKEN_MULTIPLY: return 297;    /* MULTIPLY  */
            case TOKEN_DIVIDE: return 298;      /* DIVIDE  */
            case TOKEN_MODULO: return 299;      /* MODULO  */
            case TOKEN_PLUS: return 300;        /* PLUS  */
            case TOKEN_MINUS: return 301;       /* MINUS  */
            case TOKEN_INT_VAL: return 302;     /* INT_VAL  */
            case TOKEN_REAL_VAL: return 303;    /* REAL_VAL  */
            case TOKEN_BOOLEAN_VAL: return 304; /* BOOLEAN_VAL  */
            case TOKEN_SEMICOLON: return 305;   /* BOOLEAN_VAL  */
        }
    }
    return -1; // Return 0 to indicate end of tokens
}
} // namespace yy