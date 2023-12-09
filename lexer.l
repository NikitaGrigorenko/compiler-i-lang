%{
#include <iostream>
#include <string>
#include "token.hpp"

// Function that initialize the token structure for one token
void currentTokenInit(const TokenType& TOKEN, int currentPosition, char* token_name) {
    currentToken.code = TOKEN;
    currentToken.span.lineNum = yylineno;
    currentToken.span.posBegin = currentPosition;
    currentPos += yyleng;
    currentToken.span.posEnd = currentPos;
    currentToken.image = token_name;
    tokenSequence.push_back(currentToken);
}

%}

%option noyywrap
%option yylineno

%%
"routine"           { currentTokenInit(TOKEN_ROUTINE, currentPos, "routine"); std::cout << "routine"; }
"("                 { currentTokenInit(TOKEN_LPAREN, currentPos, "("); std::cout << "("; }
")"                 { currentTokenInit(TOKEN_RPAREN, currentPos, ")"); std::cout << ")"; }
":"                 { currentTokenInit(TOKEN_COLON, currentPos, ":"); std::cout << ":"; }
"integer"           { currentTokenInit(TOKEN_INTEGER, currentPos, "integer"); std::cout << "integer"; }
"real"              { currentTokenInit(TOKEN_REAL, currentPos, "real"); std::cout << "real"; }
"boolean"           { currentTokenInit(TOKEN_BOOLEAN, currentPos, "boolean"); std::cout << "boolean"; }
"record"            { currentTokenInit(TOKEN_RECORD, currentPos, "record"); std::cout << "record"; }
"array"             { currentTokenInit(TOKEN_ARRAY, currentPos, "array"); std::cout << "array"; }
"is"                { currentTokenInit(TOKEN_IS, currentPos, "is"); std::cout << "is"; }
"in"                { currentTokenInit(TOKEN_IN, currentPos, "in"); std::cout << "in"; }
"return"            { currentTokenInit(TOKEN_RETURN, currentPos, "return"); std::cout << "return"; }
"end"               { currentTokenInit(TOKEN_END, currentPos, "end"); std::cout << "end"; }
"var"               { currentTokenInit(TOKEN_VAR, currentPos, "var"); std::cout << "var";}
","                 { currentTokenInit(TOKEN_COMMA, currentPos, ","); std::cout << ","; }
";"                 { currentTokenInit(TOKEN_SEMICOLON, currentPos, ";"); std::cout << ";"; }
"+"                 { currentTokenInit(TOKEN_PLUS, currentPos, "+"); std::cout << "+"; }
"-"                 { currentTokenInit(TOKEN_MINUS, currentPos, "-"); std::cout << "-"; }
"*"                 { currentTokenInit(TOKEN_MULTIPLY, currentPos, "*"); std::cout << "*"; }
"/"                 { currentTokenInit(TOKEN_DIVIDE, currentPos, "/"); std::cout << "/"; }
"%"                 { currentTokenInit(TOKEN_MODULO, currentPos, "%"); std::cout << "%"; }
":="                { currentTokenInit(TOKEN_ASSIGN, currentPos, ":="); std::cout << ":="; }
"["                 { currentTokenInit(TOKEN_LBRACKET, currentPos, "["); std::cout << "["; }
"]"                 { currentTokenInit(TOKEN_RBRACKET, currentPos, "]"); std::cout << "]"; }
">"                 { currentTokenInit(TOKEN_GT, currentPos, ">"); std::cout << ">"; }
"<"                 { currentTokenInit(TOKEN_LT, currentPos, "<"); std::cout << "<"; }
">="                { currentTokenInit(TOKEN_GE, currentPos, ">="); std::cout << ">="; }
"<="                { currentTokenInit(TOKEN_LE, currentPos, "<="); std::cout << "<="; }
"="                 { currentTokenInit(TOKEN_EQ, currentPos, "="); std::cout << "="; }
"/="                { currentTokenInit(TOKEN_NE, currentPos, "/="); std::cout << "/="; }
"and"               { currentTokenInit(TOKEN_AND, currentPos, "and"); std::cout << "and"; }
"or"                { currentTokenInit(TOKEN_OR, currentPos, "or"); std::cout << "or";  }
"xor"               { currentTokenInit(TOKEN_XOR, currentPos, "xor"); std::cout << "xor"; }
"if"                { currentTokenInit(TOKEN_IF, currentPos, "if"); std::cout << "if"; }
"then"              { currentTokenInit(TOKEN_THEN, currentPos, "then"); std::cout << "then"; }
"while"             { currentTokenInit(TOKEN_WHILE, currentPos, "while"); std::cout << "while"; }
"for"               { currentTokenInit(TOKEN_FOR, currentPos, "for"); std::cout << "for"; }
"loop"              { currentTokenInit(TOKEN_LOOP, currentPos, "loop"); std::cout << "loop"; }
"reverse"           { currentTokenInit(TOKEN_REVERSE, currentPos, "reverse"); std::cout << "reverse"; }
"else"              { currentTokenInit(TOKEN_ELSE, currentPos, "else"); std::cout << "else"; }
".."                { currentTokenInit(TOKEN_RANGE, currentPos, ".."); std::cout << ".."; }
"."                 { currentTokenInit(TOKEN_DOT, currentPos, "."); std::cout << "."; }
"not"               { currentTokenInit(TOKEN_NOT, currentPos, "not"); std::cout << "not"; }
"type"              { currentTokenInit(TOKEN_TYPE, currentPos, "type"); std::cout << "type"; }
" "                 { currentPos++; std::cout << " "; } 

"true" {
    std::cout << "true";
    currentToken.booleanValue = true;
    currentTokenInit(TOKEN_BOOLEAN_VAL, currentPos, "BOOLEAN_VAL");
}

"false" {
    std::cout << "false";
    currentToken.booleanValue = false;
    currentTokenInit(TOKEN_BOOLEAN_VAL, currentPos, "BOOLEAN_VAL");
}

[a-zA-Z_][a-zA-Z0-9_]* {
    std::cout << "identifier";
    currentTokenInit(TOKEN_IDENTIFIER, currentPos, strdup(yytext));
}

[0-9]+              {
    std::cout << strdup(yytext);
    currentToken.intValue = atoi(yytext);
    currentTokenInit(TOKEN_INT_VAL, currentPos, "INT_VAL");
}

[0-9]+"."[0-9]+     {
    std::cout << strdup(yytext);
    currentToken.realValue = atof(yytext);
    currentTokenInit(TOKEN_REAL_VAL, currentPos, "REAL_VAL");
}

\n                  {
    std::cout << "\n";
    currentTokenInit(TOKEN_NEW_LINE, currentPos, "\n");
    currentPos = 1;
}

. {
    std::cout << "invalid";
    currentTokenInit(TOKEN_INVALID, currentPos, "INVALID");
}


%%
