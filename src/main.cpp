#define PLATFORM_getcwd getcwd
#include <cstdlib>
#include <iostream>
#include <map>
#include <zconf.h>
#include "codegen.hpp"
#include "comment_file.hpp"
#include "remove_comments.hpp"
#include "semanticAnalyser.hpp"
#include "token.hpp"

int yylex();
extern FILE *yyin;
extern int yyparse();

// This function is used for printing debug messages.
void printDev(std::string str) {
    std::cout << std::endl << str << std::endl;
}

// This function is used to generate and increment unique counters for different purposes.
long long GetAndIncrementUniqueCounter(Counter c) {
    static long long counter[] = {0, 0, 0};
    return counter[c]++;
}

// This is the main compiler function that takes an input file, processes it and generates the
// output file.
int RunCompiler(int argc, const char *argv[]) {
    const char *fileName = argv[1];
    commentFile(fileName);

    FILE *inputFile = stdin;
    inputFile = fopen("codeWithoutComments.i", "rb");
    Semantics::InitSemanticContext();
    FILE *outputFile = fopen("IRRepresentationCode.ll", "wb");
    Codegen::InitCodegenContext("compilation_result", outputFile);

    printDev("---------------------------- Lexer Result "
             "----------------------------");
    yyin = inputFile;
    int lexer = yylex();

    printDev("---------------------------- Parser Result "
             "----------------------------");
    int parser = yyparse();
    if (parser != 0) exit(1);

    printDev("---------------------------- Semantic Result "
             "----------------------------");
    Semantics::RunSemanticPass(GetProgramTree());

    printDev("---------------------------- Code Generation Result "
             "----------------------------");
    Codegen::RunCodegenPass(GetProgramTree());

    printDev("---------------------------- Clang Result "
             "----------------------------");

    int result = std::system("clang++ -o compiledCode IRRepresentationCode.ll");

    if (result == 0) {
        printDev("The program was compiled succesfully in file: \033[34mcompiledCode\033[0m");
        printDev("You can run it by using \033[34m./compiledCode\033[0m");
    } else {
        printDev("The program was not compiled due to clang or compiler error!");
    }

    return 0;
}

// The main function of the program, which just calls the RunCompiler function with input arguments.
int main(int argc, const char *argv[]) {
    return RunCompiler(argc, argv);
}
