#include <fstream>
#include <iostream>
#include "remove_comments.hpp"

// Function that takes input file and produce it to function remove_comments
void commentFile(std::string file_name) {
    std::ifstream file(file_name);
    if (!file) {
        std::cerr << "Error: failed to open the file." << std::endl;
        exit(1);
    }

    std::string code((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();

    std::string codeWithoutComments = removeComments(code);

    std::ofstream outputFile("codeWithoutComments.i");
    if (!outputFile) {
        std::cerr << "Error: failed to create the output file." << std::endl;
        exit(1);
    }

    outputFile << codeWithoutComments;
    outputFile.close();
}