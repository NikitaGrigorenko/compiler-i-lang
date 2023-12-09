#include <string>

// Function to remove all comments from the input file
std::string removeComments(const std::string &code) {
    std::string result;
    bool inSingleLineComment = false;
    bool inMultiLineComment = false;

    for (size_t i = 0; i < code.length(); ++i) {
        if (inSingleLineComment) {
            if (code[i] == '\n') {
                inSingleLineComment = false;
                result += code[i];
            }
        } else if (inMultiLineComment) {
            if (code[i] == '*' && code[i + 1] == '/') {
                inMultiLineComment = false;
                ++i;
            }
        } else {
            if (code[i] == '/' && code[i + 1] == '/') {
                inSingleLineComment = true;
                ++i;
            } else if (code[i] == '/' && code[i + 1] == '*') {
                inMultiLineComment = true;
                ++i;
            } else {
                result += code[i];
            }
        }
    }

    return result;
}
