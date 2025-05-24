#ifndef LEXER_H
#define LEXER_H

#include "token.h"
#include <string>
#include <vector>
#include <fstream>

class Lexer {
public:
    Lexer(const std::string& filename);
    Token getNextToken();
    std::vector<Token> getAllTokens(); // Helper to get all tokens at once

private:
    std::ifstream sourceFile;
    std::string currentLineContent;
    int line;
    int column; // Current position in currentLineContent

    char currentChar();
    void advance();
    void skipWhitespace();
    void skipComment();
    Token identifier();
    Token number();
    Token formatString();
    Token operatorOrPunc();
};

#endif // LEXER_H
