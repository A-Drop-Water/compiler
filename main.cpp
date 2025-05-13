// In your main.cpp or equivalent
#include "lexer.h"
#include "parser.h"
#include "ast.h" // For AstNode
#include <fstream>
#include <iostream>
#include <string>
#include <vector> // For std::vector<Token>
#include <sstream>

std::string readFromFile(const std::string &filename)
{
  std::ifstream file(filename);
  if (!file.is_open())
  {
    throw std::runtime_error("Failed to open file: " + filename);
  }
  std::stringstream buffer;
  buffer << file.rdbuf(); // 将文件内容读入字符串流
  return buffer.str();
}

int main() {
    std::string sourceCode = readFromFile("testfile.txt"); // Your source code
    Lexer lexer(sourceCode);
    std::vector<Token> tokens = lexer.tokenize();

    std::ofstream astOutFile("ast_output.txt");
    if (!astOutFile) {
        std::cerr << "Error opening ast_output.txt for writing." << std::endl;
        return 1;
    }

    Parser parser(tokens, astOutFile);
    try {
        std::unique_ptr<CompUnitNode> astRoot = parser.parse();
        if (astRoot) {
            astRoot->print(astOutFile, 0); // Print the AST to the file
            std::cout << "AST successfully generated and printed to ast_output.txt" << std::endl;
        } else {
            std::cerr << "Parsing failed to produce an AST." << std::endl;
        }
    } catch (const std::runtime_error& e) {
        std::cerr << "Parsing Error: " << e.what() << std::endl;
        // Also print error to astOutFile if desired
        astOutFile << "Parsing Error: " << e.what() << std::endl;
        return 1;
    }

    astOutFile.close();
    return 0;
}