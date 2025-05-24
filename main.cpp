#include <iostream>
#include <fstream>
#include <memory> // For std::unique_ptr
#include "include/lexer.h"
#include "include/parser.h"
#include "include/ast.h" // Assuming ast.h includes the print methods
#include "include/symbol_table.h"
#include "include/semantic_analyzer.h"
#include "include/code_generator.h" // New include

int main() {
    std::cout << "Starting compiler..." << std::endl;

    // Lexer lexer("testfile.txt"); // This instance would be for initial token printing if uncommented
    // Optional: You can get all tokens and print them for debugging the lexer
    // std::vector<Token> tokens = lexer.getAllTokens();
    // std::cout << "Tokens:" << std::endl;
    // for (const auto& token : tokens) {
    //     // Assuming Token has a toString() or similar method, or implement one
    //     // std::cout << token.toString() << std::endl; 
    // }
    
    // For parsing, we need a fresh lexer instance because getAllTokens() consumes the input.
    Lexer parserLexer("testfile.txt"); // Use a new lexer instance for the parser
    Parser parser(parserLexer);

    std::unique_ptr<CompUnitNode> astRoot = nullptr;
    try {
        astRoot = parser.parseCompUnit();
        std::cout << "Parsing completed successfully." << std::endl;
    } catch (const std::runtime_error& e) {
        std::cerr << "Parsing failed: " << e.what() << std::endl;
        return 1; // Indicate error
    }

    if (astRoot) {
        // Optional AST Printing (as before)
        // std::cout << "AST generated. Printing AST to ast_output.txt..." << std::endl;
        // std::ofstream astFile("ast_output.txt");
        // if (astFile.is_open()) {
        //     astRoot->print(astFile);
        //     astFile.close();
        // } else {
        //     std::cerr << "Error: Could not open ast_output.txt for writing." << std::endl;
        // }

        std::cout << "Performing semantic analysis..." << std::endl;
        SymbolTable symbolTable;
        SemanticAnalyzer semanticAnalyzer(symbolTable);
        try {
            semanticAnalyzer.analyze(astRoot.get());
            std::cout << "Semantic analysis completed successfully." << std::endl;

            // If reached here, semantic analysis was successful. Proceed to code generation.
            std::cout << "Performing MIPS code generation..." << std::endl;
            CodeGenerator codeGenerator(symbolTable); // symbolTable is from outer scope
            // The generate method might throw its own std::runtime_error for internal issues
            codeGenerator.generate(astRoot.get(), "mips.txt");
            std::cout << "MIPS code generation completed successfully. Output to mips.txt" << std::endl;

        } catch (const std::runtime_error& e) {
            // This will catch errors from either semanticAnalyzer.analyze() or codeGenerator.generate()
            std::cerr << "Compilation failed: " << e.what() << std::endl;
            // Optionally, print AST to help debug if semantic analysis fails (already handled in sema part)
            // If error is from codegen, AST might still be useful.
            // For simplicity, the error_ast_output is tied to semantic errors in the prompt,
            // but could be generalized.
            // If the error was from semantic analysis, error_ast_output.txt might have been created.
            // If it was from code generation, we might want to print it now if not already.
            if (dynamic_cast<const std::runtime_error*>(&e) && std::string(e.what()).find("Semantic error occurred") != std::string::npos) {
                // Semantic error already handled printing error_ast_output.txt if it was configured
                 std::ofstream errorAstFile("error_ast_output.txt");
                 if (errorAstFile.is_open()) {
                     astRoot->print(errorAstFile);
                     errorAstFile.close();
                     std::cout << "AST printed to error_ast_output.txt due to semantic error." << std::endl;
                 }
            }
            return 1; // Indicate error
        }
        
    } else {
        // This part might be less likely if parser throws, but good for safety
        std::cout << "AST generation failed (root is null), skipping further stages." << std::endl;
        return 1; 
    }

    std::cout << "Compiler pipeline finished." << std::endl;
    // Next step would be MIPS code generation
    // ...

    return 0;
}
