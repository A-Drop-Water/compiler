#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

#include "ast.h" // Needs full AST definitions
#include "symbol_table.h" // Needs SymbolTable
#include <vector>
#include <string>
#include <stdexcept> // For semantic errors

// Helper function to convert FuncType enum from AST to DataType enum for SymbolTable
DataType astFuncTypeToDataType(FuncType type);

class SemanticAnalyzer {
public:
    SemanticAnalyzer(SymbolTable& table);

    // Main entry point for semantic analysis
    void analyze(CompUnitNode* compUnit);

private:
    SymbolTable& symbolTable;
    FuncDefNode* currentFunction; // To keep track of the current function context for return statements etc.
    int loopDepth; // To check for break/continue validity

    // AST traversal methods (Visitors)
    // These will build the symbol table and perform initial checks.
    void visit(CompUnitNode* node);
    void visit(DeclNode* node); // Dispatcher for ConstDeclNode or VarDeclNode
    void visit(ConstDeclNode* node);
    void visit(ConstDefNode* node); // Process individual constant definition
    void visit(VarDeclNode* node);
    void visit(VarDefNode* node);   // Process individual variable definition
    
    void visit(FuncDefNode* node);
    void visit(MainFuncDefNode* node);
    void visit(FuncFParamNode* node, SymbolEntry& funcEntry); // Add param to funcEntry and symbol table

    void visit(BlockNode* node, bool isFunctionBody = false); // isFunctionBody to control scope for params
    void visit(BlockItemNode* node);

    void visit(StmtNode* node); // Dispatcher for different statements
    void visit(AssignStmtNode* node);
    void visit(ExpStmtNode* node);
    void visit(IfStmtNode* node);
    void visit(WhileStmtNode* node);
    void visit(BreakStmtNode* node);
    void visit(ContinueStmtNode* node);
    void visit(ReturnStmtNode* node);
    void visit(PrintfStmtNode* node);

    // Expression traversal methods (will also perform type checking)
    // These return the DataType of the expression.
    DataType visit(ExpNode* node); // Dispatcher for different expression types
    DataType visit(NumberNode* node);
    DataType visit(LValNode* node);
    DataType visit(BinaryExpNode* node);
    DataType visit(UnaryExpNode* node);
    DataType visit(FuncCallNode* node);
    DataType visit(GetIntNode* node); // getint() returns an int

    // For ConstExp, we might need a separate visitor or pass a flag
    // to ensure it's compile-time evaluatable and to get its value.
    // For now, treat ConstExp's expression like a regular Exp for type checking.
    int evaluateConstExp(ConstExpNode* constExp); // Example: a helper to get const value

    // Helper to report semantic errors
    void reportError(const std::string& message, int lineNumber);
};

#endif // SEMANTIC_ANALYZER_H
