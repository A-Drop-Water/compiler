#ifndef CODE_GENERATOR_H
#define CODE_GENERATOR_H

#include "ast.h" // Needs full AST definitions
#include "symbol_table.h" // May need symbol table for addresses, types, etc.
#include <iostream> // For output stream (to mips.txt)
#include <fstream>
#include <string>
#include <vector>
#include <map> // For register allocation or tracking variable locations

class CodeGenerator {
public:
    CodeGenerator(SymbolTable& table);
    void generate(CompUnitNode* compUnit, const std::string& outputFilename);

private:
    SymbolTable& symbolTable;
    std::ofstream mipsFile; // Output file stream for MIPS code

    // MIPS Register Management (simplified)
    // Example: track which temp registers are free/used.
    // For a more complex compiler, a dedicated RegisterAllocator class would be used.
    int currentLabelId; // For generating unique labels
    std::string newLabel(const std::string& prefix = "L");

    // Stack Management
    int currentStackOffset; // Tracks current offset from frame pointer ($fp) for local vars
    std::map<std::string, int> localVarOffsets; // Maps local var name to stack offset from $fp
    std::map<std::string, int> funcParamOffsets; // Maps param name to stack offset (for caller/callee)
    int currentFunctionArgSpace; // Space needed on stack for arguments to functions being called by current function


    // Emit various MIPS instructions (helper methods)
    void emit(const std::string& instruction);
    void emitLabel(const std::string& label);
    void emitComment(const std::string& comment);

    // Data segment helpers
    void emitDataSegment();
    void emitGlobalVar(const std::string& varName, bool isArray, int arraySize = 0); // arraySize in words
    void emitStringConstant(const std::string& strLabel, const std::string& strValue);

    // Text segment helpers
    void emitTextSegment();
    void emitFunctionProlog(const std::string& funcName, int localStackSize);
    void emitFunctionEpilog(const std::string& funcName, bool isMain = false);


    // AST Traversal methods for code generation
    void visit(CompUnitNode* node);
    void visit(DeclNode* node); // For global declarations
    void visit(ConstDeclNode* node); // Global constants
    void visit(VarDeclNode* node);   // Global variables

    void visit(FuncDefNode* node);
    void visit(MainFuncDefNode* node);
    // visit(FuncFParamNode* node); // Parameters handled within FuncDefNode

    void visit(BlockNode* node, bool isNewScope = true); // isNewScope for nested blocks
    void visit(BlockItemNode* node);

    void visit(StmtNode* node); // Dispatcher
    void visit(AssignStmtNode* node);
    void visit(ExpStmtNode* node);
    void visit(IfStmtNode* node);
    void visit(WhileStmtNode* node);
    void visit(BreakStmtNode* node);     // Needs context (target label for loop end)
    void visit(ContinueStmtNode* node);  // Needs context (target label for loop start)
    void visit(ReturnStmtNode* node);
    void visit(PrintfStmtNode* node);

    // Expression code generation (typically results in value in a register, e.g., $v0 or a temp reg)
    // These might return the register name where the result is stored.
    std::string visit(ExpNode* node); 
    std::string visit(NumberNode* node);
    std::string visit(LValNode* node, boolต้องการAddress = false); // needsAddress to load address vs value
    std::string visit(BinaryExpNode* node);
    std::string visit(UnaryExpNode* node);
    std::string visit(FuncCallNode* node);
    std::string visit(GetIntNode* node);
    
    // Keep track of loop labels for break/continue
    std::vector<std::string> loopStartLabels;
    std::vector<std::string> loopEndLabels;

    // Register allocation helpers (very basic for now)
    std::string getTempReg(); // Gets a free temporary register like $t0-$t9
    void freeTempReg(const std::string& reg); // Marks a temp register as free
    std::vector<bool> tempRegStatus; // True if $ti is free
    
    // Store string literals to be emitted in data segment
    std::map<std::string, std::string> stringLiterals; // label -> string value
    std::string addStringLiteral(const std::string& str); // Adds string, returns label

    std::string currentFuncEpilogueLabel; // Label for the current function's epilogue
};

#endif // CODE_GENERATOR_H
