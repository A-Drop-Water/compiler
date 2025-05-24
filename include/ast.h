#ifndef AST_H
#define AST_H

#include <string>
#include <vector>
#include <memory> // For std::unique_ptr
#include <iostream> // For std::ostream
#include "token.h" // For TokenType, and potentially to store token info

// Forward declarations for node types if they reference each other
struct ExpNode;
struct StmtNode;
struct DeclNode;
struct BlockItemNode; // Can be DeclNode or StmtNode
struct ConstExpNode;
struct InitValNode;
struct ConstInitValNode;

// Enum for binary operator types (as an example, can be expanded)
enum class BinOpType {
    ADD, SUB, MUL, DIV, MOD,
    EQ, NEQ, LT, GT, LTE, GTE,
    AND, OR
    // Add other binary operators as needed
};

// Enum for unary operator types
enum class UnaryOpType {
    PLUS, MINUS, NOT
};

// Base class for all AST nodes
struct AstNode {
    virtual ~AstNode() = default;
    // Optionally, add a virtual method for an AST visitor pattern later
    // virtual void accept(class AstVisitor& visitor) = 0;
    // Store line number for error reporting
    int lineNumber = 0; 
    AstNode(int line = 0) : lineNumber(line) {}

    virtual void print(std::ostream& out, int indentLevel = 0) const {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "AstNode (line " << lineNumber << ")\n";
    }
};

// Expressions
struct ExpNode : public AstNode { 
    ExpNode(int line = 0) : AstNode(line) {} 
};

struct NumberNode : public ExpNode {
    int value;
    NumberNode(int val, int line) : ExpNode(line), value(val) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "NumberNode (value: " << value << ", line: " << lineNumber << ")\n";
    }
};

struct LValNode : public ExpNode {
    std::string ident;
    std::unique_ptr<ExpNode> index; // Null if not an array access
    LValNode(std::string id, int line, std::unique_ptr<ExpNode> idx = nullptr) 
        : ExpNode(line), ident(id), index(std::move(idx)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "LValNode (ident: " << ident << ", line: " << lineNumber << ")\n";
        if (index) {
            for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
            out << "Index:\n";
            index->print(out, indentLevel + 2);
        }
    }
};

struct BinaryExpNode : public ExpNode {
    BinOpType op;
    std::unique_ptr<ExpNode> left;
    std::unique_ptr<ExpNode> right;
    BinaryExpNode(BinOpType o, std::unique_ptr<ExpNode> l, std::unique_ptr<ExpNode> r, int line)
        : ExpNode(line), op(o), left(std::move(l)), right(std::move(r)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        // TODO: Convert BinOpType to string for printing if possible, or just print enum int value
        out << "BinaryExpNode (op: " << static_cast<int>(op) << ", line: " << lineNumber << ")\n";
        if (left) left->print(out, indentLevel + 1);
        else {
            for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
            out << "Left: (null)\n";
        }
        if (right) right->print(out, indentLevel + 1);
        else {
            for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
            out << "Right: (null)\n";
        }
    }
};

struct UnaryExpNode : public ExpNode {
    UnaryOpType op;
    std::unique_ptr<ExpNode> operand;
    // For function calls, op can be a special value or handled by a different node type
    UnaryExpNode(UnaryOpType o, std::unique_ptr<ExpNode> oper, int line)
        : ExpNode(line), op(o), operand(std::move(oper)) {}
};

struct FuncCallNode : public ExpNode {
    std::string funcIdent;
    std::vector<std::unique_ptr<ExpNode>> args;
    FuncCallNode(std::string id, std::vector<std::unique_ptr<ExpNode>> a, int line)
        : ExpNode(line), funcIdent(id), args(std::move(a)) {}
};

// Constant Expressions (subset of ExpNode, evaluated at compile time)
struct ConstExpNode : public AstNode { // May evaluate to an int
    std::unique_ptr<ExpNode> exp; // The actual expression
    ConstExpNode(std::unique_ptr<ExpNode> e, int line) : AstNode(line), exp(std::move(e)) {}
};


// Initial Values
struct InitValNode : public AstNode {
    std::vector<std::unique_ptr<InitValNode>> elements; // For array initialization
    std::unique_ptr<ExpNode> singleExp; // For single expression initialization

    InitValNode(std::unique_ptr<ExpNode> sExp, int line) : AstNode(line), singleExp(std::move(sExp)) {}
    InitValNode(std::vector<std::unique_ptr<InitValNode>> elems, int line) : AstNode(line), elements(std::move(elems)) {}
};

struct ConstInitValNode : public AstNode {
    std::vector<std::unique_ptr<ConstInitValNode>> elements; // For array initialization
    std::unique_ptr<ConstExpNode> singleConstExp; // For single const expression

    ConstInitValNode(std::unique_ptr<ConstExpNode> sConstExp, int line) : AstNode(line), singleConstExp(std::move(sConstExp)) {}
    ConstInitValNode(std::vector<std::unique_ptr<ConstInitValNode>> elems, int line) : AstNode(line), elements(std::move(elems)) {}
};


// Declarations
struct DeclNode : public AstNode { 
    DeclNode(int line = 0) : AstNode(line) {}
};

struct VarDefNode : public AstNode {
    std::string ident;
    std::vector<std::unique_ptr<ConstExpNode>> arrayDimensions; // Empty if not an array
    std::unique_ptr<InitValNode> initVal; // Null if no initializer
    
    VarDefNode(std::string id, int line, std::unique_ptr<InitValNode> init = nullptr) 
        : AstNode(line), ident(id), initVal(std::move(init)) {}
    
    VarDefNode(std::string id, std::vector<std::unique_ptr<ConstExpNode>> dims, int line, std::unique_ptr<InitValNode> init = nullptr)
        : AstNode(line), ident(id), arrayDimensions(std::move(dims)), initVal(std::move(init)) {}
};

struct VarDeclNode : public DeclNode {
    // BType is implicitly 'int' based on grammar
    std::vector<std::unique_ptr<VarDefNode>> varDefs;
    VarDeclNode(std::vector<std::unique_ptr<VarDefNode>> defs, int line) : DeclNode(line), varDefs(std::move(defs)) {}
};

struct ConstDefNode : public AstNode {
    std::string ident;
    std::vector<std::unique_ptr<ConstExpNode>> arrayDimensions; // Empty if not an array
    std::unique_ptr<ConstInitValNode> constInitVal;
    
    ConstDefNode(std::string id, std::unique_ptr<ConstInitValNode> val, int line)
        : AstNode(line), ident(id), constInitVal(std::move(val)) {}

    ConstDefNode(std::string id, std::vector<std::unique_ptr<ConstExpNode>> dims, std::unique_ptr<ConstInitValNode> val, int line)
        : AstNode(line), ident(id), arrayDimensions(std::move(dims)), constInitVal(std::move(val)) {}
};

struct ConstDeclNode : public DeclNode {
    // BType is implicitly 'int'
    std::vector<std::unique_ptr<ConstDefNode>> constDefs;
    ConstDeclNode(std::vector<std::unique_ptr<ConstDefNode>> defs, int line) : DeclNode(line), constDefs(std::move(defs)) {}
};


// Statements
struct StmtNode : public AstNode { 
    StmtNode(int line = 0) : AstNode(line) {}
};

struct BlockNode : public StmtNode { // Also used for function bodies
    std::vector<std::unique_ptr<BlockItemNode>> items;
    BlockNode(std::vector<std::unique_ptr<BlockItemNode>> i, int line) : StmtNode(line), items(std::move(i)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "BlockNode (line: " << lineNumber << ")\n";
        for (const auto& item : items) {
            if (item) item->print(out, indentLevel + 1);
        }
    }
};

// BlockItem can be a Decl or a Stmt. Using a wrapper or std::variant if C++17 is available.
// For simplicity with unique_ptr, we might need separate vectors or a base BlockItemNode.
struct BlockItemNode : public AstNode {
    std::unique_ptr<DeclNode> decl;
    std::unique_ptr<StmtNode> stmt;

    BlockItemNode(std::unique_ptr<DeclNode> d, int line) : AstNode(line), decl(std::move(d)), stmt(nullptr) {}
    BlockItemNode(std::unique_ptr<StmtNode> s, int line) : AstNode(line), decl(nullptr), stmt(std::move(s)) {}
};


struct AssignStmtNode : public StmtNode {
    std::unique_ptr<LValNode> lval;
    std::unique_ptr<ExpNode> exp; // Can be regular Exp or GetIntExp
    AssignStmtNode(std::unique_ptr<LValNode> lv, std::unique_ptr<ExpNode> e, int line)
        : StmtNode(line), lval(std::move(lv)), exp(std::move(e)) {}
};

// For LVal = getint(); we can make GetIntNode a type of ExpNode
struct GetIntNode : public ExpNode {
    GetIntNode(int line) : ExpNode(line) {}
};


struct ExpStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> exp; // Can be null for empty statement ';'
    ExpStmtNode(int line, std::unique_ptr<ExpNode> e = nullptr) : StmtNode(line), exp(std::move(e)) {}
};

struct IfStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> condition; // Cond -> LOrExp
    std::unique_ptr<StmtNode> thenStmt;
    std::unique_ptr<StmtNode> elseStmt; // Can be null
    IfStmtNode(std::unique_ptr<ExpNode> cond, std::unique_ptr<StmtNode> thenS, int line, std::unique_ptr<StmtNode> elseS = nullptr)
        : StmtNode(line), condition(std::move(cond)), thenStmt(std::move(thenS)), elseStmt(std::move(elseS)) {}
};

struct WhileStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> condition; // Cond -> LOrExp
    std::unique_ptr<StmtNode> body;
    WhileStmtNode(std::unique_ptr<ExpNode> cond, std::unique_ptr<StmtNode> b, int line)
        : StmtNode(line), condition(std::move(cond)), body(std::move(b)) {}
};

struct BreakStmtNode : public StmtNode { 
    BreakStmtNode(int line) : StmtNode(line) {} 
};
struct ContinueStmtNode : public StmtNode { 
    ContinueStmtNode(int line) : StmtNode(line) {} 
};

struct ReturnStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> returnExp; // Can be null for 'return;' in void function
    ReturnStmtNode(int line, std::unique_ptr<ExpNode> retExp = nullptr) : StmtNode(line), returnExp(std::move(retExp)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "ReturnStmtNode (line: " << lineNumber << ")\n";
        if (returnExp) returnExp->print(out, indentLevel + 1);
        else {
            for (int i = 0; i < indentLevel+1; ++i) out << "  ";
            out << "(No return expression)\n";
        }
    }
};

struct PrintfStmtNode : public StmtNode {
    std::string formatString; // The raw format string token value
    std::vector<std::unique_ptr<ExpNode>> args;
    PrintfStmtNode(std::string fmt, std::vector<std::unique_ptr<ExpNode>> a, int line)
        : StmtNode(line), formatString(fmt), args(std::move(a)) {}
};


// Function Definitions
enum class FuncType { VOID, INT };

struct FuncFParamNode : public AstNode {
    // BType is 'int'
    std::string ident;
    // Grammar says `BType Ident`, not array.
    // bool isArray; // Future extension: for array parameters
    // std::vector<std::unique_ptr<ConstExpNode>> arrayDimensions; // if isArray is true
    FuncFParamNode(std::string id, int line) : AstNode(line), ident(id) {}
};

struct FuncDefNode : public AstNode {
    FuncType funcType;
    std::string ident;
    std::vector<std::unique_ptr<FuncFParamNode>> params;
    std::unique_ptr<BlockNode> body;
    FuncDefNode(FuncType type, std::string id, std::vector<std::unique_ptr<FuncFParamNode>> p, std::unique_ptr<BlockNode> b, int line)
        : AstNode(line), funcType(type), ident(id), params(std::move(p)), body(std::move(b)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "FuncDefNode (type: " << (funcType == FuncType::INT ? "int" : "void") 
            << ", ident: " << ident << ", line: " << lineNumber << ")\n";
        for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
        out << "Params:\n";
        for (const auto& param : params) {
            if (param) param->print(out, indentLevel + 2);
        }
        if (body) body->print(out, indentLevel + 1);
    }
};

struct MainFuncDefNode : public AstNode {
    // Type is 'int', name is 'main', no params
    std::unique_ptr<BlockNode> body;
    MainFuncDefNode(std::unique_ptr<BlockNode> b, int line) : AstNode(line), body(std::move(b)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "MainFuncDefNode (line: " << lineNumber << ")\n";
        if (body) body->print(out, indentLevel + 1);
    }
};


// Compilation Unit (Root of AST)
struct CompUnitNode : public AstNode {
    std::vector<std::unique_ptr<DeclNode>> globalDecls;
    std::vector<std::unique_ptr<FuncDefNode>> funcDefs;
    std::unique_ptr<MainFuncDefNode> mainFuncDef;

    CompUnitNode(std::vector<std::unique_ptr<DeclNode>> decls,
                 std::vector<std::unique_ptr<FuncDefNode>> funcs,
                 std::unique_ptr<MainFuncDefNode> mainFunc, int line = 0)
        : AstNode(line), globalDecls(std::move(decls)), funcDefs(std::move(funcs)), mainFuncDef(std::move(mainFunc)) {}
    void print(std::ostream& out, int indentLevel = 0) const override {
        for (int i = 0; i < indentLevel; ++i) out << "  ";
        out << "CompUnitNode (line: " << lineNumber << ")\n";
        
        for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
        out << "Global Decls:\n";
        for (const auto& decl : globalDecls) {
            if (decl) decl->print(out, indentLevel + 2);
        }

        for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
        out << "Function Defs:\n";
        for (const auto& funcDef : funcDefs) {
            if (funcDef) funcDef->print(out, indentLevel + 2);
        }

        if (mainFuncDef) {
            for (int i = 0; i < indentLevel + 1; ++i) out << "  ";
            out << "Main Function Def:\n";
            mainFuncDef->print(out, indentLevel + 2);
        }
    }
};


#endif // AST_H
