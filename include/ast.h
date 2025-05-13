#ifndef AST_H
#define AST_H

#include "token.h" // For TokenType, Token value
#include <vector>
#include <string>
#include <memory>   // For std::unique_ptr
#include <iostream> // For printing
#include <algorithm> // For std::any_of, std::all_of if needed

// Forward declarations for mutual recursion if any (not strictly needed with unique_ptr to base)
struct ExpNode;
struct StmtNode;
struct DeclNode;
struct BlockItemNode; // For items in a block (Decl or Stmt)
struct InitValNode;   // Forward declaration

// --- Helper for printing indentation ---
inline void printIndent(std::ostream& out, int indent) {
    for (int i = 0; i < indent; ++i) out << "  ";
}

// --- Base AST Node ---
struct AstNode {
    size_t lineNumber; // Line number for error reporting and debugging
    AstNode(size_t line = 0) : lineNumber(line) {}
    virtual ~AstNode() = default;
    virtual void print(std::ostream& out, int indent = 0) const = 0;
};

// --- Expressions ---
struct ExpNode : public AstNode {
    ExpNode(size_t line = 0) : AstNode(line) {}
};

struct NumberNode : public ExpNode {
    std::string value;
    NumberNode(const Token& t) : ExpNode(t.line), value(t.value) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "NumberNode: " << value << " (Line: " << lineNumber << ")" << std::endl;
    }
};

struct LValNode : public ExpNode {
    std::string identName;
    std::vector<std::unique_ptr<ExpNode>> arrayIndices;
    LValNode(const Token& idToken) : ExpNode(idToken.line), identName(idToken.value) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "LValNode: " << identName << " (Line: " << lineNumber << ")" << std::endl;
        for (const auto& index : arrayIndices) {
            printIndent(out, indent + 1);
            out << "Index:" << std::endl;
            index->print(out, indent + 2);
        }
    }
};

struct BinaryExpNode : public ExpNode {
    std::unique_ptr<ExpNode> left;
    TokenType opType;
    std::string opString;
    std::unique_ptr<ExpNode> right;

    BinaryExpNode(std::unique_ptr<ExpNode> l, const Token& opToken, std::unique_ptr<ExpNode> r)
        : ExpNode(opToken.line), left(std::move(l)), opType(opToken.type), opString(opToken.value), right(std::move(r)) {}

    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "BinaryExpNode: Op=" << opString << " (Line: " << lineNumber << ")" << std::endl;
        printIndent(out, indent + 1); out << "Left:" << std::endl;
        left->print(out, indent + 2);
        printIndent(out, indent + 1); out << "Right:" << std::endl;
        right->print(out, indent + 2);
    }
};

struct UnaryExpNode : public ExpNode {
    TokenType opType;
    std::string opString;
    std::unique_ptr<ExpNode> operand;

    UnaryExpNode(const Token& opToken, std::unique_ptr<ExpNode> exp)
        : ExpNode(opToken.line), opType(opToken.type), opString(opToken.value), operand(std::move(exp)) {}

    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "UnaryExpNode: Op=" << opString << " (Line: " << lineNumber << ")" << std::endl;
        printIndent(out, indent + 1); out << "Operand:" << std::endl;
        operand->print(out, indent + 2);
    }
};

struct FuncCallNode : public ExpNode {
    std::string funcName;
    std::vector<std::unique_ptr<ExpNode>> args;
    FuncCallNode(const Token& funcIdToken) : ExpNode(funcIdToken.line), funcName(funcIdToken.value) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FuncCallNode: " << funcName << " (Line: " << lineNumber << ")" << std::endl;
        for (size_t i = 0; i < args.size(); ++i) {
            printIndent(out, indent + 1);
            out << "Arg " << i + 1 << ":" << std::endl;
            args[i]->print(out, indent + 2);
        }
    }
};

// --- Statements ---
struct StmtNode : public AstNode {
     StmtNode(size_t line = 0) : AstNode(line) {}
};

struct AssignStmtNode : public StmtNode {
    std::unique_ptr<LValNode> lval;
    std::unique_ptr<ExpNode> rhs; // For regular assignment
    bool isGetintCall = false;    // True if LVal = getint()

    AssignStmtNode(std::unique_ptr<LValNode> lv, std::unique_ptr<ExpNode> r, size_t line)
        : StmtNode(line), lval(std::move(lv)), rhs(std::move(r)), isGetintCall(false) {}
    // Constructor for getint
    AssignStmtNode(std::unique_ptr<LValNode> lv, size_t line)
        : StmtNode(line), lval(std::move(lv)), rhs(nullptr), isGetintCall(true) {}

    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "AssignStmtNode" << (isGetintCall ? " (getint)" : "") << " (Line: " << lineNumber << ")" << std::endl;
        printIndent(out, indent + 1); out << "LVal:" << std::endl;
        lval->print(out, indent + 2);
        if (rhs) {
            printIndent(out, indent + 1); out << "RHS:" << std::endl;
            rhs->print(out, indent + 2);
        }
    }
};

struct ExpStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> expression; // Can be nullptr for an empty statement (just ';')
    ExpStmtNode(std::unique_ptr<ExpNode> exp, size_t line) : StmtNode(line), expression(std::move(exp)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ExpStmtNode" << (expression ? "" : " (Empty)") << " (Line: " << lineNumber << ")" << std::endl;
        if (expression) {
            expression->print(out, indent + 1);
        }
    }
};

struct BlockItemNode : public AstNode { // Common base for Decl and Stmt in a block
    BlockItemNode(size_t line = 0) : AstNode(line) {}
};


struct BlockNode : public StmtNode { // Block can also be a statement
    std::vector<std::unique_ptr<BlockItemNode>> items;
    BlockNode(size_t line = 0) : StmtNode(line) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "BlockNode (Line: " << lineNumber << ")" << std::endl;
        for (const auto& item : items) {
            item->print(out, indent + 1);
        }
    }
};

struct IfStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> condition; // Cond
    std::unique_ptr<StmtNode> thenStmt;
    std::unique_ptr<StmtNode> elseStmt; // Can be nullptr
    IfStmtNode(std::unique_ptr<ExpNode> cond, std::unique_ptr<StmtNode> thenS, std::unique_ptr<StmtNode> elseS, size_t line)
        : StmtNode(line), condition(std::move(cond)), thenStmt(std::move(thenS)), elseStmt(std::move(elseS)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "IfStmtNode (Line: " << lineNumber << ")" << std::endl;
        printIndent(out, indent + 1); out << "Condition:" << std::endl;
        condition->print(out, indent + 2);
        printIndent(out, indent + 1); out << "Then:" << std::endl;
        thenStmt->print(out, indent + 2);
        if (elseStmt) {
            printIndent(out, indent + 1); out << "Else:" << std::endl;
            elseStmt->print(out, indent + 2);
        }
    }
};

struct WhileStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> condition; // Cond
    std::unique_ptr<StmtNode> body;
    WhileStmtNode(std::unique_ptr<ExpNode> cond, std::unique_ptr<StmtNode> b, size_t line)
        : StmtNode(line), condition(std::move(cond)), body(std::move(b)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "WhileStmtNode (Line: " << lineNumber << ")" << std::endl;
        printIndent(out, indent + 1); out << "Condition:" << std::endl;
        condition->print(out, indent + 2);
        printIndent(out, indent + 1); out << "Body:" << std::endl;
        body->print(out, indent + 2);
    }
};

struct BreakStmtNode : public StmtNode {
    BreakStmtNode(size_t line) : StmtNode(line) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "BreakStmtNode (Line: " << lineNumber << ")" << std::endl;
    }
};

struct ContinueStmtNode : public StmtNode {
    ContinueStmtNode(size_t line) : StmtNode(line) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ContinueStmtNode (Line: " << lineNumber << ")" << std::endl;
    }
};

struct ReturnStmtNode : public StmtNode {
    std::unique_ptr<ExpNode> returnValue; // Can be nullptr for 'return;'
    ReturnStmtNode(std::unique_ptr<ExpNode> val, size_t line) : StmtNode(line), returnValue(std::move(val)) {}
     void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ReturnStmtNode" << (returnValue ? "" : " (void)") << " (Line: " << lineNumber << ")" << std::endl;
        if (returnValue) {
            returnValue->print(out, indent + 1);
        }
    }
};

struct FormatStringNode : public AstNode {
    std::string value; // The literal string, e.g., "\"Hello %d\\n\""
    FormatStringNode(const Token& t) : AstNode(t.line), value(t.value) {}
     void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FormatStringNode: " << value << " (Line: " << lineNumber << ")" << std::endl;
    }
};

struct PrintfStmtNode : public StmtNode {
    std::unique_ptr<FormatStringNode> formatString;
    std::vector<std::unique_ptr<ExpNode>> args;
    PrintfStmtNode(std::unique_ptr<FormatStringNode> fs, size_t line) : StmtNode(line), formatString(std::move(fs)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "PrintfStmtNode (Line: " << lineNumber << ")" << std::endl;
        formatString->print(out, indent + 1);
        for (size_t i = 0; i < args.size(); ++i) {
            printIndent(out, indent + 1);
            out << "Arg " << i + 1 << ":" << std::endl;
            args[i]->print(out, indent + 2);
        }
    }
};

// --- Declarations ---
struct DeclNode : public BlockItemNode { // Base for ConstDecl, VarDecl; also a BlockItem
    DeclNode(size_t line = 0) : BlockItemNode(line) {}
};

struct BTypeNode : public AstNode {
    // In this grammar, BType is always 'int'.
    // Could store TokenType::INTTK if needed, or just imply 'int'.
    BTypeNode(size_t line) : AstNode(line) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "BTypeNode: int (Line: " << lineNumber << ")" << std::endl;
    }
};

// ConstInitVal: ConstExp | '{' [ConstInitVal {',' ConstInitVal}] '}'
struct ConstInitValNode : public AstNode {
    std::unique_ptr<ExpNode> singleValue; // If not aggregate
    std::vector<std::unique_ptr<ConstInitValNode>> aggregateValues; // If aggregate
    bool isAggregate;

    ConstInitValNode(std::unique_ptr<ExpNode> val, size_t line)
        : AstNode(line), singleValue(std::move(val)), isAggregate(false) {}
    ConstInitValNode(std::vector<std::unique_ptr<ConstInitValNode>> vals, size_t line)
        : AstNode(line), singleValue(nullptr), aggregateValues(std::move(vals)), isAggregate(true) {}
    // Constructor for empty aggregate {}
    ConstInitValNode(size_t line) : AstNode(line), singleValue(nullptr), isAggregate(true) {}


    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ConstInitValNode" << (isAggregate ? " (Aggregate)" : "") << " (Line: " << lineNumber << ")" << std::endl;
        if (singleValue) {
            singleValue->print(out, indent + 1);
        }
        for (const auto& val : aggregateValues) {
            val->print(out, indent + 1);
        }
    }
};

struct ConstDefNode : public AstNode {
    std::string identName;
    std::vector<std::unique_ptr<ExpNode>> arrayDimensions; // ConstExp for array sizes
    std::unique_ptr<ConstInitValNode> initVal;
    ConstDefNode(const Token& idToken) : AstNode(idToken.line), identName(idToken.value) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ConstDefNode: " << identName << " (Line: " << lineNumber << ")" << std::endl;
        for (const auto& dim : arrayDimensions) {
            printIndent(out, indent + 1); out << "Dimension:" << std::endl;
            dim->print(out, indent + 2);
        }
        if (initVal) { // initVal is mandatory for ConstDef
             printIndent(out, indent + 1); out << "InitVal:" << std::endl;
            initVal->print(out, indent + 2);
        }
    }
};

struct ConstDeclNode : public DeclNode {
    std::unique_ptr<BTypeNode> type; // 'const' BType ...
    std::vector<std::unique_ptr<ConstDefNode>> constDefs;
    ConstDeclNode(std::unique_ptr<BTypeNode> bt, size_t line) : DeclNode(line), type(std::move(bt)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "ConstDeclNode (Line: " << lineNumber << ")" << std::endl;
        type->print(out, indent + 1);
        for (const auto& def : constDefs) {
            def->print(out, indent + 1);
        }
    }
};

// InitVal: Exp | '{' [InitVal {',' InitVal}] '}'
struct InitValNode : public AstNode {
    std::unique_ptr<ExpNode> singleValue;
    std::vector<std::unique_ptr<InitValNode>> aggregateValues;
    bool isAggregate;

    InitValNode(std::unique_ptr<ExpNode> val, size_t line)
        : AstNode(line), singleValue(std::move(val)), isAggregate(false) {}
    InitValNode(std::vector<std::unique_ptr<InitValNode>> vals, size_t line)
        : AstNode(line), singleValue(nullptr), aggregateValues(std::move(vals)), isAggregate(true) {}
    // Constructor for empty aggregate {}
    InitValNode(size_t line) : AstNode(line), singleValue(nullptr), isAggregate(true) {}

    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "InitValNode" << (isAggregate ? " (Aggregate)" : "") << " (Line: " << lineNumber << ")" << std::endl;
        if (singleValue) {
            singleValue->print(out, indent + 1);
        }
        for (const auto& val : aggregateValues) {
            val->print(out, indent + 1);
        }
    }
};


struct VarDefNode : public AstNode {
    std::string identName;
    std::vector<std::unique_ptr<ExpNode>> arrayDimensions; // ConstExp for array sizes
    std::unique_ptr<InitValNode> initVal; // Optional
    VarDefNode(const Token& idToken) : AstNode(idToken.line), identName(idToken.value) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "VarDefNode: " << identName << " (Line: " << lineNumber << ")" << std::endl;
        for (const auto& dim : arrayDimensions) {
            printIndent(out, indent + 1); out << "Dimension:" << std::endl;
            dim->print(out, indent + 2);
        }
        if (initVal) {
            printIndent(out, indent + 1); out << "InitVal:" << std::endl;
            initVal->print(out, indent + 2);
        }
    }
};

struct VarDeclNode : public DeclNode {
    std::unique_ptr<BTypeNode> type;
    std::vector<std::unique_ptr<VarDefNode>> varDefs;
    VarDeclNode(std::unique_ptr<BTypeNode> bt, size_t line) : DeclNode(line), type(std::move(bt)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "VarDeclNode (Line: " << lineNumber << ")" << std::endl;
        type->print(out, indent + 1);
        for (const auto& def : varDefs) {
            def->print(out, indent + 1);
        }
    }
};

// --- Functions ---
struct FuncTypeNode : public AstNode {
    TokenType type; // VOIDTK or INTTK
    FuncTypeNode(const Token& typeToken) : AstNode(typeToken.line), type(typeToken.type) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FuncTypeNode: " << (type == TokenType::VOIDTK ? "void" : "int") << " (Line: " << lineNumber << ")" << std::endl;
    }
};

struct FuncFParamNode : public AstNode {
    std::unique_ptr<BTypeNode> type;
    std::string paramName;
    bool isArray = false; // True if '[]' is present after Ident
    std::vector<std::unique_ptr<ExpNode>> arrayPointerDimensions; // For subsequent '[ConstExp]' in C-style `int arr[][10]`
                                                                // But grammar is `BType Ident ['[' ']' {'[' ConstExp ']'}]`
                                                                // This means `int p[]` or `int p[][DIM]`
                                                                // PDF says "只包含普通变量" for FuncFParam.
                                                                // I will follow the PDF and simplify this. If full array params are needed, this needs adjustment.
                                                                // For now, assuming simple `BType Ident`.
    FuncFParamNode(std::unique_ptr<BTypeNode> paramType, const Token& nameToken)
        : AstNode(nameToken.line), type(std::move(paramType)), paramName(nameToken.value), isArray(false) {}

    // If array parameters were to be fully supported as per the EBNF in parser.h:
    // FuncFParamNode(std::unique_ptr<BTypeNode> paramType, const Token& nameToken, bool isArr, std::vector<std::unique_ptr<ExpNode>> dims)
    //     : AstNode(nameToken.line), type(std::move(paramType)), paramName(nameToken.value), isArray(isArr), arrayPointerDimensions(std::move(dims)) {}


    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FuncFParamNode: " << paramName << (isArray ? " (Array Param)" : "") << " (Line: " << lineNumber << ")" << std::endl;
        type->print(out, indent + 1);
        if (isArray) {
             for(const auto& dim : arrayPointerDimensions) {
                printIndent(out, indent + 1); out << "Dim (pointer sense):" << std::endl;
                dim->print(out, indent + 2);
             }
        }
    }
};

struct FuncFParamsNode : public AstNode {
    std::vector<std::unique_ptr<FuncFParamNode>> params;
    FuncFParamsNode(size_t line = 0) : AstNode(line) {}
     void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FuncFParamsNode (Line: " << lineNumber << ")" << std::endl;
        for (const auto& param : params) {
            param->print(out, indent + 1);
        }
    }
};

struct FuncDefNode : public AstNode { // Also a BlockItem conceptually for CompUnit
    std::unique_ptr<FuncTypeNode> funcType;
    std::string funcName;
    std::unique_ptr<FuncFParamsNode> params; // Can be nullptr if no params
    std::unique_ptr<BlockNode> body;
    FuncDefNode(std::unique_ptr<FuncTypeNode> ft, const Token& nameToken, 
                std::unique_ptr<FuncFParamsNode> fps, std::unique_ptr<BlockNode> b)
        : AstNode(nameToken.line), funcType(std::move(ft)), funcName(nameToken.value), 
          params(std::move(fps)), body(std::move(b)) {}
    void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "FuncDefNode: " << funcName << " (Line: " << lineNumber << ")" << std::endl;
        funcType->print(out, indent + 1);
        if (params) {
            params->print(out, indent + 1);
        }
        body->print(out, indent + 1);
    }
};

struct MainFuncDefNode : public AstNode {
    // Implicitly 'int main()'
    std::unique_ptr<BlockNode> body;
    MainFuncDefNode(std::unique_ptr<BlockNode> b, size_t line) : AstNode(line), body(std::move(b)) {}
     void print(std::ostream& out, int indent) const override {
        printIndent(out, indent);
        out << "MainFuncDefNode (int main) (Line: " << lineNumber << ")" << std::endl;
        body->print(out, indent + 1);
    }
};

// --- Compilation Unit (Root) ---
// CompUnit: {Decl} {FuncDef} MainFuncDef
struct CompUnitNode : public AstNode {
    std::vector<std::unique_ptr<AstNode>> globalDefinitions; // Decl or FuncDef
    std::unique_ptr<MainFuncDefNode> mainFunc; // Mandatory
    CompUnitNode(size_t line = 0) : AstNode(line) {}
    void print(std::ostream& out, int indent = 0) const override {
        printIndent(out, indent);
        out << "CompUnitNode (Line: " << lineNumber << ")" << std::endl;
        for (const auto& def : globalDefinitions) {
            def->print(out, indent + 1);
        }
        if (mainFunc) { // Should always be present
            mainFunc->print(out, indent + 1);
        }
    }
};


#endif // AST_H
