#ifndef PARSER_H
#define PARSER_H

#include "lexer.h" // Needs Lexer to get tokens
#include "ast.h"   // Needs AST node definitions
#include <vector>
#include <memory> // For std::unique_ptr

class Parser {
public:
    Parser(Lexer& lexer);
    std::unique_ptr<CompUnitNode> parseCompUnit();

private:
    Lexer& lexer;
    Token currentToken;
    Token lookaheadToken; // For one token lookahead

    void advanceToken();    // Consumes currentToken, currentToken becomes lookaheadToken, new lookahead fetched
    void expectToken(TokenType expectedType); // Consumes token if it matches, else error

    // Parsing methods for grammar rules (examples)
    std::unique_ptr<DeclNode> parseDecl();
    std::unique_ptr<ConstDeclNode> parseConstDecl();
    std::unique_ptr<ConstDefNode> parseConstDef();
    std::unique_ptr<ConstExpNode> parseConstExp();
    std::unique_ptr<ConstInitValNode> parseConstInitVal();
    
    std::unique_ptr<VarDeclNode> parseVarDecl();
    std::unique_ptr<VarDefNode> parseVarDef();
    std::unique_ptr<InitValNode> parseInitVal();

    std::unique_ptr<FuncDefNode> parseFuncDef();
    std::unique_ptr<MainFuncDefNode> parseMainFuncDef();
    FuncType parseFuncType();
    std::vector<std::unique_ptr<FuncFParamNode>> parseFuncFParams();
    std::unique_ptr<FuncFParamNode> parseFuncFParam();

    std::unique_ptr<BlockNode> parseBlock();
    std::unique_ptr<BlockItemNode> parseBlockItem();
    
    std::unique_ptr<StmtNode> parseStmt();
    // Specific statement parsing methods
    std::unique_ptr<StmtNode> parseAssignOrExpStmtOrLValAsExpStmt(); // Handles LVal = Exp; and [Exp]; (where LVal can be an Exp)
    std::unique_ptr<IfStmtNode> parseIfStmt();
    std::unique_ptr<WhileStmtNode> parseWhileStmt();
    std::unique_ptr<ReturnStmtNode> parseReturnStmt();
    std::unique_ptr<PrintfStmtNode> parsePrintfStmt();
    // LVal = getint() is handled by parseAssignOrExpStmtOrLValAsExpStmt with GetIntNode as Exp

    std::unique_ptr<ExpNode> parseExp();
    std::unique_ptr<ExpNode> parseCond(); // Cond -> LOrExp
    std::unique_ptr<LValNode> parseLVal();
    std::unique_ptr<ExpNode> parsePrimaryExp();
    std::unique_ptr<ExpNode> parseUnaryExp();
    // For operator precedence, e.g., parseAddExp, parseMulExp etc.
    std::unique_ptr<ExpNode> parseMulExp(); // Precedence: *, /, %
    std::unique_ptr<ExpNode> parseAddExp(); // Precedence: +, -
    std::unique_ptr<ExpNode> parseRelExp(); // Precedence: <, >, <=, >=
    std::unique_ptr<ExpNode> parseEqExp();  // Precedence: ==, !=
    std::unique_ptr<ExpNode> parseLAndExp(); // Precedence: &&
    std::unique_ptr<ExpNode> parseLOrExp();  // Precedence: ||

    std::vector<std::unique_ptr<ExpNode>> parseFuncRParams(); // For function call arguments

    // Helper to get current token's line number
    int currentLine() { return currentToken.line; }
};

#endif // PARSER_H
