#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>
#include <stdexcept>
#include <fstream>
#include <memory>   // For std::unique_ptr
#include "token.h"
#include "ast.h"    // Include AST node definitions

/**
 * @brief 语法分析器类，用于将token序列解析为抽象语法树 (AST)
 * * 此类实现了递归下降语法分析器，能够构建程序的AST结构
 * 包括常量声明、变量声明、函数定义、表达式、语句等语法单元
 */
class Parser {
private:
    std::vector<Token> tokens;    // 词法分析器产生的token序列
    size_t currentPos = 0;        // 当前正在处理的token位置
    std::ofstream& outFile;       // 输出文件流 (用于打印AST)
  
    /**
     * @brief 获取当前token的引用，如果越界则抛出异常
     * @return 当前位置的token引用
     */
    const Token& getCurrentToken() const;

    /**
     * @brief 获取当前token的行号，如果越界则返回0或抛出
     * @return 当前token的行号
     */
    size_t getCurrentLine() const;


    /**
     * @brief 检查当前token的类型
     * @param type 需要检查的token类型
     * @return 如果当前token类型匹配则返回true，否则返回false
     */
    bool check(TokenType type) const;

    /**
     * @brief 消耗当前token并前进到下一个token
     * @return 被消耗的token的拷贝
     */
    Token consume();

    /**
     * @brief 检查当前token类型并消耗，如果不匹配则抛出异常
     * @param type 需要消耗的token类型
     * @param errorMessage 错误信息（如果类型不匹配）
     * @return 被消耗的token的拷贝
     */
    Token consume(TokenType type, const std::string& errorMessage);

    /**
     * @brief 预览后续的token
     * @param type 需要预览的token类型
     * @param offset 向前预览的偏移量
     * @return 如果预览位置的token类型匹配则返回true，否则返回false
     */
    bool lookAhead(TokenType type, int offset = 1) const;

    // --- AST Node Parsing Methods ---
    std::unique_ptr<CompUnitNode> parseCompUnit();
    std::unique_ptr<AstNode> parseDeclOrFuncDef(); // Helper for CompUnit
    std::unique_ptr<FuncDefNode> parseFuncDef();
    std::unique_ptr<FuncTypeNode> parseFuncType();
    std::unique_ptr<FuncFParamsNode> parseFuncFParams();
    std::unique_ptr<FuncFParamNode> parseFuncFParam();
    std::unique_ptr<MainFuncDefNode> parseMainFuncDef();
    std::unique_ptr<BlockNode> parseBlock();
    std::unique_ptr<BlockItemNode> parseBlockItem(); // Returns DeclNode or StmtNode wrapped in BlockItemNode
    std::unique_ptr<DeclNode> parseDecl(); // Returns ConstDeclNode or VarDeclNode
    std::unique_ptr<ConstDeclNode> parseConstDecl();
    std::unique_ptr<BTypeNode> parseBType();
    std::unique_ptr<ConstDefNode> parseConstDef();
    std::unique_ptr<ConstInitValNode> parseConstInitVal();
    std::unique_ptr<VarDeclNode> parseVarDecl();
    std::unique_ptr<VarDefNode> parseVarDef();
    std::unique_ptr<InitValNode> parseInitVal();
    std::unique_ptr<StmtNode> parseStmt();
    
    // Expressions
    std::unique_ptr<ExpNode> parseExp();
    std::unique_ptr<ExpNode> parseCond(); // Cond ::= LOrExp
    std::unique_ptr<ExpNode> parseLOrExp();
    std::unique_ptr<ExpNode> parseLAndExp();
    std::unique_ptr<ExpNode> parseEqExp();
    std::unique_ptr<ExpNode> parseRelExp();
    std::unique_ptr<ExpNode> parseAddExp();
    std::unique_ptr<ExpNode> parseMulExp();
    std::unique_ptr<ExpNode> parseUnaryExp();
    std::unique_ptr<UnaryExpNode> parseUnaryOpAndExp(); // Helper for UnaryOp UnaryExp
    std::unique_ptr<ExpNode> parsePrimaryExp();
    std::unique_ptr<LValNode> parseLVal();
    std::unique_ptr<NumberNode> parseNumber();
    std::vector<std::unique_ptr<ExpNode>> parseFuncRParams(); // Returns a vector of argument expressions
    std::unique_ptr<ExpNode> parseConstExp(); // ConstExp ::= AddExp

    // Specific statement types
    std::unique_ptr<IfStmtNode> parseIfStmt();
    std::unique_ptr<WhileStmtNode> parseWhileStmt();
    std::unique_ptr<FormatStringNode> parseFormatString();


public:
    /**
     * @brief 构造语法分析器
     * @param tokens 词法分析产生的token序列
     * @param out 输出文件流 (用于打印AST)
     */
    Parser(const std::vector<Token>& tokens, std::ofstream& out);

    /**
     * @brief 执行语法分析并构建AST
     * @return 指向根节点 (CompUnitNode) 的智能指针
     */
    std::unique_ptr<CompUnitNode> parse();
};

#endif 
