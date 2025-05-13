#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>
#include <stdexcept>
#include <fstream>
#include "token.h"

/**
 * @brief 语法分析器类，用于将token序列解析为语法树
 * 
 * 此类实现了递归下降语法分析器，能够解析符合语法规则的程序
 * 包括常量声明、变量声明、函数定义、表达式、语句等语法单元
 */
class Parser {
private:
    std::vector<Token> tokens;    // 词法分析器产生的token序列
    size_t currentPos = 0;        // 当前正在处理的token位置
    std::ofstream& outFile;       // 输出文件流
  
    /**
     * @brief 获取当前token
     * @return 当前位置的token引用
     */
    Token& getCurrentToken();

    /**
     * @brief 检查当前token的类型
     * @param type 需要检查的token类型
     * @return 如果当前token类型匹配则返回true，否则返回false
     */
    bool check(TokenType type);

    /**
     * @brief 消耗当前token并前进到下一个token
     */
    void consume();

    /**
     * @brief 检查当前token类型并消耗
     * @param type 需要消耗的token类型
     * @return 如果成功消耗则返回true，否则返回false
     */
    bool consume(TokenType type);

    /**
     * @brief 预览后续的token
     * @param type 需要预览的token类型
     * @param offset 向前预览的偏移量
     * @return 如果预览位置的token类型匹配则返回true，否则返回false
     */
    bool lookAhead(TokenType type, int offset = 1);

    /**
     * @brief 输出语法单元名称
     * @param name 语法单元名称
     */
    void printSyntaxUnit(const std::string& name);

    /**
     * @brief 输出Token的类型
     * @param type Token类型
     */
    void printAns(TokenType type);

    /**
     * @brief 解析编译单元（程序的最高层次结构）
     * CompUnit ::= [CompUnit] (Decl | FuncDef)
     */
    void parseCompUnit();
    
    /**
     * @brief 解析函数定义
     * FuncDef ::= FuncType Ident '(' [FuncFParams] ')' Block
     */
    void parseFuncDef();
    
    /**
     * @brief 解析函数类型
     * FuncType ::= 'void' | 'int'
     */
    void parseFuncType();
    
    /**
     * @brief 解析函数形式参数列表
     * FuncFParams ::= FuncFParam {',' FuncFParam}
     */
    void parseFuncFParams();
    
    /**
     * @brief 解析函数形式参数
     * FuncFParam ::= BType Ident ['[' ']' {'[' ConstExp ']'}]
     */
    void parseFuncFParam();

    /**
     * @brief 解析main函数定义
     * MainFuncDef ::= 'int' 'main' '(' ')' Block
     */
    void parseMainFuncDef();

    /**
     * @brief 解析代码块
     * Block ::= '{' {BlockItem} '}'
     */
    void parseBlock();

    /**
     * @brief 解析块中的项
     * BlockItem ::= Decl | Stmt
     */
    void parseBlockItem();

    /**
     * @brief 解析声明
     * Decl ::= ConstDecl | VarDecl
     */
    void parseDecl();

    /**
     * @brief 解析常量声明
     * ConstDecl ::= 'const' BType ConstDef {',' ConstDef} ';'
     */
    void parseConstDecl();

    /**
     * @brief 解析基本类型
     * BType ::= 'int'
     */
    void parseBType();

    /**
     * @brief 解析常量定义
     * ConstDef ::= Ident {'[' ConstExp ']'} '=' ConstInitVal
     */
    void parseConstDef();

    /**
     * @brief 解析常量初始值
     * ConstInitVal ::= ConstExp | '{' [ConstInitVal {',' ConstInitVal}] '}'
     */
    void parseConstInitVal();

    /**
     * @brief 解析变量声明
     * VarDecl ::= BType VarDef {',' VarDef} ';'
     */
    void parseVarDecl();

    /**
     * @brief 解析变量定义
     * VarDef ::= Ident {'[' ConstExp ']'} ['=' InitVal]
     */
    void parseVarDef();

    /**
     * @brief 解析变量初始值
     * InitVal ::= Exp | '{' [InitVal {',' InitVal}] '}'
     */
    void parseInitVal();

    /**
     * @brief 解析语句
     * Stmt ::= LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
     *       | 'while' '(' Cond ')' Stmt | 'break' ';' | 'continue' ';' 
     *       | 'return' [Exp] ';' | LVal '=' 'getint' '(' ')' ';' | 'printf' '(' FormatString {',' Exp} ')' ';'
     */
    void parseStmt();

    /**
     * @brief 解析基本表达式
     * PrimaryExp ::= '(' Exp ')' | LVal | Number
     */
    void parsePrimaryExp();

    /**
     * @brief 解析左值表达式
     * LVal ::= Ident {'[' Exp ']'}
     */
    void parseLVal();

    /**
     * @brief 解析数字
     * Number ::= IntConst
     */
    void parseNumber();

    /**
     * @brief 解析一元表达式
     * UnaryExp ::= PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
     */
    void parseUnaryExp();

    /**
     * @brief 解析一元运算符
     * UnaryOp ::= '+' | '-' | '!'
     */
    void parseUnaryOp();

    /**
     * @brief 解析函数实参列表
     * FuncRParams ::= Exp {',' Exp}
     */
    void parseFuncRParams();

    /**
     * @brief 解析乘法表达式
     * MulExp ::= UnaryExp {('*' | '/' | '%') UnaryExp}
     */
    void parseMulExp();

    /**
     * @brief 解析加法表达式
     * AddExp ::= MulExp {('+' | '-') MulExp}
     */
    void parseAddExp();

    /**
     * @brief 解析关系表达式
     * RelExp ::= AddExp {('<' | '>' | '<=' | '>=') AddExp}
     */
    void parseRelExp();

    /**
     * @brief 解析相等性表达式
     * EqExp ::= RelExp {('==' | '!=') RelExp}
     */
    void parseEqExp();

    /**
     * @brief 解析逻辑与表达式
     * LAndExp ::= EqExp {'&&' EqExp}
     */
    void parseLAndExp();

    /**
     * @brief 解析逻辑或表达式
     * LOrExp ::= LAndExp {'||' LAndExp}
     */
    void parseLOrExp();

    /**
     * @brief 解析条件表达式
     * Cond ::= LOrExp
     */
    void parseCond();

    /**
     * @brief 解析if语句
     * 'if' '(' Cond ')' Stmt ['else' Stmt]
     */
    void parseIfStmt();

    /**
     * @brief 解析while语句
     * 'while' '(' Cond ')' Stmt
     */
    void parseWhileStmt();

    /**
     * @brief 解析常量表达式
     * ConstExp ::= AddExp
     */
    void parseConstExp();

    /**
     * @brief 解析表达式
     * Exp ::= AddExp
     */
    void parseExp();

public:
    /**
     * @brief 构造语法分析器
     * @param tokens 词法分析产生的token序列
     * @param out 输出文件流
     */
    Parser(const std::vector<Token>& tokens, std::ofstream& out);

    /**
     * @brief 执行语法分析
     * 从CompUnit开始，完成整个程序的语法分析
     */
    void parse();
};

#endif 