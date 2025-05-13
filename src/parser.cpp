#include "parser.h"
#include <iostream>

// 构造函数实现
Parser::Parser(const std::vector<Token>& tokens, std::ofstream& out) 
    : tokens(tokens), outFile(out), currentPos(0) {
}

// 获取当前token
Token& Parser::getCurrentToken() {
    if (currentPos >= tokens.size()) {
        throw std::runtime_error("Token索引超出边界");
    }
    return tokens[currentPos];
}

// 检查当前token的类型
bool Parser::check(TokenType type) {
    return currentPos < tokens.size() && getCurrentToken().type == type;
}

// 消耗一个token并前进
void Parser::consume() {
    // 打印当前token信息
    const auto& token = getCurrentToken();
    printAns(token.type);
    outFile << " " << token.value << std::endl;
    
    currentPos++;
}

// 消耗指定类型的token
bool Parser::consume(TokenType type) {
    if (check(type)) {
        consume();
        return true;
    }
    return false;
}

// 预览下一个token
bool Parser::lookAhead(TokenType type, int offset) {
    return (currentPos + offset) < tokens.size() && tokens[currentPos + offset].type == type;
}

// 输出语法单元名称
void Parser::printSyntaxUnit(const std::string& name) {
    outFile << "<" << name << ">" << std::endl;
}

// 输出Token的类型
void Parser::printAns(TokenType type) {
    // 此处实现根据TokenType输出对应的字符串
    switch (type)
  {
  case TokenType::EQL:
    outFile << "EQL";
    break;
  case TokenType::ASSIGN:
    outFile << "ASSIGN";
    break;
  case TokenType::IDENFR:
    outFile << "IDENFR";
    break;
  case TokenType::INTCON:
    outFile << "INTCON";
    break;
  case TokenType::STRCON:
    outFile << "STRCON";
    break;
  case TokenType::MAINTK:
    outFile << "MAINTK";
    break;
  case TokenType::CONSTTK:
    outFile << "CONSTTK";
    break;
  case TokenType::IFTK:
    outFile << "IFTK";
    break;
  case TokenType::ELSETK:
    outFile << "ELSETK";
    break;
  case TokenType::CONTINUETK:
    outFile << "CONTINUETK";
    break;
  case TokenType::BREAKTK:
    outFile << "BREAKTK";
    break;
  case TokenType::INTTK:
    outFile << "INTTK";
    break;
  case TokenType::WHILETK:
    outFile << "WHILETK";
    break;
  case TokenType::RETURNTK:
    outFile << "RETURNTK";
    break;
  case TokenType::VOIDTK:
    outFile << "VOIDTK";
    break;
  case TokenType::GETINTTK:
    outFile << "GETINTTK";
    break;
  case TokenType::PRINTFTK:
    outFile << "PRINTFTK";
    break;
  case TokenType::NOT:
    outFile << "NOT";
    break;
  case TokenType::SEMICN:
    outFile << "SEMICN";
    break;
  case TokenType::COMMA:
    outFile << "COMMA";
    break;
  case TokenType::LBRACE:
    outFile << "LBRACE";
    break;
  case TokenType::RBRACE:
    outFile << "RBRACE";
    break;
  case TokenType::LBRACK:
    outFile << "LBRACK";
    break;
  case TokenType::RBRACK:
    outFile << "RBRACK";
    break;
  case TokenType::LPARENT:
    outFile << "LPARENT";
    break;
  case TokenType::RPARENT:
    outFile << "RPARENT";
    break;
  case TokenType::MINU:
    outFile << "MINU";
    break;
  case TokenType::PLUS:
    outFile << "PLUS";
    break;
  case TokenType::MULT:
    outFile << "MULT";
    break;
  case TokenType::DIV:
    outFile << "DIV";
    break;
  case TokenType::MOD:
    outFile << "MOD";
    break;
  case TokenType::AND:
    outFile << "AND";
    break;
  case TokenType::OR:
    outFile << "OR";
    break;
  case TokenType::NEQ:
    outFile << "NEQ";
    break;
  case TokenType::LSS:
    outFile << "LSS";
    break;
  case TokenType::LEQ:
    outFile << "LEQ";
    break;
  case TokenType::GEQ:
    outFile << "GEQ";
    break;
  case TokenType::GRE:
    outFile << "GRE";
    break;

  default:
    outFile << "UNKNOWN";
  }
}



// 执行语法分析
void Parser::parse() {
    parseCompUnit();
}

// 解析编译单元
void Parser::parseCompUnit() {
    // 解析声明和函数定义，直到遇到main函数
    while (currentPos < tokens.size())
    {
        // 检查可能的结束条件，避免死循环
        size_t startPos = currentPos;
        
        // 处理常量声明
        if (check(TokenType::CONSTTK)) {
            parseConstDecl();
        }
        // 处理变量声明或函数定义
        else if (check(TokenType::INTTK) || check(TokenType::VOIDTK)) {
            if (check(TokenType::VOIDTK)) {
                // void只能是函数定义
                parseFuncDef();
            }
            else if (currentPos + 1 < tokens.size()) {
                // 检查是否是main函数
                if (tokens[currentPos + 1].type == TokenType::MAINTK) {
                    break; // 遇到main函数，结束全局定义解析
                }
                // 检查是否是普通函数定义
                else if (currentPos + 2 < tokens.size() && 
                            tokens[currentPos + 1].type == TokenType::IDENFR && 
                            tokens[currentPos + 2].type == TokenType::LPARENT) {
                    parseFuncDef();
                }
                // 变量声明
                else {
                    parseVarDecl();
                }
            }
            else {
                // 无法确定，跳出循环避免死循环
                break;
            }
        }
        else {
            // 无法解析的内容，跳出循环
            break;
        }
        
        // 检查是否取得进展，防止死循环
        if (currentPos == startPos) {
            break;
        }
    }
    
    // 解析main函数
    if (currentPos < tokens.size() && check(TokenType::INTTK) && 
        currentPos + 1 < tokens.size() && tokens[currentPos + 1].type == TokenType::MAINTK) {
        parseMainFuncDef();
    }
    
    printSyntaxUnit("CompUnit");
}

// 解析函数定义
void Parser::parseFuncDef() {
    parseFuncType();
    consume(TokenType::IDENFR);
    consume(TokenType::LPARENT);
    
    // 可选的函数参数
    if (!check(TokenType::RPARENT)) {
        parseFuncFParams();
    }
    
    consume(TokenType::RPARENT);
    parseBlock();
    printSyntaxUnit("FuncDef");
}

// 解析函数类型
void Parser::parseFuncType() {
    if (check(TokenType::VOIDTK) || check(TokenType::INTTK)) {
        consume();
    }
    printSyntaxUnit("FuncType");
}

// 解析函数形式参数列表
void Parser::parseFuncFParams() {
    parseFuncFParam();
    
    while (check(TokenType::COMMA)) {
        consume();
        parseFuncFParam();
    }
    
    printSyntaxUnit("FuncFParams");
}

// 解析函数形式参数
void Parser::parseFuncFParam() {
    parseBType();
    consume(TokenType::IDENFR);
    
    // 处理数组参数
    if (check(TokenType::LBRACK)) {
        consume();
        consume(TokenType::RBRACK);
        
        // 处理多维数组
        while (check(TokenType::LBRACK)) {
            consume();
            parseConstExp();
            consume(TokenType::RBRACK);
        }
    }
    
    printSyntaxUnit("FuncFParam");
}

// 解析main函数定义
void Parser::parseMainFuncDef() {
    // 分析 'int' 'main' '(' ')'
    consume(TokenType::INTTK);
    consume(TokenType::MAINTK);
    consume(TokenType::LPARENT);
    consume(TokenType::RPARENT);
    parseBlock();
    printSyntaxUnit("MainFuncDef");
}

// 解析代码块
void Parser::parseBlock() {
    consume(TokenType::LBRACE);
    
    // 解析块中的所有项
    while (currentPos < tokens.size() && !check(TokenType::RBRACE)) {
        parseBlockItem();
    }
    
    consume(TokenType::RBRACE);
    printSyntaxUnit("Block");
}

// 解析块中的项
void Parser::parseBlockItem() {
    if (check(TokenType::CONSTTK) || (check(TokenType::INTTK) && !lookAhead(TokenType::MAINTK))) {
        parseDecl();
    } else {
        parseStmt();
    }
    // 这里不输出 BlockItem
}

// 解析声明
void Parser::parseDecl() {
    if (check(TokenType::CONSTTK)) {
        parseConstDecl();
    } else {
        parseVarDecl();
    }
    // 不输出 Decl
}

// 解析常量声明
void Parser::parseConstDecl() {
    consume(TokenType::CONSTTK);
    parseBType();
    parseConstDef();
    
    while (check(TokenType::COMMA)) {
        consume();
        parseConstDef();
    }
    
    consume(TokenType::SEMICN);
    printSyntaxUnit("ConstDecl");
}

// 解析基本类型
void Parser::parseBType() {
    consume(TokenType::INTTK);
    // 不输出 BType
}

// 解析常量定义
void Parser::parseConstDef() {
    consume(TokenType::IDENFR);
    
    while (check(TokenType::LBRACK)) {
        consume();
        parseConstExp();
        consume(TokenType::RBRACK);
    }
    
    consume(TokenType::ASSIGN);
    parseConstInitVal();
    printSyntaxUnit("ConstDef");
}

// 解析常量初始值
void Parser::parseConstInitVal() {
    if (check(TokenType::LBRACE)) {
        consume();
        
        if (!check(TokenType::RBRACE)) {
            parseConstInitVal();
            
            while (check(TokenType::COMMA)) {
                consume();
                parseConstInitVal();
            }
        }
        
        consume(TokenType::RBRACE);
    } else {
        parseConstExp();
    }
    
    printSyntaxUnit("ConstInitVal");
}

// 解析变量声明
void Parser::parseVarDecl() {
    parseBType();
    parseVarDef();
    
    while (check(TokenType::COMMA)) {
        consume();
        parseVarDef();
    }
    
    consume(TokenType::SEMICN);
    printSyntaxUnit("VarDecl");
}

// 解析变量定义
void Parser::parseVarDef() {
    consume(TokenType::IDENFR);
    
    while (check(TokenType::LBRACK)) {
        consume();
        parseConstExp();
        consume(TokenType::RBRACK);
    }
    
    if (check(TokenType::ASSIGN)) {
        consume();
        parseInitVal();
    }
    
    printSyntaxUnit("VarDef");
}

// 解析变量初始值
void Parser::parseInitVal() {
    if (check(TokenType::LBRACE)) {
        consume();
        
        if (!check(TokenType::RBRACE)) {
            parseInitVal();
            
            while (check(TokenType::COMMA)) {
                consume();
                parseInitVal();
            }
        }
        
        consume(TokenType::RBRACE);
    } else {
        parseExp();
    }
    
    printSyntaxUnit("InitVal");
}

// 解析语句
void Parser::parseStmt() {
    // 记录当前位置，避免回溯导致的问题
    size_t startPos = currentPos;
    
    if (check(TokenType::IFTK)) {
        parseIfStmt();
    } else if (check(TokenType::WHILETK)) {
        parseWhileStmt();
    } else if (check(TokenType::BREAKTK)) {
        consume();
        consume(TokenType::SEMICN);
        printSyntaxUnit("Stmt");
    } else if (check(TokenType::CONTINUETK)) {
        consume();
        consume(TokenType::SEMICN);
        printSyntaxUnit("Stmt");
    } else if (check(TokenType::RETURNTK)) {
        consume();
        if (!check(TokenType::SEMICN)) {
            parseExp();
        }
        consume(TokenType::SEMICN);
        printSyntaxUnit("Stmt");
    } else if (check(TokenType::PRINTFTK)) {
        consume();
        consume(TokenType::LPARENT);
        consume(TokenType::STRCON);
        
        while (check(TokenType::COMMA)) {
            consume();
            parseExp();
        }
        
        consume(TokenType::RPARENT);
        consume(TokenType::SEMICN);
        printSyntaxUnit("Stmt");
    } else if (check(TokenType::LBRACE)) {
        parseBlock();
        printSyntaxUnit("Stmt");
    } else if (check(TokenType::SEMICN)) {
        consume();
        printSyntaxUnit("Stmt");
    } else {
        // 尝试解析赋值语句或表达式语句
        
        // 检查是否可能是赋值语句
        if (check(TokenType::IDENFR)) {
            // 尝试解析左值
            size_t savePos = currentPos;
            bool isAssignment = false;
            
            // 先解析标识符
            consume();
            
            // 处理数组下标
            while (check(TokenType::LBRACK)) {
                consume();
                parseExp();
                consume(TokenType::RBRACK);
            }
            
            // 如果下一个是'='，则是赋值语句
            if (check(TokenType::ASSIGN)) {
                // 输出LVal
                printSyntaxUnit("LVal");
                isAssignment = true;
                
                consume(TokenType::ASSIGN);
                
                // getint() 调用
                if (check(TokenType::GETINTTK)) {
                    consume();
                    consume(TokenType::LPARENT);
                    consume(TokenType::RPARENT);
                } else {
                    parseExp();
                }
                
                consume(TokenType::SEMICN);
                printSyntaxUnit("Stmt");
                return;
            } else {
                // 不是赋值语句，回溯到标识符处重新解析
                currentPos = savePos;
            }
        }
        
        // 处理表达式语句
        if (!check(TokenType::SEMICN)) {
            parseExp();
        }
        consume(TokenType::SEMICN);
        printSyntaxUnit("Stmt");
    }
    
    // 检查以确保进展
    if (currentPos == startPos) {
        // 强制前进以避免死循环
        if (currentPos < tokens.size()) {
            currentPos++;
        }
    }
}

// 解析基本表达式
void Parser::parsePrimaryExp() {
    // 保存起始位置以防死循环
    size_t startPos = currentPos;
    
    if (check(TokenType::LPARENT)) {
        consume();
        parseExp();
        consume(TokenType::RPARENT);
    } else if (check(TokenType::IDENFR)) {
        parseLVal();
    } else if (check(TokenType::INTCON)) {
        parseNumber();
    } else {
        // 错误处理：尝试恢复
        if (currentPos < tokens.size()) {
            consume(); // 跳过当前token
        }
    }
    
    printSyntaxUnit("PrimaryExp");
    
    // 检查是否有进展
    if (currentPos == startPos && currentPos < tokens.size()) {
        currentPos++; // 强制前进
    }
}

// 解析左值表达式
void Parser::parseLVal() {
    if (!check(TokenType::IDENFR)) {
        throw std::runtime_error("左值表达式中需要标识符");
    }
    
    consume();
    
    while (check(TokenType::LBRACK)) {
        consume();
        parseExp();
        if (!check(TokenType::RBRACK)) {
            throw std::runtime_error("数组访问中缺少右方括号");
        }
        consume();
    }
    
    printSyntaxUnit("LVal");
}

// 解析数字
void Parser::parseNumber() {
    consume(TokenType::INTCON);
    printSyntaxUnit("Number");
}

// 解析一元表达式
void Parser::parseUnaryExp() {
    if (check(TokenType::IDENFR) && currentPos + 1 < tokens.size() && tokens[currentPos + 1].type == TokenType::LPARENT) {
        // 函数调用
        consume(TokenType::IDENFR);
        consume(TokenType::LPARENT);
        
        // A处理可能的函数参数
        if (!check(TokenType::RPARENT)) {
            parseFuncRParams();
        }
        
        consume(TokenType::RPARENT);
    } else if (check(TokenType::PLUS) || check(TokenType::MINU) || check(TokenType::NOT)) {
        parseUnaryOp();
        parseUnaryExp();
    } else {
        parsePrimaryExp();
    }
    
    printSyntaxUnit("UnaryExp");
}

// 解析一元运算符
void Parser::parseUnaryOp() {
    if (check(TokenType::PLUS) || check(TokenType::MINU) || check(TokenType::NOT)) {
        consume();
    }
    
    printSyntaxUnit("UnaryOp");
}

// 解析函数实参列表
void Parser::parseFuncRParams() {
    parseExp();
    
    while (check(TokenType::COMMA)) {
        consume();
        parseExp();
    }
    
    printSyntaxUnit("FuncRParams");
}

// 解析乘法表达式
void Parser::parseMulExp() {
    parseUnaryExp();
    printSyntaxUnit("MulExp");  // 在解析操作符前输出MulExp
    
    while (currentPos < tokens.size() && (check(TokenType::MULT) || check(TokenType::DIV) || check(TokenType::MOD))) {
        consume();
        parseUnaryExp();
        printSyntaxUnit("MulExp");  // 每次解析完一个运算符和操作数后输出MulExp
    }
}

// 解析加法表达式
void Parser::parseAddExp() {
    parseMulExp();
    printSyntaxUnit("AddExp");  // 在解析操作符前输出AddExp
    
    while (currentPos < tokens.size() && (check(TokenType::PLUS) || check(TokenType::MINU))) {
        consume();
        parseMulExp();
        printSyntaxUnit("AddExp");  // 每次解析完一个运算符和操作数后输出AddExp
    }
}

// 解析关系表达式
void Parser::parseRelExp() {
    parseAddExp();
    printSyntaxUnit("RelExp");  // 在解析操作符前输出RelExp
    
    while (currentPos < tokens.size() && 
          (check(TokenType::LSS) || check(TokenType::GRE) || 
           check(TokenType::LEQ) || check(TokenType::GEQ))) {
        consume();
        parseAddExp();
        printSyntaxUnit("RelExp");  // 每次解析完一个运算符和操作数后输出RelExp
    }
}

// 解析相等性表达式
void Parser::parseEqExp() {
    parseRelExp();
    printSyntaxUnit("EqExp");  // 在解析操作符前输出EqExp
    
    while (currentPos < tokens.size() && (check(TokenType::EQL) || check(TokenType::NEQ))) {
        consume();
        parseRelExp();
        printSyntaxUnit("EqExp");  // 每次解析完一个运算符和操作数后输出EqExp
    }
}

// 解析逻辑与表达式
void Parser::parseLAndExp() {
    parseEqExp();
    printSyntaxUnit("LAndExp");  // 在解析操作符前输出LAndExp
    
    while (currentPos < tokens.size() && check(TokenType::AND)) {
        consume();
        parseEqExp();
        printSyntaxUnit("LAndExp");  // 每次解析完一个运算符和操作数后输出LAndExp
    }
}

// 解析逻辑或表达式
void Parser::parseLOrExp() {
    parseLAndExp();
    printSyntaxUnit("LOrExp");  // 在解析操作符前输出LOrExp
    
    while (currentPos < tokens.size() && check(TokenType::OR)) {
        consume();
        parseLAndExp();
        printSyntaxUnit("LOrExp");  // 每次解析完一个运算符和操作数后输出LOrExp
    }
}

// 解析条件表达式
void Parser::parseCond() {
    parseLOrExp();
    printSyntaxUnit("Cond");
}

// 解析if语句
void Parser::parseIfStmt() {
    consume(TokenType::IFTK);
    
    if (!check(TokenType::LPARENT)) {
        throw std::runtime_error("'if'后需要'('");
    }
    consume(TokenType::LPARENT);
    
    parseCond();
    
    if (!check(TokenType::RPARENT)) {
        throw std::runtime_error("条件后需要')'");
    }
    consume(TokenType::RPARENT);
    
    parseStmt();
    
    if (check(TokenType::ELSETK)) {
        consume();
        parseStmt();
    }
    
    printSyntaxUnit("Stmt");
}

// 解析while语句
void Parser::parseWhileStmt() {
    consume(TokenType::WHILETK);
    
    if (!check(TokenType::LPARENT)) {
        throw std::runtime_error("'while'后需要'('");
    }
    consume(TokenType::LPARENT);
    
    parseCond();
    
    if (!check(TokenType::RPARENT)) {
        throw std::runtime_error("条件后需要')'");
    }
    consume(TokenType::RPARENT);
    
    parseStmt();
    printSyntaxUnit("Stmt");
}

// 解析常量表达式
void Parser::parseConstExp() {
    parseAddExp();
    printSyntaxUnit("ConstExp");
}

// 解析表达式
void Parser::parseExp() {
    parseAddExp();
    printSyntaxUnit("Exp");
}
