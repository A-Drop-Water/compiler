#include "parser.h"
#include "ast.h"
#include <iostream> // For std::cerr (though errors are thrown)

// 构造函数实现
Parser::Parser(const std::vector<Token>& tokens, std::ofstream& out) 
    : tokens(tokens), outFile(out), currentPos(0) {
    if (tokens.empty()) {
        // Handle empty token list if necessary, perhaps by throwing
        // or ensuring parse() returns nullptr or an empty CompUnitNode.
        // For now, assume it won't be called with empty tokens or parse() handles it.
    }
}

// 获取当前token (const version)
const Token& Parser::getCurrentToken() const {
    if (currentPos >= tokens.size()) {
        // This indicates an unexpected end of input, usually a bug in parser logic
        // or an incomplete source file that wasn't caught by an EOF token.
        // A special EOF token at the end of the token list can help manage this.
        // For now, throw. A more graceful recovery might be needed in a full compiler.
        throw std::runtime_error("Unexpected end of token stream. Current position: " + 
                                 std::to_string(currentPos) + ", Total tokens: " + std::to_string(tokens.size()));
    }
    return tokens[currentPos];
}

size_t Parser::getCurrentLine() const {
    if (currentPos < tokens.size()) {
        return tokens[currentPos].line;
    }
    if (!tokens.empty()) {
        return tokens.back().line; // Best guess: line of the last token
    }
    return 0; // No tokens
}


// 检查当前token的类型
bool Parser::check(TokenType type) const {
    return currentPos < tokens.size() && tokens[currentPos].type == type;
}

// 消耗一个token并前进
Token Parser::consume() {
    if (currentPos >= tokens.size()) {
         throw std::runtime_error("Consume called beyond token stream.");
    }
    // No longer prints token here, AST nodes will carry the info.
    return tokens[currentPos++];
}

// 消耗指定类型的token，不匹配则抛出异常
Token Parser::consume(TokenType type, const std::string& errorMessage) {
    if (!check(type)) {
        throw std::runtime_error(errorMessage + " (Line: " + std::to_string(getCurrentLine()) + 
                                 "). Got " + tokens[currentPos].value + " instead.");
    }
    return consume();
}

// 预览下一个token
bool Parser::lookAhead(TokenType type, int offset) const {
    return (currentPos + offset) < tokens.size() && tokens[currentPos + offset].type == type;
}

// --- Main Parse Method ---
std::unique_ptr<CompUnitNode> Parser::parse() {
    auto compUnit = parseCompUnit();
    if (currentPos < tokens.size()) {
        // This means not all tokens were consumed, which could be a syntax error
        // or an issue with the grammar / parser logic for the top level.
        // std::cerr << "Warning: Not all tokens consumed. Remaining token: "
        //           << tokens[currentPos].value << " at line " << tokens[currentPos].line << std::endl;
        // Depending on strictness, this could be an error.
        // For now, we assume parseCompUnit should consume everything if the input is valid.
         throw std::runtime_error("Extra tokens found after parsing CompUnit, starting with '" +
                                 tokens[currentPos].value + "' at line " + std::to_string(tokens[currentPos].line));
    }
    return compUnit;
}

// --- AST Node Parsing Methods Implementation ---

// CompUnit ::= {Decl | FuncDef} MainFuncDef
std::unique_ptr<CompUnitNode> Parser::parseCompUnit() {
    auto compUnitNode = std::make_unique<CompUnitNode>(getCurrentLine());

    while (currentPos < tokens.size()) {
        if (check(TokenType::CONSTTK)) { // Start of ConstDecl
            compUnitNode->globalDefinitions.push_back(parseConstDecl());
        } else if (check(TokenType::INTTK) || check(TokenType::VOIDTK)) {
            // Need to distinguish VarDecl, FuncDef, MainFuncDef
            if (check(TokenType::INTTK) && lookAhead(TokenType::MAINTK)) {
                break; // Found main, stop parsing global decls/defs
            }
            // Check for FuncDef: FuncType Ident '('
            // VarDecl: BType Ident (';' or '[' or '=')
            // A common way to distinguish is to look further, e.g., for '('.
            // If BType Ident '(', it's a FuncDef. Otherwise, VarDecl.
            // (Assuming BType is 'int' for VarDecl)
            if (lookAhead(TokenType::IDENFR, 1) && lookAhead(TokenType::LPARENT, 2)) {
                 compUnitNode->globalDefinitions.push_back(parseFuncDef());
            } else if (check(TokenType::INTTK)) { // Must be VarDecl if not FuncDef or Main
                 compUnitNode->globalDefinitions.push_back(parseVarDecl());
            } else if (check(TokenType::VOIDTK)) { // Must be FuncDef
                 compUnitNode->globalDefinitions.push_back(parseFuncDef());
            }
             else {
                throw std::runtime_error("Expected declaration, function definition, or main function at line " + std::to_string(getCurrentLine()));
            }
        } else {
            break; // No more global declarations or function definitions
        }
    }

    compUnitNode->mainFunc = parseMainFuncDef();
    return compUnitNode;
}

// FuncDef ::= FuncType Ident '(' [FuncFParams] ')' Block
std::unique_ptr<FuncDefNode> Parser::parseFuncDef() {
    auto funcType = parseFuncType();
    Token funcNameToken = consume(TokenType::IDENFR, "Expected function name");
    consume(TokenType::LPARENT, "Expected '(' after function name");

    std::unique_ptr<FuncFParamsNode> paramsNode = nullptr;
    if (!check(TokenType::RPARENT)) {
        paramsNode = parseFuncFParams();
    }
    consume(TokenType::RPARENT, "Expected ')' after function parameters");
    
    auto body = parseBlock();
    
    return std::make_unique<FuncDefNode>(std::move(funcType), funcNameToken, std::move(paramsNode), std::move(body));
}

// FuncType ::= 'void' | 'int'
std::unique_ptr<FuncTypeNode> Parser::parseFuncType() {
    Token typeToken = consume(); // Consumes 'void' or 'int'
    if (typeToken.type != TokenType::VOIDTK && typeToken.type != TokenType::INTTK) {
        throw std::runtime_error("Expected 'void' or 'int' for function type at line " + std::to_string(typeToken.line));
    }
    return std::make_unique<FuncTypeNode>(typeToken);
}

// FuncFParams ::= FuncFParam {',' FuncFParam}
std::unique_ptr<FuncFParamsNode> Parser::parseFuncFParams() {
    auto paramsNode = std::make_unique<FuncFParamsNode>(getCurrentLine());
    paramsNode->params.push_back(parseFuncFParam());
    while (check(TokenType::COMMA)) {
        consume(); // ','
        paramsNode->params.push_back(parseFuncFParam());
    }
    return paramsNode;
}

// FuncFParam ::= BType Ident ['[' ']' {'[' ConstExp ']'}]
// PDF says: "只包含普通变量" (Only plain variables)
// My AST node for FuncFParam is simplified based on PDF.
// If full array params are needed, this parser and AST node need changes.
std::unique_ptr<FuncFParamNode> Parser::parseFuncFParam() {
    auto bType = parseBType();
    Token nameToken = consume(TokenType::IDENFR, "Expected parameter name");
    
    auto paramNode = std::make_unique<FuncFParamNode>(std::move(bType), nameToken);

    // Handling array parameters as per the EBNF in parser.h (more complex than PDF's note)
    // The PDF note "只包含普通变量" simplifies this. If we follow the EBNF strictly:
    if (check(TokenType::LBRACK)) {
        paramNode->isArray = true;
        consume(TokenType::LBRACK, "Expected '[' for array parameter");
        consume(TokenType::RBRACK, "Expected ']' for array parameter"); // First '[]' is empty
        
        while(check(TokenType::LBRACK)) {
            consume(TokenType::LBRACK, "Expected '[' for array parameter dimension");
            paramNode->arrayPointerDimensions.push_back(parseConstExp());
            consume(TokenType::RBRACK, "Expected ']' after array parameter dimension");
        }
    }
    return paramNode;
}


// MainFuncDef ::= 'int' 'main' '(' ')' Block
std::unique_ptr<MainFuncDefNode> Parser::parseMainFuncDef() {
    size_t line = getCurrentLine();
    consume(TokenType::INTTK, "Expected 'int' for main function");
    consume(TokenType::MAINTK, "Expected 'main' keyword");
    consume(TokenType::LPARENT, "Expected '(' after 'main'");
    consume(TokenType::RPARENT, "Expected ')' after 'main()'");
    auto body = parseBlock();
    return std::make_unique<MainFuncDefNode>(std::move(body), line);
}

// Block ::= '{' {BlockItem} '}'
std::unique_ptr<BlockNode> Parser::parseBlock() {
    Token lbrace = consume(TokenType::LBRACE, "Expected '{' to start a block");
    auto blockNode = std::make_unique<BlockNode>(lbrace.line);
    while (!check(TokenType::RBRACE) && currentPos < tokens.size()) {
        blockNode->items.push_back(parseBlockItem());
    }
    consume(TokenType::RBRACE, "Expected '}' to end a block");
    return blockNode;
}

// BlockItem ::= Decl | Stmt
std::unique_ptr<BlockItemNode> Parser::parseBlockItem() {
    // Decl starts with 'const' or 'int' (BType)
    if (check(TokenType::CONSTTK) || check(TokenType::INTTK)) {
        // Need to ensure 'int' is not part of 'int main()' if parsing was less strict before.
        // Here, we are inside a block, so 'int' should be a VarDecl.
        return parseDecl();
    } else {
        // Statements need to be wrapped as BlockItemNode
        auto stmt = parseStmt();
        // 创建一个包装StmtNode的BlockItemNode
        class StmtBlockItemNode : public BlockItemNode {
        private:
            std::unique_ptr<StmtNode> stmt;
        public:
            StmtBlockItemNode(std::unique_ptr<StmtNode> s) : BlockItemNode(s->lineNumber), stmt(std::move(s)) {}
            void print(std::ostream& out, int indent) const override {
                stmt->print(out, indent);
            }
        };
        return std::make_unique<StmtBlockItemNode>(std::move(stmt));
    }
}

// Decl ::= ConstDecl | VarDecl
std::unique_ptr<DeclNode> Parser::parseDecl() {
    if (check(TokenType::CONSTTK)) {
        return parseConstDecl();
    } else if (check(TokenType::INTTK)) {
        return parseVarDecl();
    } else {
        throw std::runtime_error("Expected 'const' or 'int' for declaration at line " + std::to_string(getCurrentLine()));
    }
}

// ConstDecl ::= 'const' BType ConstDef {',' ConstDef} ';'
std::unique_ptr<ConstDeclNode> Parser::parseConstDecl() {
    Token constToken = consume(TokenType::CONSTTK, "Expected 'const' keyword");
    auto bType = parseBType();
    auto constDeclNode = std::make_unique<ConstDeclNode>(std::move(bType), constToken.line);
    
    constDeclNode->constDefs.push_back(parseConstDef());
    while (check(TokenType::COMMA)) {
        consume(); // ','
        constDeclNode->constDefs.push_back(parseConstDef());
    }
    consume(TokenType::SEMICN, "Expected ';' after const declaration");
    return constDeclNode;
}

// BType ::= 'int'
std::unique_ptr<BTypeNode> Parser::parseBType() {
    Token typeToken = consume(TokenType::INTTK, "Expected 'int' type specifier");
    return std::make_unique<BTypeNode>(typeToken.line);
}

// ConstDef ::= Ident ['[' ConstExp ']'] '=' ConstInitVal
std::unique_ptr<ConstDefNode> Parser::parseConstDef() {
    Token idToken = consume(TokenType::IDENFR, "Expected identifier in const definition");
    auto constDefNode = std::make_unique<ConstDefNode>(idToken);

    while (check(TokenType::LBRACK)) {
        consume(); // '['
        constDefNode->arrayDimensions.push_back(parseConstExp());
        consume(TokenType::RBRACK, "Expected ']' after array dimension in const definition");
    }
    consume(TokenType::ASSIGN, "Expected '=' in const definition");
    constDefNode->initVal = parseConstInitVal();
    return constDefNode;
}

// ConstInitVal ::= ConstExp | '{' [ConstInitVal {',' ConstInitVal}] '}'
std::unique_ptr<ConstInitValNode> Parser::parseConstInitVal() {
    size_t line = getCurrentLine();
    if (check(TokenType::LBRACE)) {
        consume(); // '{'
        std::vector<std::unique_ptr<ConstInitValNode>> aggValues;
        if (!check(TokenType::RBRACE)) {
            aggValues.push_back(parseConstInitVal());
            while (check(TokenType::COMMA)) {
                consume(); // ','
                aggValues.push_back(parseConstInitVal());
            }
        }
        consume(TokenType::RBRACE, "Expected '}' for aggregate const initializer");
        return std::make_unique<ConstInitValNode>(std::move(aggValues), line);
    } else {
        return std::make_unique<ConstInitValNode>(parseConstExp(), line);
    }
}

// VarDecl ::= BType VarDef {',' VarDef} ';'
std::unique_ptr<VarDeclNode> Parser::parseVarDecl() {
    auto bType = parseBType();
    size_t line = bType->lineNumber; // Line of 'int'
    auto varDeclNode = std::make_unique<VarDeclNode>(std::move(bType), line);

    varDeclNode->varDefs.push_back(parseVarDef());
    while (check(TokenType::COMMA)) {
        consume(); // ','
        varDeclNode->varDefs.push_back(parseVarDef());
    }
    consume(TokenType::SEMICN, "Expected ';' after variable declaration");
    return varDeclNode;
}

// VarDef ::= Ident ['[' ConstExp ']'] ['=' InitVal]
std::unique_ptr<VarDefNode> Parser::parseVarDef() {
    Token idToken = consume(TokenType::IDENFR, "Expected identifier in variable definition");
    auto varDefNode = std::make_unique<VarDefNode>(idToken);

    while (check(TokenType::LBRACK)) {
        consume(); // '['
        varDefNode->arrayDimensions.push_back(parseConstExp());
        consume(TokenType::RBRACK, "Expected ']' after array dimension");
    }

    if (check(TokenType::ASSIGN)) {
        consume(); // '='
        varDefNode->initVal = parseInitVal();
    }
    return varDefNode;
}

// InitVal ::= Exp | '{' [InitVal {',' InitVal}] '}'
std::unique_ptr<InitValNode> Parser::parseInitVal() {
    size_t line = getCurrentLine();
    if (check(TokenType::LBRACE)) {
        consume(); // '{'
        std::vector<std::unique_ptr<InitValNode>> aggValues;
        if (!check(TokenType::RBRACE)) {
            aggValues.push_back(parseInitVal());
            while (check(TokenType::COMMA)) {
                consume(); // ','
                aggValues.push_back(parseInitVal());
            }
        }
        consume(TokenType::RBRACE, "Expected '}' for aggregate initializer");
        if (aggValues.empty()) return std::make_unique<InitValNode>(line); // Empty {}
        return std::make_unique<InitValNode>(std::move(aggValues), line);
    } else {
        return std::make_unique<InitValNode>(parseExp(), line);
    }
}

// Stmt ::= LVal '=' Exp ';' | LVal '=' 'getint''('')'';'
//        | [Exp] ';' | Block
//        | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
//        | 'while' '(' Cond ')' Stmt
//        | 'break' ';' | 'continue' ';'
//        | 'return' [Exp] ';'
//        | 'printf' '(' FormatString {',' Exp} ')' ';'
std::unique_ptr<StmtNode> Parser::parseStmt() {
    size_t line = getCurrentLine();
    if (check(TokenType::IFTK)) {
        return parseIfStmt();
    } else if (check(TokenType::WHILETK)) {
        return parseWhileStmt();
    } else if (check(TokenType::BREAKTK)) {
        Token breakToken = consume(); // 'break'
        consume(TokenType::SEMICN, "Expected ';' after 'break'");
        return std::make_unique<BreakStmtNode>(breakToken.line);
    } else if (check(TokenType::CONTINUETK)) {
        Token continueToken = consume(); // 'continue'
        consume(TokenType::SEMICN, "Expected ';' after 'continue'");
        return std::make_unique<ContinueStmtNode>(continueToken.line);
    } else if (check(TokenType::RETURNTK)) {
        Token returnToken = consume(); // 'return'
        std::unique_ptr<ExpNode> returnValue = nullptr;
        if (!check(TokenType::SEMICN)) {
            returnValue = parseExp();
        }
        consume(TokenType::SEMICN, "Expected ';' after return statement");
        return std::make_unique<ReturnStmtNode>(std::move(returnValue), returnToken.line);
    } else if (check(TokenType::PRINTFTK)) {
        Token printfToken = consume(); // 'printf'
        consume(TokenType::LPARENT, "Expected '(' after 'printf'");
        auto formatStr = parseFormatString();
        auto printfNode = std::make_unique<PrintfStmtNode>(std::move(formatStr), printfToken.line);
        while (check(TokenType::COMMA)) {
            consume(); // ','
            printfNode->args.push_back(parseExp());
        }
        consume(TokenType::RPARENT, "Expected ')' after printf arguments");
        consume(TokenType::SEMICN, "Expected ';' after printf statement");
        return printfNode;
    } else if (check(TokenType::LBRACE)) { // Block
        return parseBlock();
    } else if (check(TokenType::SEMICN)) { // Empty statement [Exp]; where Exp is null
        Token semiToken = consume(); // ';'
        return std::make_unique<ExpStmtNode>(nullptr, semiToken.line);
    } else {
        // This can be LVal = ... or Exp;
        // Try parsing as LVal first to see if it's an assignment.
        // This requires a way to "peek" or "try parse".
        // A common approach for LL parsers is to parse the common prefix (LVal or start of Exp)
        // and then decide.
        // If current token is IDENFR, it could be start of LVal or start of Exp (e.g. function call).
        // Let's parse an expression. If it's an LVal and followed by '=', it's assignment.
        // This is tricky. The grammar `LVal '=' Exp` vs `[Exp];`
        // If we parse an LVal, and next is '=', it's assignment.
        // If we parse an LVal, and next is ';', it's an ExpStmt with LVal as Exp.
        // If we parse a Number, and next is ';', it's an ExpStmt.

        // Simplified: Check if it's an assignment by looking for LVal followed by '='
        // This is a classic LL parsing dilemma. One way is to parse LVal, then check next token.
        // If it's '=', then it's an assignment. Otherwise, that LVal is part of an Exp.
        // This might require backtracking or a more sophisticated lookahead.

        // For your grammar, LVal is also a PrimaryExp, so an LVal can be an Exp.
        // Let's try to parse LVal. If successful and followed by '=', it's assignment.
        // Otherwise, it's an Exp.
        
        // Heuristic: if IDENFR is followed by ( LBRACK or ASSIGN ), it's likely LVal for assignment.
        // This is still not robust enough.
        // The standard way is to parse the longest possible LVal.
        // Then check if the next token is ASSIGN.
        
        // Let's assume `parseExp()` can handle LVal as a part of it.
        // If the expression statement is `lval;`, `parseExp` will return an `LValNode`.
        // If it's `lval = exp;`, we need to distinguish.

        // Try parsing LVal:
        if (check(TokenType::IDENFR)) { // Potential LVal start
            // Peek ahead for '=' without consuming the LVal structure yet.
            // This is where an AST helps: parse the LVal, then decide.
            // Let's try to parse LVal and see if next is '='
            size_t preLValPos = currentPos;
            try {
                auto lval = parseLVal(); // Tentatively parse LVal
                if (check(TokenType::ASSIGN)) { // It's an assignment
                    consume(TokenType::ASSIGN, "Expected '=' for assignment"); // '='
                    if (check(TokenType::GETINTTK)) {
                        Token getintToken = consume(); // 'getint'
                        consume(TokenType::LPARENT, "Expected '(' after 'getint'");
                        consume(TokenType::RPARENT, "Expected ')' after 'getint()'");
                        consume(TokenType::SEMICN, "Expected ';' after getint statement");
                        return std::make_unique<AssignStmtNode>(std::move(lval), getintToken.line);
                    } else {
                        auto rhs = parseExp();
                        consume(TokenType::SEMICN, "Expected ';' after assignment statement");
                        return std::make_unique<AssignStmtNode>(std::move(lval), std::move(rhs), lval->lineNumber);
                    }
                } else { // Not an assignment, LVal is part of an ExpStmt
                    currentPos = preLValPos; // Backtrack
                }
            } catch (const std::runtime_error& e) { // Parsing LVal failed, or something else
                currentPos = preLValPos; // Backtrack
                // It might not have been an LVal, or LVal parsing itself failed.
                // Proceed to parse as general Exp.
            }
        }

        // If not an assignment identified above, or if it doesn't start with IDENFR for LVal check
        auto exp = parseExp();
        consume(TokenType::SEMICN, "Expected ';' after expression statement");
        return std::make_unique<ExpStmtNode>(std::move(exp), exp ? exp->lineNumber : line);
    }
}


// PrimaryExp ::= '(' Exp ')' | LVal | Number
std::unique_ptr<ExpNode> Parser::parsePrimaryExp() {
    if (check(TokenType::LPARENT)) {
        consume(); // '('
        auto exp = parseExp();
        consume(TokenType::RPARENT, "Expected ')' after expression in parentheses");
        return exp;
    } else if (check(TokenType::IDENFR)) { // Could be LVal or start of FuncCall (handled in UnaryExp)
        return parseLVal(); // LVal is a type of ExpNode
    } else if (check(TokenType::INTCON)) {
        return parseNumber();
    } else {
        throw std::runtime_error("Expected '(', identifier, or number for primary expression at line " + std::to_string(getCurrentLine()));
    }
}

// LVal ::= Ident {'[' Exp ']'}
std::unique_ptr<LValNode> Parser::parseLVal() {
    Token idToken = consume(TokenType::IDENFR, "Expected identifier for LValue");
    auto lvalNode = std::make_unique<LValNode>(idToken);
    while (check(TokenType::LBRACK)) {
        consume(); // '['
        lvalNode->arrayIndices.push_back(parseExp());
        consume(TokenType::RBRACK, "Expected ']' after array index");
    }
    return lvalNode;
}

// Number ::= IntConst
std::unique_ptr<NumberNode> Parser::parseNumber() {
    Token numToken = consume(TokenType::INTCON, "Expected integer constant");
    return std::make_unique<NumberNode>(numToken);
}

// UnaryExp ::= PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
std::unique_ptr<ExpNode> Parser::parseUnaryExp() {
    // Check for UnaryOp UnaryExp first
    if (check(TokenType::PLUS) || check(TokenType::MINU) || check(TokenType::NOT)) {
        return parseUnaryOpAndExp();
    }
    // Check for Ident '(' ... ')' -- Function Call
    else if (check(TokenType::IDENFR) && lookAhead(TokenType::LPARENT)) {
        Token funcIdToken = consume(TokenType::IDENFR, "Expected function identifier for call");
        auto funcCallNode = std::make_unique<FuncCallNode>(funcIdToken);
        consume(TokenType::LPARENT, "Expected '(' for function call");
        if (!check(TokenType::RPARENT)) {
            funcCallNode->args = parseFuncRParams();
        }
        consume(TokenType::RPARENT, "Expected ')' after function call arguments");
        return funcCallNode;
    }
    // Otherwise, it's a PrimaryExp
    else {
        return parsePrimaryExp();
    }
}

// Helper for UnaryOp UnaryExp part of UnaryExp rule
std::unique_ptr<UnaryExpNode> Parser::parseUnaryOpAndExp() {
    Token opToken = consume(); // Consumes '+', '-', or '!'
    if (opToken.type != TokenType::PLUS && opToken.type != TokenType::MINU && opToken.type != TokenType::NOT) {
        throw std::runtime_error("Expected unary operator (+, -, !) at line " + std::to_string(opToken.line));
    }
    auto operand = parseUnaryExp(); // Recursive call for the UnaryExp part
    return std::make_unique<UnaryExpNode>(opToken, std::move(operand));
}


// FuncRParams ::= Exp {',' Exp}
std::vector<std::unique_ptr<ExpNode>> Parser::parseFuncRParams() {
    std::vector<std::unique_ptr<ExpNode>> args;
    args.push_back(parseExp());
    while (check(TokenType::COMMA)) {
        consume(); // ','
        args.push_back(parseExp());
    }
    return args;
}

// MulExp ::= UnaryExp {('*' | '/' | '%') UnaryExp}
std::unique_ptr<ExpNode> Parser::parseMulExp() {
    auto leftNode = parseUnaryExp();
    while (check(TokenType::MULT) || check(TokenType::DIV) || check(TokenType::MOD)) {
        Token opToken = consume();
        auto rightNode = parseUnaryExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// AddExp ::= MulExp {('+' | '-') MulExp}
std::unique_ptr<ExpNode> Parser::parseAddExp() {
    auto leftNode = parseMulExp();
    while (check(TokenType::PLUS) || check(TokenType::MINU)) {
        Token opToken = consume();
        auto rightNode = parseMulExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// RelExp ::= AddExp {('<' | '>' | '<=' | '>=') AddExp}
std::unique_ptr<ExpNode> Parser::parseRelExp() {
    auto leftNode = parseAddExp();
    while (check(TokenType::LSS) || check(TokenType::GRE) || check(TokenType::LEQ) || check(TokenType::GEQ)) {
        Token opToken = consume();
        auto rightNode = parseAddExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// EqExp ::= RelExp {('==' | '!=') RelExp}
std::unique_ptr<ExpNode> Parser::parseEqExp() {
    auto leftNode = parseRelExp();
    while (check(TokenType::EQL) || check(TokenType::NEQ)) {
        Token opToken = consume();
        auto rightNode = parseRelExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// LAndExp ::= EqExp {'&&' EqExp}
std::unique_ptr<ExpNode> Parser::parseLAndExp() {
    auto leftNode = parseEqExp();
    while (check(TokenType::AND)) {
        Token opToken = consume();
        auto rightNode = parseEqExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// LOrExp ::= LAndExp {'||' LAndExp}
std::unique_ptr<ExpNode> Parser::parseLOrExp() {
    auto leftNode = parseLAndExp();
    while (check(TokenType::OR)) {
        Token opToken = consume();
        auto rightNode = parseLAndExp();
        leftNode = std::make_unique<BinaryExpNode>(std::move(leftNode), opToken, std::move(rightNode));
    }
    return leftNode;
}

// Cond ::= LOrExp
std::unique_ptr<ExpNode> Parser::parseCond() {
    return parseLOrExp();
}

// Exp ::= AddExp
std::unique_ptr<ExpNode> Parser::parseExp() {
    return parseAddExp();
}

// ConstExp ::= AddExp
std::unique_ptr<ExpNode> Parser::parseConstExp() {
    // For AST structure, ConstExp is an AddExp.
    // Semantic analysis will later verify its constant nature.
    return parseAddExp();
}

// 'if' '(' Cond ')' Stmt ['else' Stmt]
std::unique_ptr<IfStmtNode> Parser::parseIfStmt() {
    Token ifToken = consume(TokenType::IFTK, "Expected 'if' keyword");
    consume(TokenType::LPARENT, "Expected '(' after 'if'");
    auto condition = parseCond();
    consume(TokenType::RPARENT, "Expected ')' after if condition");
    auto thenStmt = parseStmt();
    std::unique_ptr<StmtNode> elseStmt = nullptr;
    if (check(TokenType::ELSETK)) {
        consume(); // 'else'
        elseStmt = parseStmt();
    }
    return std::make_unique<IfStmtNode>(std::move(condition), std::move(thenStmt), std::move(elseStmt), ifToken.line);
}

// 'while' '(' Cond ')' Stmt
std::unique_ptr<WhileStmtNode> Parser::parseWhileStmt() {
    Token whileToken = consume(TokenType::WHILETK, "Expected 'while' keyword");
    consume(TokenType::LPARENT, "Expected '(' after 'while'");
    auto condition = parseCond();
    consume(TokenType::RPARENT, "Expected ')' after while condition");
    auto body = parseStmt();
    return std::make_unique<WhileStmtNode>(std::move(condition), std::move(body), whileToken.line);
}

// FormatString (used in printf)
std::unique_ptr<FormatStringNode> Parser::parseFormatString() {
    Token strToken = consume(TokenType::STRCON, "Expected format string for printf");
    return std::make_unique<FormatStringNode>(strToken);
}

