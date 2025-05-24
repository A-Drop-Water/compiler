#include "include/semantic_analyzer.h" // Adjust path as needed
#include <iostream> // For std::cerr in reportError

// Helper function to convert FuncType enum from AST to DataType enum for SymbolTable
DataType astFuncTypeToDataType(FuncType type) {
    if (type == FuncType::INT) {
        return DataType::INT;
    } else if (type == FuncType::VOID) {
        return DataType::VOID;
    }
    // Should not happen with current grammar
    throw std::runtime_error("Unknown AST FuncType encountered."); 
}

SemanticAnalyzer::SemanticAnalyzer(SymbolTable& table) : symbolTable(table), currentFunction(nullptr), loopDepth(0) {
}

void SemanticAnalyzer::reportError(const std::string& message, int lineNumber) {
    std::cerr << "Semantic Error (Line " << lineNumber << "): " << message << std::endl;
    // In a real compiler, you might count errors and stop after too many,
    // or throw an exception to halt compilation.
    // For now, just print to cerr.
    // Consider throwing a specific semantic_error exception.
    throw std::runtime_error("Semantic error occurred: " + message);
}

void SemanticAnalyzer::analyze(CompUnitNode* compUnit) {
    if (!compUnit) return;
    visit(compUnit);
}

void SemanticAnalyzer::visit(CompUnitNode* node) {
    // Global scope is already entered by SymbolTable constructor.
    // symbolTable.enterScope(); // Global scope

    for (const auto& decl : node->globalDecls) {
        visit(decl.get());
    }
    for (const auto& funcDef : node->funcDefs) {
        visit(funcDef.get());
    }
    if (node->mainFuncDef) {
        visit(node->mainFuncDef.get());
    }
    // symbolTable.exitScope(); // Exit global scope (optional, SymbolTable dtor might handle)
    // Exiting global scope here might be too early if table is used later.
    // Usually, symbol table lives as long as compilation process needs it.
}

// Dispatcher for DeclNode
void SemanticAnalyzer::visit(DeclNode* node) {
    if (!node) return;
    // Try to downcast to specific declaration types
    if (auto constDecl = dynamic_cast<ConstDeclNode*>(node)) {
        visit(constDecl);
    } else if (auto varDecl = dynamic_cast<VarDeclNode*>(node)) {
        visit(varDecl);
    } else {
        reportError("Unknown declaration node type.", node->lineNumber);
    }
}

void SemanticAnalyzer::visit(ConstDeclNode* node) {
    for (const auto& constDef : node->constDefs) {
        visit(constDef.get());
    }
}

void SemanticAnalyzer::visit(ConstDefNode* node) {
    // TODO: Evaluate ConstInitVal to get the constant's value.
    // For now, assume 0 or handle simple cases in evaluateConstExp.
    int constValue = 0; 
    if (node->constInitVal) {
        // A full const evaluator is needed here.
        // For a simple integer constant, it would be from ConstExp -> NumberNode.
        // If it's a single ConstExp:
        if (node->constInitVal->singleConstExp) {
            constValue = evaluateConstExp(node->constInitVal->singleConstExp.get());
        } else {
            // Array constant initializers are more complex to reduce to a single 'constValue' here.
            // This SymbolEntry field 'constantValue' is more for single int constants.
            // For array constants, the values are in the initializer list.
        }
    }


    DataType dt = DataType::INT; // SysY BType is always int for const
    std::vector<int> arrayDimSizes;
    if (!node->arrayDimensions.empty()) {
        dt = DataType::ARRAY_INT;
        for(const auto& dimExp : node->arrayDimensions) {
            arrayDimSizes.push_back(evaluateConstExp(dimExp.get()));
        }
    }

    SymbolEntry entry(node->ident, SymbolKind::CONSTANT, dt, symbolTable.getCurrentScopeLevel(), node->lineNumber);
    entry.constantValue = constValue; // Store the evaluated constant value
    entry.arrayDimensions = arrayDimSizes; // Store array dimension sizes

    if (!symbolTable.addSymbol(entry)) {
        reportError("Constant '" + node->ident + "' redefined in the current scope.", node->lineNumber);
    }
}

void SemanticAnalyzer::visit(VarDeclNode* node) {
    for (const auto& varDef : node->varDefs) {
        visit(varDef.get());
    }
}

void SemanticAnalyzer::visit(VarDefNode* node) {
    DataType dt = DataType::INT; // SysY BType is always int for var
    std::vector<int> arrayDimSizes;

    if (!node->arrayDimensions.empty()) {
        dt = DataType::ARRAY_INT;
        for(const auto& dimExp : node->arrayDimensions) {
            // Dimensions for variables must also be constant expressions
            arrayDimSizes.push_back(evaluateConstExp(dimExp.get()));
        }
    }
    
    // TODO: Process InitVal if present (for type checking or further analysis, e.g. array init list size)
    // For now, just adding the variable symbol.

    SymbolEntry entry(node->ident, SymbolKind::VARIABLE, dt, symbolTable.getCurrentScopeLevel(), node->lineNumber);
    entry.arrayDimensions = arrayDimSizes;

    if (!symbolTable.addSymbol(entry)) {
        reportError("Variable '" + node->ident + "' redefined in the current scope.", node->lineNumber);
    }
}

void SemanticAnalyzer::visit(FuncDefNode* node) {
    currentFunction = node; // Set current function context

    DataType returnType = astFuncTypeToDataType(node->funcType);
    SymbolEntry funcEntry(node->ident, SymbolKind::FUNCTION, returnType, symbolTable.getCurrentScopeLevel(), node->lineNumber);
    
    // Collect parameter types for the function signature
    for(const auto& param : node->params) {
        // SysY params are always int
        funcEntry.paramTypes.push_back(DataType::INT);
    }

    if (!symbolTable.addSymbol(funcEntry)) {
        reportError("Function '" + node->ident + "' redefined in the current scope.", node->lineNumber);
        // Potentially return or don't process body if redefinition is fatal for this function
    }

    symbolTable.enterScope(); // Enter new scope for parameters and function body

    // Add parameters to the new scope
    for (const auto& param : node->params) {
        visit(param.get(), funcEntry); // Pass funcEntry to link params if needed, or just add to table
    }

    visit(node->body.get(), true); // true indicates this block is a function body

    symbolTable.exitScope(); // Exit function scope
    currentFunction = nullptr; // Reset current function context
}

void SemanticAnalyzer::visit(MainFuncDefNode* node) {
    // 'main' function has fixed signature: int main()
    SymbolEntry funcEntry("main", SymbolKind::FUNCTION, DataType::INT, symbolTable.getCurrentScopeLevel(), node->lineNumber);
    // No parameters for main in SysY

    if (!symbolTable.addSymbol(funcEntry)) {
        reportError("Function 'main' redefined in the current scope (should not happen if lexer/parser identify 'main' uniquely).", node->lineNumber);
    }
    
    // Represent main as a FuncDefNode for currentFunction context temporarily
    // This is a bit of a hack. Better to have currentFunction be a struct/variant.
    FuncType mainAstFuncType = FuncType::INT; // Main returns int
    std::vector<std::unique_ptr<FuncFParamNode>> mainAstParams; // Empty params
    // Create a temporary FuncDefNode for context. This is not ideal.
    // A better way would be to have a common structure for function context.
    // For now, we can set currentFunction to null or a special marker for main,
    // or adapt the ReturnStmt check.
    // Let's make a dummy FuncDefNode for context of return type.
    FuncDefNode mainContextFuncDef(mainAstFuncType, "main", std::move(mainAstParams), nullptr /*body not needed for this context obj*/, node->lineNumber);
    currentFunction = &mainContextFuncDef;


    symbolTable.enterScope(); // Enter scope for main function's body
    visit(node->body.get(), true); // true indicates this block is a function body
    symbolTable.exitScope(); // Exit main function scope
    currentFunction = nullptr;
}

// For FuncFParamNode, we are adding it to the symbol table within the function's scope.
void SemanticAnalyzer::visit(FuncFParamNode* node, SymbolEntry& funcEntry /* unused for now */) {
    // Parameters are always DataType::INT in SysY
    // Parameter kind is PARAMETER or VARIABLE. Let's use PARAMETER for now.
    SymbolEntry paramEntry(node->ident, SymbolKind::PARAMETER, DataType::INT, symbolTable.getCurrentScopeLevel(), node->lineNumber);
    // SysY parameters are simple integers, not arrays.
    
    if (!symbolTable.addSymbol(paramEntry)) {
        reportError("Parameter '" + node->ident + "' redefined in function scope.", node->lineNumber);
    }
}


// STUBS for other visit methods (Block, Stmt, Exp) - to be implemented next.
void SemanticAnalyzer::visit(BlockNode* node, bool isFunctionBody) {
    // If this block is NOT a function body, it creates its own new scope.
    // If it IS a function body, the scope is already created by visit(FuncDefNode).
    if (!isFunctionBody) {
        symbolTable.enterScope();
    }

    for (const auto& item : node->items) {
        visit(item.get());
    }

    if (!isFunctionBody) {
        symbolTable.exitScope();
    }
}

void SemanticAnalyzer::visit(BlockItemNode* node) {
    if (node->decl) {
        visit(node->decl.get());
    } else if (node->stmt) {
        visit(node->stmt.get()); // Stubbed
    }
}

// Stubs for statements and expressions
void SemanticAnalyzer::visit(StmtNode* node) { 
    if (!node) return;
    // Basic dynamic_cast dispatcher for statements
    if (auto s = dynamic_cast<AssignStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ExpStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<IfStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<WhileStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<BreakStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ContinueStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ReturnStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<PrintfStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<BlockNode*>(node)) {
        // Determine if this block is part of a function body or a standalone block.
        // This information isn't directly available here without context.
        // The existing visit(BlockNode*, bool) takes this.
        // For a generic StmtNode dispatch, we assume it's not a function body scope here.
        visit(s, false); 
    } else {
        // This might indicate an AST node type that StmtNode can be, but isn't handled above
        // (e.g. if a new statement type was added to AST but not to this dispatcher)
        // Or, it's an actual AstNode that isn't a StmtNode somehow (shouldn't happen if AST is well-typed)
        reportError("Unknown or unhandled statement node type.", node->lineNumber);
    }
}


void SemanticAnalyzer::visit(AssignStmtNode* node) {
    // Get type of LVal (also checks if LVal is valid)
    DataType lvalType = visit(node->lval.get());

    // Check if LVal is assignable (not a constant)
    // We need to get the base identifier of the LVal to check its symbol kind.
    // LValNode in AST stores the ident string.
    SymbolEntry* lvalSymbol = symbolTable.lookupSymbol(node->lval->ident);
    if (lvalSymbol && lvalSymbol->kind == SymbolKind::CONSTANT) {
        reportError("Cannot assign to constant '" + node->lval->ident + "'.", node->lineNumber);
    }

    DataType expType = visit(node->exp.get());

    // Type checking for assignment (SysY: int = int)
    if (lvalType != DataType::INT || expType != DataType::INT) {
        // Error already reported by LVal/Exp visit if they are not int where expected
        // e.g. array used as int, or operation that doesn't yield int.
        // Here, ensure both sides ultimately resolved to INT for the assignment.
        if (lvalType != DataType::INT) {
             reportError("Left-hand side of assignment must be an integer type.", node->lval->lineNumber);
        }
        if (expType != DataType::INT) {
             reportError("Right-hand side of assignment must be an integer type.", node->exp->lineNumber);
        }
    }
}

void SemanticAnalyzer::visit(ExpStmtNode* node) {
    if (node->exp) {
        visit(node->exp.get()); // Evaluate expression for side effects and errors
    }
}

void SemanticAnalyzer::visit(IfStmtNode* node) {
    DataType condType = visit(node->condition.get());
    if (condType != DataType::INT) {
        reportError("If statement condition must be an integer (boolean-like).", node->condition->lineNumber);
    }
    visit(node->thenStmt.get());
    if (node->elseStmt) {
        visit(node->elseStmt.get());
    }
}

void SemanticAnalyzer::visit(WhileStmtNode* node) {
    loopDepth++;
    DataType condType = visit(node->condition.get());
    if (condType != DataType::INT) {
        reportError("While statement condition must be an integer (boolean-like).", node->condition->lineNumber);
    }
    visit(node->body.get());
    loopDepth--;
}

void SemanticAnalyzer::visit(BreakStmtNode* node) {
    if (loopDepth <= 0) {
        reportError("'break' statement not inside a loop.", node->lineNumber);
    }
}

void SemanticAnalyzer::visit(ContinueStmtNode* node) {
    if (loopDepth <= 0) {
        reportError("'continue' statement not inside a loop.", node->lineNumber);
    }
}

void SemanticAnalyzer::visit(ReturnStmtNode* node) {
    if (!currentFunction) {
        // This should not happen if parser ensures return is within a function.
        // Main function context is handled by setting currentFunction appropriately.
        reportError("'return' statement outside of a function.", node->lineNumber);
        return;
    }

    // Need the return type of the currentFunction from its SymbolEntry
    // The currentFunction pointer is FuncDefNode*. Its name is node->ident.
    // For main, currentFunction is a temporary FuncDefNode with name "main" and type INT.
    SymbolEntry* funcSym = symbolTable.lookupSymbol(currentFunction->ident); 
    // For main, if currentFunction->ident is "main", funcSym will be the main function symbol.
    
    DataType expectedReturnType = DataType::VOID; // Default if symbol not found (error already reported)
    if(funcSym) {
        expectedReturnType = funcSym->type;
    } else if (currentFunction->ident == "main") { // Fallback for the temporary main FuncDefNode context
        expectedReturnType = DataType::INT;
    } else {
         reportError("Could not determine return type for current function context.", node->lineNumber);
         // This implies an issue with how currentFunction is set or symbol table for functions.
    }


    if (expectedReturnType == DataType::VOID) {
        if (node->returnExp) {
            reportError("Cannot return a value from a void function.", node->returnExp->lineNumber);
        }
    } else { // Expected INT return
        if (!node->returnExp) {
            reportError("Must return a value from a non-void (int) function.", node->lineNumber);
        } else {
            DataType actualReturnType = visit(node->returnExp.get());
            if (actualReturnType != DataType::INT) {
                reportError("Return expression type mismatch. Expected INT.", node->returnExp->lineNumber);
            }
        }
    }
}

void SemanticAnalyzer::visit(PrintfStmtNode* node) {
    // 1. Count %d in formatString
    size_t expectedArgs = 0;
    for (size_t i = 0; i < node->formatString.length(); ++i) {
        if (node->formatString[i] == '%') {
            if (i + 1 < node->formatString.length() && node->formatString[i+1] == 'd') {
                expectedArgs++;
                i++; // Skip 'd'
            } else {
                // SysY grammar for FormatChar is only '%d'. Other % are implicitly errors by grammar.
                // The lexer should ensure FormatString contains valid chars as per grammar.
                // Here, we could report an error if a '%' is not followed by 'd'.
                // reportError("Invalid format specifier in printf string: " + node->formatString.substr(i,2), node->lineNumber);
            }
        }
        // SysY grammar for NormalChar: '\' appears if and only if it is '\n'.
        // This validation could also be done here if not fully by lexer/parser.
        else if (node->formatString[i] == '\\') { // Check for backslash
             if (!(i + 1 < node->formatString.length() && node->formatString[i+1] == 'n')) {
                 reportError("Invalid escape sequence in printf format string. Only '\\n' is allowed.", node->lineNumber);
             } // else, it's '\n', which is fine.
             i++; // Skip 'n'
        }
    }

    // 2. Check number of arguments
    if (node->args.size() != expectedArgs) {
        reportError("Printf: Number of arguments (" + std::to_string(node->args.size()) + 
                    ") does not match number of format specifiers (%d): " + std::to_string(expectedArgs) + ".", node->lineNumber);
    }

    // 3. Visit each argument expression; its type must be INT
    for (size_t i = 0; i < node->args.size(); ++i) {
        DataType argType = visit(node->args[i].get());
        if (argType != DataType::INT) {
            reportError("Printf: Argument " + std::to_string(i+1) + " type mismatch. Expected INT.", node->args[i]->lineNumber);
        }
    }
}

// DataType SemanticAnalyzer::visit(ExpNode* node) - Dispatcher
DataType SemanticAnalyzer::visit(ExpNode* node) {
    if (!node) {
        reportError("Encountered null expression node.", 0); // Or a more specific line if possible
        return DataType::INT; // Default/error recovery type
    }

    if (auto n = dynamic_cast<NumberNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<LValNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<BinaryExpNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<UnaryExpNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<FuncCallNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<GetIntNode*>(node)) return visit(n);
    
    reportError("Unknown expression node type.", node->lineNumber);
    return DataType::INT; // Default/error recovery
}

DataType SemanticAnalyzer::visit(NumberNode* node) {
    return DataType::INT;
}

DataType SemanticAnalyzer::visit(LValNode* node) {
    SymbolEntry* entry = symbolTable.lookupSymbol(node->ident);
    if (!entry) {
        reportError("Identifier '" + node->ident + "' not declared.", node->lineNumber);
        return DataType::INT; // Error recovery: assume int to allow further checks
    }

    if (entry->kind == SymbolKind::FUNCTION) {
        reportError("Identifier '" + node->ident + "' is a function, not a variable/constant.", node->lineNumber);
        return entry->type; // Return function's return type for recovery
    }

    if (node->index) { // Array element access: e.g., arr[i]
        if (entry->type != DataType::ARRAY_INT) {
            reportError("Identifier '" + node->ident + "' is not an array, cannot be indexed.", node->lineNumber);
            return DataType::INT; // Error recovery
        }
        DataType indexType = visit(node->index.get());
        if (indexType != DataType::INT) {
            reportError("Array index for '" + node->ident + "' must be an integer.", node->index->lineNumber);
        }
        return DataType::INT; // Accessing an element of an int array yields an int
    } else { // Not an array access: e.g., simple variable 'x' or constant 'C'
        if (entry->type == DataType::ARRAY_INT) {
            // Using an array identifier without index might be an error depending on context
            // e.g. `int arr[5]; int x = arr;` is usually an error or means pointer assignment (not in SysY)
            // For SysY, if an array is used as a value, it's typically an error unless it's for specific ops
            // not present here (like passing array to function, which implicitly becomes pointer).
            // For now, let's consider using array name directly as an error if not contextually handled.
            // This might be refined when checking specific statements like function calls.
             reportError("Array '" + node->ident + "' used without index.", node->lineNumber);
             return DataType::ARRAY_INT; // Return its actual type for now
        }
        return entry->type; // Return type of the variable or constant
    }
}

DataType SemanticAnalyzer::visit(BinaryExpNode* node) {
    DataType leftType = visit(node->left.get());
    DataType rightType = visit(node->right.get());

    // For SysY, all binary operations are on INTs.
    // Result type is also INT (0 or 1 for boolean results).
    bool typeError = false;
    if (leftType != DataType::INT) {
        reportError("Left operand of binary operator is not an integer.", node->left->lineNumber);
        typeError = true;
    }
    if (rightType != DataType::INT) {
        reportError("Right operand of binary operator is not an integer.", node->right->lineNumber);
        typeError = true;
    }

    if (typeError) {
        return DataType::INT; // Error recovery
    }

    // Operators like +, -, *, /, %, <, >, <=, >=, ==, !=, &&, || all produce INT in SysY
    // (0 or 1 for relational/logical)
    return DataType::INT;
}

DataType SemanticAnalyzer::visit(UnaryExpNode* node) {
    DataType operandType = visit(node->operand.get());
    
    // Unary +, - expect INT. Result is INT.
    // Unary ! (NOT) expects INT (boolean-like). Result is INT.
    if (operandType != DataType::INT) {
        reportError("Operand of unary operator is not an integer.", node->operand->lineNumber);
        return DataType::INT; // Error recovery
    }
    return DataType::INT;
}

DataType SemanticAnalyzer::visit(FuncCallNode* node) {
    SymbolEntry* funcEntry = symbolTable.lookupSymbol(node->funcIdent);
    if (!funcEntry) {
        reportError("Function '" + node->funcIdent + "' not declared.", node->lineNumber);
        return DataType::INT; // Error recovery, assume int return type
    }
    if (funcEntry->kind != SymbolKind::FUNCTION) {
        reportError("'" + node->funcIdent + "' is not a function.", node->lineNumber);
        return DataType::INT; // Error recovery
    }

    // Check number of arguments
    if (node->args.size() != funcEntry->paramTypes.size()) {
        reportError("Function '" + node->funcIdent + "' called with incorrect number of arguments. Expected " +
                    std::to_string(funcEntry->paramTypes.size()) + ", got " + std::to_string(node->args.size()) + ".", node->lineNumber);
        // Continue to check types of args provided, if any
    }

    // Check type of each argument (SysY params are all INT)
    for (size_t i = 0; i < node->args.size(); ++i) {
        DataType argType = visit(node->args[i].get());
        if (i < funcEntry->paramTypes.size()) { // Only check if param exists
            if (argType != funcEntry->paramTypes[i]) { // SysY: all params are INT
                 reportError("Type mismatch for argument " + std::to_string(i+1) + " in call to function '" + 
                             node->funcIdent + "'. Expected INT.", node->args[i]->lineNumber);
            }
        }
    }
    return funcEntry->type; // Return the function's declared return type
}

DataType SemanticAnalyzer::visit(GetIntNode* node) {
    return DataType::INT;
}

// Placeholder for constant expression evaluation
// This needs to traverse the expression and compute its value if possible.
// Requires expression visiting logic to be more complete.
int SemanticAnalyzer::evaluateConstExp(ConstExpNode* constExp) {
    if (!constExp || !constExp->exp) {
        reportError("Invalid constant expression.", constExp ? constExp->lineNumber : 0);
        return 0; // Default or error value
    }
    // For now, very simplified: if it's a NumberNode, return its value.
    // A real implementation would recursively evaluate the expression.
    if (NumberNode* numNode = dynamic_cast<NumberNode*>(constExp->exp.get())) {
        return numNode->value;
    }
    // If it's an identifier, it must be a known constant.
    if (LValNode* lvalNode = dynamic_cast<LValNode*>(constExp->exp.get())) {
        if (lvalNode->index) { // Array elements not supported as const exp this way for now
            reportError("Array element access not supported in this phase of const expression evaluation.", lvalNode->lineNumber);
            return 0;
        }
        SymbolEntry* entry = symbolTable.lookupSymbol(lvalNode->ident);
        if (entry && entry->kind == SymbolKind::CONSTANT && entry->type == DataType::INT) {
            return entry->constantValue;
        } else {
            reportError("Identifier '" + lvalNode->ident + "' is not a recognized integer constant.", lvalNode->lineNumber);
            return 0;
        }
    }
    // TODO: Handle simple binary/unary operations on constants if grammar allows for ConstExp.
    // The SysY grammar for ConstExp -> AddExp implies it can be complex.
    reportError("Complex constant expression evaluation is not fully implemented.", constExp->lineNumber);
    return 0; // Default for unhandled constant expressions
}
