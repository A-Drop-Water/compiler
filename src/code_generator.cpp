#include "include/code_generator.h" // Adjust if necessary
#include "include/symbol_table.h" // For SymbolEntry details
#include <algorithm> // For std::find if used for string literals

CodeGenerator::CodeGenerator(SymbolTable& table) : symbolTable(table), currentLabelId(0), currentStackOffset(0), currentFunctionArgSpace(0) {
    // Initialize temporary register status (e.g., $t0-$t9 are free)
    // Assuming 10 temporary registers $t0-$t7 ($8-$15), $s0-$s7 ($16-$23)
    // Let's use $t0-$t7 for now. $t8, $t9 are $24, $25
    tempRegStatus.resize(8, true); // For $t0 ($8) to $t7 ($15)
}

std::string CodeGenerator::newLabel(const std::string& prefix) {
    return prefix + std::to_string(currentLabelId++);
}

void CodeGenerator::emit(const std::string& instruction) {
    mipsFile << "  " << instruction << std::endl;
}

void CodeGenerator::emitLabel(const std::string& label) {
    mipsFile << label << ":" << std::endl;
}

void CodeGenerator::emitComment(const std::string& comment) {
    mipsFile << "  # " << comment << std::endl;
}

void CodeGenerator::emitDataSegment() {
    mipsFile << ".data" << std::endl;
    // Emit any pre-defined data, like newline character for printf
    emitStringConstant("_newline", "\n"); // Predefined for convenience
}

void CodeGenerator::emitTextSegment() {
    mipsFile << ".text" << std::endl;
    mipsFile << ".globl main" << std::endl; // main function must be global
}

void CodeGenerator::emitGlobalVar(const std::string& varName, bool isArray, int arraySizeInElements) {
    emitLabel(varName);
    if (isArray) {
        emit(".space " + std::to_string(arraySizeInElements * 4)); // SysY ints are 4 bytes
    } else {
        emit(".word 0"); // Initialize global vars to 0
    }
}

void CodeGenerator::emitStringConstant(const std::string& strLabel, const std::string& strValue) {
    emitLabel(strLabel);
    // Properly escape string for MIPS .asciiz
    std::string escapedString = "";
    for (char c : strValue) {
        if (c == '\n') {
            escapedString += "\\n"; // MIPS .asciiz needs backslash escaped for newline
        } else if (c == '\t') {
            escapedString += "\\t";
        } else if (c == '"') {
            // .asciiz strings in MIPS are null-terminated, quotes are part of the string if included
            // If the string itself contains quotes, they don't need special escaping for .asciiz itself
            // but be careful if this string is then used in e.g. C contexts.
            // For MIPS .asciiz, "foo" is stored as f,o,o,\0. "a\"b" is a, ", b, \0.
            escapedString += c; 
        } else if (c == '\\') {
            escapedString += "\\\\";
        }
        else {
            escapedString += c;
        }
    }
    emit(".asciiz \"" + escapedString + "\""); // Enclose in quotes for .asciiz directive
}

std::string CodeGenerator::addStringLiteral(const std::string& str) {
    // Check if string already exists to reuse label
    for (const auto& pair : stringLiterals) {
        if (pair.second == str) {
            return pair.first;
        }
    }
    std::string label = newLabel("str_");
    stringLiterals[label] = str;
    return label;
}


void CodeGenerator::generate(CompUnitNode* compUnit, const std::string& outputFilename) {
    mipsFile.open(outputFilename);
    if (!mipsFile.is_open()) {
        std::cerr << "Error: Could not open output file " << outputFilename << std::endl;
        return;
    }

    emitDataSegment();
    // Global declarations are processed by visit(CompUnitNode) which then calls
    // visit(DeclNode) etc. These will populate the .data segment.
    
    // String literals are collected during AST traversal (e.g. in PrintfStmt)
    // and then emitted together. We'll call visit(compUnit) which does all generation,
    // then emit the collected strings.

    visit(compUnit); 

    // After all code is generated (including function bodies that might use strings),
    // emit collected string literals into the .data segment.
    // Note: This means .data segment might be "reopened" if text segment was already started.
    // This is fine for most assemblers. Or, collect all data items first.
    // The current structure emits .data, then .text via CompUnit, then more .data for strings.
    // Better to emit all .data, then all .text.
    // Let's adjust: CompUnit will first call global decls (data), then functions (text).
    // Then we emit string literals here, still under .data conceptually.
    // To ensure correct segment order, string literals should be emitted before .text or after all .text
    // If emitDataSegment is called once at start, and text segment after, string literals should be emitted
    // before emitTextSegment or ensure they are added to a list and then all emitted together.
    // The current structure is: emitDataSegment -> visit(CompUnit) [-> emitTextSegment -> functions] -> emitStrings
    // This is fine as long as emitStringConstant directly writes to mipsFile and it's known we are in .data
    // Or, more robustly:
    // 1. Collect all global vars/consts (but don't emit yet)
    // 2. Traverse functions to collect string literals and other data items.
    // 3. Emit .data, then all global vars/consts, then all string literals.
    // 4. Emit .text, then all functions.
    // For now, let's stick to the simpler model and emit strings at the end of the .data segment.
    // The `visit(CompUnitNode)` will handle global vars first, then switch to .text.
    // So, string literals collected during function visits need to be emitted *before* text or at end of data.
    // The current approach of emitting strings *after* visit(compUnit) implies they are added to .data *after* .text.
    // This is generally acceptable.

    if (!stringLiterals.empty()) {
        emitComment("String Literals (appended to .data segment)");
        for (const auto& pair : stringLiterals) {
            emitStringConstant(pair.first, pair.second);
        }
    }

    mipsFile.close();
}

void CodeGenerator::visit(CompUnitNode* node) {
    emitComment("Compilation Unit Start");

    // Global declarations (constants and variables) go into .data segment
    for (const auto& decl : node->globalDecls) {
        visit(decl.get()); 
    }
    
    // After all data for globals is laid out (and string literals collected if any were in global inits),
    // switch to .text segment for functions.
    emitTextSegment(); 

    for (const auto& funcDef : node->funcDefs) {
        visit(funcDef.get());
    }
    if (node->mainFuncDef) {
        visit(node->mainFuncDef.get());
    }
    emitComment("Compilation Unit End");
}

// Global Declarations
void CodeGenerator::visit(DeclNode* node) {
    if (auto constDecl = dynamic_cast<ConstDeclNode*>(node)) {
        visit(constDecl);
    } else if (auto varDecl = dynamic_cast<VarDeclNode*>(node)) {
        visit(varDecl);
    }
    // else error or unknown decl type
}

void CodeGenerator::visit(ConstDeclNode* node) {
    emitComment("Global Constant Declaration");
    for (const auto& def : node->constDefs) {
        SymbolEntry* entry = symbolTable.lookupSymbol(def->ident); // Should be in global scope
        if (!entry) {
            emitComment("Error: SymbolTable entry not found for global const " + def->ident);
            continue;
        }

        if (entry->type == DataType::ARRAY_INT) {
            // Global constant arrays. Values must come from ConstInitVal.
            // This requires iterating through ConstInitVal and emitting .word directives.
            emitComment("Global const array " + def->ident);
            emitLabel(def->ident);
            // Example: const int c[3] = {10, 20, 30};
            // Need to evaluate ConstInitVal here.
            if (def->constInitVal && !def->constInitVal->elements.empty()) {
                for(const auto& elemInitVal : def->constInitVal->elements) {
                    if (elemInitVal->singleConstExp) {
                        // Semantic analyzer should have evaluated this to an int value.
                        // For codegen, we re-evaluate or assume it's simple.
                        // int val = evaluateConstExp(elemInitVal->singleConstExp.get()); // Need this
                        // emit(".word " + std::to_string(val));
                         emit(".word 0 # Placeholder for const array element"); // Placeholder
                    } else {
                         emit(".word 0 # Placeholder for complex const array element");
                    }
                }
            } else if (def->constInitVal && def->constInitVal->singleConstExp) {
                 emitComment("Error: Global const array " + def->ident + " initialized with single value instead of list.");
            }
            else { // No initializer or empty, ensure space is allocated based on dimensions
                int totalSize = 1;
                for(int dimSize : entry->arrayDimensions) totalSize *= dimSize;
                if (totalSize == 0 && entry->arrayDimensions.empty() && def->constInitVal == nullptr){
                    emitComment("Error: Global const array " + def->ident + " has no dimension and no initializer.");
                } else if (totalSize > 0) {
                     for(int i=0; i<totalSize; ++i) emit(".word 0 # Uninit const array element");
                }
            }
        } else { // Simple INT constant
            emitLabel(def->ident);
            emit(".word " + std::to_string(entry->constantValue));
        }
    }
}

void CodeGenerator::visit(VarDeclNode* node) {
    emitComment("Global Variable Declaration");
    for (const auto& def : node->varDefs) {
        SymbolEntry* entry = symbolTable.lookupSymbol(def->ident); // Global scope
        if (!entry) {
             emitComment("Error: SymbolTable entry not found for global var " + def->ident);
            continue;
        }

        if (entry->type == DataType::ARRAY_INT) {
            int totalSize = 1;
            if (!entry->arrayDimensions.empty()) {
                for(int dimSize : entry->arrayDimensions) totalSize *= dimSize;
            } else { 
                totalSize = 0; // Should be caught by semantic analysis if no size and no init
            }
            emitGlobalVar(def->ident, true, totalSize);
            // Handle initializers for global arrays if present in def->initVal
            // Similar to const arrays, iterate InitVal and emit .word
        } else { // INT
            emitGlobalVar(def->ident, false); // Initialized to 0 by default
            // Handle initializer for global scalar: def->initVal
            // If def->initVal->singleExp is a NumberNode, can emit its value.
            // Otherwise, if it's a complex expression, it must be const-evaluatable.
        }
    }
}


// Function Prolog and Epilog
void CodeGenerator::emitFunctionProlog(const std::string& funcName, int localStackSize) {
    emitLabel(funcName);
    emitComment("Function Prolog for " + funcName);
    
    emit("addi $sp, $sp, -8"); // Make space for $ra, $fp
    emit("sw $ra, 4($sp)");   // Save return address
    emit("sw $fp, 0($sp)");   // Save old frame pointer

    emit("move $fp, $sp");    // Set up new frame pointer

    if (localStackSize > 0) {
        emit("addi $sp, $sp, -" + std::to_string(localStackSize));
    }
    // Parameter access: params are at positive offsets from $fp.
    // e.g., 8($fp) for first param, 12($fp) for second, etc.
    // This is because $fp points to saved old $fp. $ra is above old $fp. Params are above $ra.
}

void CodeGenerator::emitFunctionEpilog(const std::string& funcName, bool isMain) {
    emitComment("Function Epilog for " + funcName);
    if (isMain) {
        emit("li $v0, 10"); // syscall for exit
        emit("syscall");
    } else {
        // Result of function should be in $v0 if non-void (handled by ReturnStmt visit)
        emit("move $sp, $fp");    // Deallocate local vars: $sp = $fp (base of current frame)
        
        emit("lw $ra, 4($sp)");   // Restore $ra from where we saved it
        emit("lw $fp, 0($sp)");   // Restore old $fp
        emit("addi $sp, $sp, 8");  // Adjust stack pointer back (popping $ra, $fp)
        
        emit("jr $ra");
    }
    emit(""); // Extra newline for readability
}

// Temporary Register Management
std::string CodeGenerator::getTempReg() {
    for (size_t i = 0; i < tempRegStatus.size(); ++i) {
        if (tempRegStatus[i]) {
            tempRegStatus[i] = false; // Mark as used
            return "$t" + std::to_string(i);
        }
    }
    emitComment("ERROR: Out of temporary registers! Spilling not implemented.");
    // This is a critical error. Real compiler would spill to stack.
    throw std::runtime_error("Out of temporary registers.");
    return "$zero"; // Fallback, should not be used if throwing
}

void CodeGenerator::freeTempReg(const std::string& reg) {
    if (reg.rfind("$t", 0) == 0 && reg.length() > 2) {
        try {
            int regNum = std::stoi(reg.substr(2));
            if (regNum >= 0 && regNum < tempRegStatus.size()) {
                if (tempRegStatus[regNum]) {
                    emitComment("Warning: Attempting to free already free register " + reg);
                }
                tempRegStatus[regNum] = true; // Mark as free
            } else {
                 emitComment("Warning: Attempting to free invalid temp register number " + reg);
            }
        } catch (const std::invalid_argument& ia) {
             emitComment("Warning: Could not parse register number from " + reg + " for freeing.");
        }
    } else {
        // emitComment("Info: Attempting to free non-temp register or non-register " + reg);
    }
}


// STUBS for other visit methods
void CodeGenerator::visit(FuncDefNode* node) {
    emitComment("Function Definition: " + node->ident);
    currentFunctionArgSpace = 0; 
    localVarOffsets.clear();
    funcParamOffsets.clear();
    currentStackOffset = 0; // Base for calculating local var offsets from $fp (negative)

    // Generate a unique label for the function's epilogue
    this->currentFuncEpilogueLabel = newLabel(node->ident + "_epilogue");

    // Calculate parameter offsets (positive from $fp)
    int paramCurrentOffset = 8; // Params start at $fp+8 (after saved $ra, old $fp)
    for(const auto& paramNode : node->params) {
        funcParamOffsets[paramNode->ident] = paramCurrentOffset;
        paramCurrentOffset += 4;
    }

    // Calculate stack size needed for local variables
    int localDataSize = 0;
    if (node->body) {
        for (const auto& item : node->body->items) {
            if (item->decl) {
                if (auto varDecl = dynamic_cast<VarDeclNode*>(item->decl.get())) {
                    for (const auto& varDef : varDecl->varDefs) {
                         SymbolEntry* sym = symbolTable.lookupSymbol(varDef->ident); 
                         if(sym) { 
                             int varSize = 4; 
                             if (sym->type == DataType::ARRAY_INT) {
                                 varSize = 4; 
                                 if (!sym->arrayDimensions.empty()) {
                                     for(int dim : sym->arrayDimensions) varSize *= dim;
                                     if (varSize == 0) varSize = 4; // Prevent 0 size for empty dim array
                                 } else {varSize = 4;} 
                             }
                             currentStackOffset -= varSize; 
                             localVarOffsets[varDef->ident] = currentStackOffset;
                             localDataSize += varSize;
                         }
                    }
                } else if (auto constDecl = dynamic_cast<ConstDeclNode*>(item->decl.get())) {
                     for (const auto& constDef : constDecl->constDefs) {
                        SymbolEntry* sym = symbolTable.lookupSymbol(constDef->ident);
                        if (sym && sym->kind == SymbolKind::CONSTANT) { // Check if it's actually a constant
                            // If constants are stack-allocated (e.g. if not all are compile-time foldable into instructions)
                            // For SysY, const int are usually immediate values or loaded from .data if global.
                            // Local constants are typically replaced or loaded as immediates.
                            // If a local const *needed* stack space (e.g. a const array struct), it would be here.
                            // SysY consts are simple int or int arrays. Local const arrays are not in grammar.
                            // Local const int should be handled by loading immediate value.
                            // So, typically, local consts don't take stack space like vars.
                        }
                    }
                }
            }
        }
    }
    currentStackOffset = 0; 

    emitFunctionProlog(node->ident, localDataSize);
    if (node->body) visit(node->body.get(), false); 
    
    emitLabel(this->currentFuncEpilogueLabel); // Emit the epilogue label
    emitFunctionEpilog(node->ident);
    this->currentFuncEpilogueLabel = ""; // Clear after function
}

void CodeGenerator::visit(MainFuncDefNode* node) {
    emitComment("Main Function Definition");
    currentFunctionArgSpace = 0;
    localVarOffsets.clear();
    funcParamOffsets.clear(); 
    currentStackOffset = 0;

    this->currentFuncEpilogueLabel = newLabel("main_epilogue");

    int localDataSize = 0;
     if (node->body) { 
        for (const auto& item : node->body->items) {
            if (item->decl) {
                if (auto varDecl = dynamic_cast<VarDeclNode*>(item->decl.get())) {
                    for (const auto& varDef : varDecl->varDefs) {
                         SymbolEntry* sym = symbolTable.lookupSymbol(varDef->ident);
                         if(sym) {
                             int varSize = 4;
                             if (sym->type == DataType::ARRAY_INT) {
                                 varSize = 4;
                                  if (!sym->arrayDimensions.empty()) {
                                     for(int dim : sym->arrayDimensions) varSize *= dim;
                                     if (varSize == 0) varSize = 4;
                                 } else {varSize = 4;}
                             }
                             currentStackOffset -= varSize;
                             localVarOffsets[varDef->ident] = currentStackOffset;
                             localDataSize += varSize;
                         }
                    }
                }
            }
        }
    }
    currentStackOffset = 0;

    emitFunctionProlog("main", localDataSize);
    if (node->body) visit(node->body.get(), false);

    emitLabel(this->currentFuncEpilogueLabel);
    emitFunctionEpilog("main", true); 
    this->currentFuncEpilogueLabel = "";
}

void CodeGenerator::visit(BlockNode* node, bool isNewScope /* true by default, but false for func body's main block */) {
    // Symbol table scopes are managed by SemanticAnalyzer.
    // Here, 'isNewScope' might mean if this block needs its own sub-stack frame, not common in SysY.
    // For SysY, all locals of a function share one frame.
    // So, this flag is more about conceptual nesting than MIPS stack frames.
    emitComment("Block Start");
    for (const auto& item : node->items) {
        visit(item.get());
    }
    emitComment("Block End");
}

void CodeGenerator::visit(BlockItemNode* node) {
    if (node->decl) {
        // Local declarations: space is already allocated in function prolog.
        // Here, we only generate code for initializers.
        if (auto varDecl = dynamic_cast<VarDeclNode*>(node->decl.get())) {
            for (const auto& varDef : varDecl->varDefs) {
                if (varDef->initVal) {
                    emitComment("Local Var Init: " + varDef->ident);
                    // LValNode for the variable being initialized
                    // The LValNode needs to be constructed to represent the varDef->ident
                    // We need its address.
                    std::string lvalIdent = varDef->ident;
                    std::unique_ptr<LValNode> lvalAstNode = std::make_unique<LValNode>(lvalIdent, varDef->lineNumber);
                    
                    std::string addrReg = visit(lvalAstNode.get(), true); // Get address of local var

                    // InitValNode (single Exp for scalar, or list for array)
                    if (varDef->initVal->singleExp) {
                        std::string expReg = visit(varDef->initVal->singleExp.get());
                        emit("sw " + expReg + ", 0(" + addrReg + ")"); // Store value
                        freeTempReg(expReg);
                    } else {
                        // Array initializers for local arrays TBD (complex, involves loops or .data section help)
                        emitComment("Local array initializer for " + varDef->ident + " TBD.");
                    }
                    freeTempReg(addrReg);
                }
            }
        }
        // ConstDecls at local scope: if they are truly compile-time constants, they might not need stack space
        // or code here unless their value is computed by an expression that needs runtime evaluation (not in SysY consts).
        // For SysY, local consts are initialized and then treated as read-only. Their values are known.
        // Code for initialization is similar to VarDecl.
    } else if (node->stmt) {
        visit(node->stmt.get());
    }
}

// Stubs for statements and expressions - to be implemented next
void CodeGenerator::visit(StmtNode* node) { 
    if (!node) return;
    if (auto s = dynamic_cast<AssignStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ExpStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<IfStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<WhileStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<BreakStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ContinueStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<ReturnStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<PrintfStmtNode*>(node)) visit(s);
    else if (auto s = dynamic_cast<BlockNode*>(node)) visit(s); // Calls BlockNode with isNewScope=true by default
    else emitComment("Warning: Unknown StmtNode type in CodeGenerator: " + std::to_string(node->lineNumber));
}

void CodeGenerator::visit(AssignStmtNode* node) {
    emitComment("AssignStmt: " + node->lval->ident + " = ...");
    
    // For LVal = getint(), SemanticAnalyzer ensures node->exp is a GetIntNode.
    // Code for getint() is handled by visit(GetIntNode*).
    std::string valReg = visit(node->exp.get()); // Value to be stored is in valReg
    
    // Get address of LVal
    // For array assignments like arr[idx] = val, visit(LValNode*, true) handles address calculation.
    std::string addrReg = visit(node->lval.get(), true); // true for needsAddress

    emit("sw " + valReg + ", 0(" + addrReg + ")");

    freeTempReg(valReg);
    freeTempReg(addrReg);
}

void CodeGenerator::visit(ExpStmtNode* node) {
    emitComment("ExpStmt");
    if (node->exp) {
        std::string reg = visit(node->exp.get());
        freeTempReg(reg); // Result of expression in ExpStmt is not used further
    }
}

void CodeGenerator::visit(IfStmtNode* node) {
    emitComment("IfStmt");
    std::string elseLabel = newLabel("if_else_");
    std::string endIfLabel = newLabel("if_end_");

    std::string condReg = visit(node->condition.get());
    
    if (node->elseStmt) {
        emit("beqz " + condReg + ", " + elseLabel); // Branch to else if condition is false
    } else {
        emit("beqz " + condReg + ", " + endIfLabel); // Branch to end if condition is false (no else)
    }
    freeTempReg(condReg);

    emitComment("IfStmt: Then block");
    visit(node->thenStmt.get());
    
    if (node->elseStmt) {
        emit("b " + endIfLabel); // Skip else block if then block was executed
        emitLabel(elseLabel);
        emitComment("IfStmt: Else block");
        visit(node->elseStmt.get());
    }
    
    emitLabel(endIfLabel);
    emitComment("IfStmt: End");
}

void CodeGenerator::visit(WhileStmtNode* node) {
    emitComment("WhileStmt");
    std::string startLabel = newLabel("while_start_");
    std::string bodyLabel = newLabel("while_body_"); // Optional, can just be after condition check
    std::string endLabel = newLabel("while_end_");

    loopStartLabels.push_back(startLabel); // For continue
    loopEndLabels.push_back(endLabel);     // For break

    emitLabel(startLabel);
    emitComment("WhileStmt: Condition check");
    std::string condReg = visit(node->condition.get());
    emit("beqz " + condReg + ", " + endLabel); // If condition false, exit loop
    freeTempReg(condReg);

    // emitLabel(bodyLabel); // If using a separate body label
    emitComment("WhileStmt: Body");
    visit(node->body.get());
    emit("b " + startLabel); // Jump back to condition check

    emitLabel(endLabel);
    emitComment("WhileStmt: End");

    loopStartLabels.pop_back();
    loopEndLabels.pop_back();
}

void CodeGenerator::visit(BreakStmtNode* node) {
    emitComment("BreakStmt");
    if (!loopEndLabels.empty()) {
        emit("b " + loopEndLabels.back());
    } else {
        emitComment("ERROR: 'break' not inside a loop (codegen).");
        // Semantic analysis should have caught this.
    }
}

void CodeGenerator::visit(ContinueStmtNode* node) {
    emitComment("ContinueStmt");
    if (!loopStartLabels.empty()) {
        emit("b " + loopStartLabels.back());
    } else {
        emitComment("ERROR: 'continue' not inside a loop (codegen).");
        // Semantic analysis should have caught this.
    }
}

void CodeGenerator::visit(ReturnStmtNode* node) {
    emitComment("ReturnStmt");
    // Semantic analysis should have verified type compatibility.
    // Here, we just generate code based on what's there.

    if (node->returnExp) {
        std::string valReg = visit(node->returnExp.get());
        emit("move $v0, " + valReg); // Standard MIPS return value register
        freeTempReg(valReg);
    }
    // Need to jump to the function's epilogue.
    // Assume a naming convention for epilogue label: funcName + "_epilogue"
    // This requires current function name. We don't have direct access to FuncDefNode here.
    // Let's assume SemanticAnalyzer stored current function's SymbolEntry or name.
    // Or, CodeGenerator needs its own `currentFuncSymbolEntry`.
    // For now, this is a simplification. A robust way is needed.
    // A simple approach: each function's epilogue code is generated at the end of its visit method.
    // So, a 'jr $ra' is part of that. This return just needs to set $v0 and then allow natural flow to epilogue,
    // or jump to a label *just before* the 'jr $ra' part of the epilogue if there's cleanup.
    // The emitFunctionEpilog already handles 'jr $ra'. So we need to branch to that.
    // Let's assume a label like "funcName_epilogue_actual_return"
    // This is getting complex. Simplest for now for SysY: after setting $v0, allow control to flow
    // to the end of the function body, where the epilogue sequence (including jr $ra) will be.
    // If return is in a nested block, it needs to jump past other blocks to the epilogue.
    // This implies a function-specific epilogue label is necessary.
    // Let's add a member `std::string currentFuncEpilogueLabel;` to CodeGenerator.
    // It's set in `visit(FuncDefNode*)`.
    if (!currentFuncEpilogueLabel.empty()) {
         emit("b " + currentFuncEpilogueLabel);
    } else {
        emitComment("ERROR: Could not find epilogue label for return.");
    }
}

void CodeGenerator::visit(PrintfStmtNode* node) {
    emitComment("PrintfStmt");
    // The format string from AST node->formatString is the raw string (e.g., "Hello %d World
")
    // It does NOT have escaped backslashes like in MIPS .asciiz
    // Our addStringLiteral and emitStringConstant handle MIPS escaping.

    std::string currentSegment = "";
    int argIdx = 0;

    for (size_t i = 0; i < node->formatString.length(); ++i) {
        if (node->formatString[i] == '%') {
            if (i + 1 < node->formatString.length() && node->formatString[i+1] == 'd') {
                // Print accumulated literal segment first
                if (!currentSegment.empty()) {
                    std::string strLabel = addStringLiteral(currentSegment);
                    emit("li $v0, 4");      // Syscall for print_string
                    emit("la $a0, " + strLabel);
                    emit("syscall");
                    currentSegment = "";
                }
                
                // Print %d argument
                if (argIdx < node->args.size()) {
                    std::string argReg = visit(node->args[argIdx].get());
                    emit("li $v0, 1");      // Syscall for print_int
                    emit("move $a0, " + argReg);
                    emit("syscall");
                    freeTempReg(argReg);
                    argIdx++;
                } else {
                    emitComment("ERROR: Not enough arguments for printf %d specifiers.");
                    // Semantic analyzer should catch this.
                }
                i++; // Skip 'd'
            } else {
                // Invalid format specifier (e.g., "%c" or just "%")
                // Semantic analyzer should ideally catch this.
                // For codegen, append to currentSegment as literal.
                currentSegment += node->formatString[i]; 
            }
        } else {
            currentSegment += node->formatString[i];
        }
    }

    // Print any remaining literal segment
    if (!currentSegment.empty()) {
        std::string strLabel = addStringLiteral(currentSegment);
        emit("li $v0, 4");
        emit("la $a0, " + strLabel);
        emit("syscall");
    }
    
    // SysY common practice: printf often implies a newline if not explicitly printing one.
    // The grammar notes 
 is the only escape. If printf needs a final newline always,
    // one could be added here, or rely on format strings to include it.
    // The current code prints exactly what format string + args specify.
}

std::string CodeGenerator::visit(ExpNode* node) { 
    if (!node) { emitComment("Error: Null ExpNode encountered."); return "$zero"; }
    if (auto n = dynamic_cast<NumberNode*>(node)) return visit(n);
    // Pass false for needsAddress by default when an LVal is used as an expression (RHS)
    if (auto n = dynamic_cast<LValNode*>(node)) return visit(n, false); 
    if (auto n = dynamic_cast<BinaryExpNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<UnaryExpNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<FuncCallNode*>(node)) return visit(n);
    if (auto n = dynamic_cast<GetIntNode*>(node)) return visit(n);
    emitComment("Warning: Unknown ExpNode type in CodeGenerator at line " + std::to_string(node->lineNumber));
    return "$zero"; 
}
std::string CodeGenerator::visit(NumberNode* node) { 
    std::string reg = getTempReg();
    emit("li " + reg + ", " + std::to_string(node->value));
    return reg;
}
// needsAddress: if true, load address of LVal into register; if false, load value.
std::string CodeGenerator::visit(LValNode* node, bool needsAddress /*= false*/) {
    emitComment("LVal: " + node->ident + (needsAddress ? " (addr)" : " (val)"));
    SymbolEntry* entry = symbolTable.lookupSymbol(node->ident);
    if (!entry) {
        // This error should have been caught by SemanticAnalyzer.
        // However, defensive coding in code generator is also good.
        emitComment("ERROR: Symbol '" + node->ident + "' not found during code gen.");
        std::string errReg = getTempReg();
        emit("li " + errReg + ", 0 # Error: LVal symbol not found");
        return errReg;
    }

    std::string baseAddrReg = getTempReg();

    if (entry->scopeLevel == 1) { // Global variable/array
        emit("la " + baseAddrReg + ", " + node->ident);
    } else { // Local variable, parameter, or array
        if (localVarOffsets.count(node->ident)) { // Local variable in current function
            emit("addi " + baseAddrReg + ", $fp, " + std::to_string(localVarOffsets[node->ident]));
        } else if (funcParamOffsets.count(node->ident)) { // Parameter in current function
            emit("addi " + baseAddrReg + ", $fp, " + std::to_string(funcParamOffsets[node->ident]));
        } else {
            emitComment("ERROR: Local symbol '" + node->ident + "' has no offset mapping.");
            emit("li " + baseAddrReg + ", 0 # Error: LVal offset not found");
            // No freeTempReg(baseAddrReg) here as it's the return in error case.
            return baseAddrReg;
        }
    }

    if (node->index) { // Array element access
        if (entry->type != DataType::ARRAY_INT) {
             emitComment("ERROR: Symbol '" + node->ident + "' is not an array but indexed.");
             // baseAddrReg might hold address of a non-array, using it is problematic.
             // Fall through to needsAddress check, but this is an error state.
        }
        std::string indexReg = visit(node->index.get()); // Index value in indexReg
        std::string offsetReg = getTempReg();
        
        emit("li " + offsetReg + ", 4");       // offsetReg = 4 (size of int)
        emit("mul " + offsetReg + ", " + indexReg + ", " + offsetReg); // offsetReg = index * 4
        freeTempReg(indexReg);
        
        emit("add " + baseAddrReg + ", " + baseAddrReg + ", " + offsetReg); // finalAddr = baseAddr + offset
        freeTempReg(offsetReg);
        // baseAddrReg now holds the address of the array element
    } else { // Simple variable or constant (if used as LVal, though consts shouldn't be LHS of assign)
        if (entry->type == DataType::ARRAY_INT) {
            // Using array name without index. If needsAddress, this is fine (passes array base address).
            // If needsValue, this is an error in SysY (cannot assign/use whole array as int).
            // Semantic analyzer should have caught use as value.
            // For codegen, if needsValue, this is problematic.
            if (!needsAddress) {
                emitComment("Warning: Array '" + node->ident + "' used as a value without index.");
                // What to load? For now, we'd load the first element's value if forced.
                // Or, this is an error. Semantic analysis should prevent this.
                // If this path is taken, baseAddrReg has base address. Loading from it would be arr[0].
            }
        }
    }

    if (needsAddress) {
        return baseAddrReg; // Return register holding the address
    } else {
        // Load value from the address in baseAddrReg
        std::string valueReg = getTempReg();
        emit("lw " + valueReg + ", 0(" + baseAddrReg + ")");
        freeTempReg(baseAddrReg);
        return valueReg;
    }
}


std::string CodeGenerator::visit(BinaryExpNode* node) {
    emitComment("BinaryExp: Op " + std::to_string(static_cast<int>(node->op)));

    // Special handling for logical AND and OR for short-circuiting
    if (node->op == BinOpType::AND) {
        std::string resultReg = getTempReg();
        std::string rightLabel = newLabel("land_right_");
        std::string endLabel = newLabel("land_end_");

        std::string leftReg = visit(node->left.get());
        emit("beqz " + leftReg + ", " + rightLabel); // If left is false (0), result is false (0)
        freeTempReg(leftReg); 
                                                // Left is true (non-zero)
        std::string rightReg = visit(node->right.get());
        emit("move " + resultReg + ", " + rightReg); // Result is value of right
        freeTempReg(rightReg);
        emit("b " + endLabel);

        emitLabel(rightLabel);
        emit("li " + resultReg + ", 0"); // Left was false, result is 0

        emitLabel(endLabel);
        return resultReg;

    } else if (node->op == BinOpType::OR) {
        std::string resultReg = getTempReg();
        std::string rightLabel = newLabel("lor_right_");
        std::string trueLabel = newLabel("lor_true_");
        std::string endLabel = newLabel("lor_end_");

        std::string leftReg = visit(node->left.get());
        emit("bnez " + leftReg + ", " + trueLabel); // If left is true (non-0), result is true (1)
        freeTempReg(leftReg);
                                                // Left is false (0)
        std::string rightReg = visit(node->right.get());
        emit("move " + resultReg + ", " + rightReg); // Result is value of right (0 or 1)
        // Ensure result is 0 or 1 if rightReg could be other non-zero.
        // For SysY, logical ops result in 0 or 1. If rightReg is already so, fine.
        // If rightReg could be any int, then: sne resultReg, rightReg, $zero
        emit("sne " + resultReg + ", " + resultReg + ", $zero"); // Ensure 0 or 1
        freeTempReg(rightReg);
        emit("b " + endLabel);

        emitLabel(trueLabel);
        emit("li " + resultReg + ", 1"); // Left was true, result is 1

        emitLabel(endLabel);
        return resultReg;
    }

    // Standard binary operations
    std::string leftReg = visit(node->left.get());
    std::string rightReg = visit(node->right.get());
    std::string resultReg = getTempReg();

    switch (node->op) {
        case BinOpType::ADD: emit("add " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::SUB: emit("sub " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::MUL: emit("mul " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::DIV: 
            emit("div " + leftReg + ", " + rightReg); // Quotient in $LO, Remainder in $HI
            emit("mflo " + resultReg); // Move quotient to resultReg
            break;
        case BinOpType::MOD:
            emit("div " + leftReg + ", " + rightReg); // Quotient in $LO, Remainder in $HI
            emit("mfhi " + resultReg); // Move remainder to resultReg
            break;
        case BinOpType::EQ:  emit("seq " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::NEQ: emit("sne " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::LT:  emit("slt " + resultReg + ", " + leftReg + ", " + rightReg); break;
        case BinOpType::GT:  emit("sgt " + resultReg + ", " + leftReg + ", " + rightReg); break; // MIPS pseudo-op (slt + sub or slt other way)
                                                                                             // Or: slt resultReg, rightReg, leftReg
        case BinOpType::LTE: emit("sle " + resultReg + ", " + leftReg + ", " + rightReg); break; // MIPS pseudo-op (sgt + xor or slt + xor)
                                                                                             // Or: sgt temp, leftReg, rightReg; xori resultReg, temp, 1
        case BinOpType::GTE: emit("sge " + resultReg + ", " + leftReg + ", " + rightReg); break; // MIPS pseudo-op
                                                                                             // Or: slt temp, leftReg, rightReg; xori resultReg, temp, 1
        default: emitComment("ERROR: Unknown binary operator"); break;
    }

    freeTempReg(leftReg);
    freeTempReg(rightReg);
    return resultReg;
}

std::string CodeGenerator::visit(UnaryExpNode* node) {
    emitComment("UnaryExp: Op " + std::to_string(static_cast<int>(node->op)));
    std::string operandReg = visit(node->operand.get());
    std::string resultReg = getTempReg(); // Usually need a new reg, unless op is PLUS

    switch (node->op) {
        case UnaryOpType::PLUS:
            // No operation needed, result is the operand itself.
            // But to ensure it's in a new temp reg as per convention:
            emit("move " + resultReg + ", " + operandReg);
            break;
        case UnaryOpType::MINUS:
            emit("neg " + resultReg + ", " + operandReg); // neg pseudo-op: sub result, $zero, operand
            break;
        case UnaryOpType::NOT:
            // !expr is true (1) if expr is 0, false (0) otherwise.
            emit("seq " + resultReg + ", " + operandReg + ", $zero");
            break;
        default:
            emitComment("ERROR: Unknown unary operator");
            emit("move " + resultReg + ", " + operandReg); // Default to move
            break;
    }
    
    if (resultReg != operandReg) { // Only free if a new register was used for the result.
         freeTempReg(operandReg);
    } else {
        // If resultReg IS operandReg (e.g. for UnaryPlus if we optimized to not move),
        // then the caller of UnaryExp is responsible for freeing resultReg.
        // Current impl always uses new resultReg for PLUS for consistency (or should).
        // If PLUS just returned operandReg, then this freeTempReg call would be conditional.
        // The current `move` for PLUS means operandReg can be freed.
         freeTempReg(operandReg);
    }
    return resultReg;
}

std::string CodeGenerator::visit(FuncCallNode* node) {
    emitComment("FuncCall: " + node->funcIdent);
    SymbolEntry* funcEntry = symbolTable.lookupSymbol(node->funcIdent);
    if (!funcEntry || funcEntry->kind != SymbolKind::FUNCTION) {
        emitComment("ERROR: Function '" + node->funcIdent + "' not found or not a function for call.");
        return "$v0"; // Default return reg, though behavior is undefined.
    }

    // 1. Evaluate arguments and push them onto stack (in reverse order for some conventions, or direct for MIPS)
    // MIPS: typically first 4 args in $a0-$a3, rest on stack.
    // SysY simplified: all args on stack. Let's push right to left for C-like order.
    // Or, push left to right, and callee accesses them in that order.
    // The chosen param access in FuncDefNode was $fp+8, $fp+12... (leftmost is $fp+8)
    // This means caller must push first arg first, then second, etc.
    // So, stack top before call has last arg.
    // Example: call f(a,b,c)
    //   push c (sw c, 0($sp); addi $sp, $sp, -4)
    //   push b (sw b, 0($sp); addi $sp, $sp, -4)
    //   push a (sw a, 0($sp); addi $sp, $sp, -4)
    //   jal f
    // After call, $sp needs to be restored by popping these args.

    int argStackSpace = node->args.size() * 4;

    // Save $ra, $fp are done by callee's prolog.
    // Caller might need to save its own $t registers if they are live across the call.
    // For simplicity, our `getTempReg` gives new regs; assume they are caller-saved if live.
    // This is a simplification. Proper handling involves saving live $t regs before call.

    // Push arguments
    for (int i = node->args.size() - 1; i >= 0; --i) {
        std::string argReg = visit(node->args[i].get());
        emit("sw " + argReg + ", 0($sp)");
        emit("addi $sp, $sp, -4");
        freeTempReg(argReg);
    }
    
    // 2. Call the function
    emit("jal " + node->funcIdent);

    // 3. Clean up stack (deallocate arguments)
    if (argStackSpace > 0) {
        emit("addi $sp, $sp, " + std::to_string(argStackSpace));
    }

    // 4. Return value is in $v0. Move to a temp register if needed by caller.
    // The convention is that the caller will use $v0 or move it.
    // If we return a temp reg, we must move $v0 to it.
    std::string resultReg = getTempReg();
    emit("move " + resultReg + ", $v0");
    return resultReg; 
}

std::string CodeGenerator::visit(GetIntNode* node) { 
    emitComment("GetInt");
    emit("li $v0, 5");      // Syscall code for read_int
    emit("syscall");        // Integer read is now in $v0
    
    std::string resultReg = getTempReg();
    emit("move " + resultReg + ", $v0"); // Move from $v0 to a temp reg
    return resultReg;
}
