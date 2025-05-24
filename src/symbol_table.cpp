#include "include/symbol_table.h" // Adjust path as needed
#include <iostream> // For potential error messages

SymbolTable::SymbolTable() : currentScopeLevel(0) {
    // Initialize with a global scope
    enterScope(); 
}

void SymbolTable::enterScope() {
    currentScopeLevel++;
    scopeStack.push_front(std::unordered_map<std::string, SymbolEntry>()); // Add a new scope (map) to the front (top of stack)
}

void SymbolTable::exitScope() {
    if (!scopeStack.empty()) {
        scopeStack.pop_front(); // Remove the current scope from the front
    }
    if (currentScopeLevel > 0) { // Ensure scope level doesn't go below zero (global scope is 1 after first enterScope)
                                 // Or, if global scope is 0, then it should not go negative.
                                 // With currentScopeLevel starting at 0 and incrementing *before* pushing,
                                 // global scope is level 1.
        currentScopeLevel--;
    }
    if (scopeStack.empty() && currentScopeLevel !=0 ) {
        // This state should ideally not be reached if scopes are managed correctly.
        // Re-initialize global scope if stack becomes empty but level is not 0.
        // Or, ensure exitScope() is not called on global scope unless program ends.
        // For robustness, if stack is empty, reset to a valid global scope state.
        // currentScopeLevel = 0;
        // enterScope(); // This would make currentScopeLevel 1 again.
        // Simpler: exitScope should not leave currentScopeLevel at 0 if stack becomes empty,
        // unless it's truly the end of all scopes.
        // The logic in constructor (starts at 0, then enterScope makes it 1) means level 1 is global.
        // So, currentScopeLevel should not be less than 1.
        if (currentScopeLevel == 0 && !scopeStack.empty()) {
            // Error state: scope level is 0 but stack is not empty
        } else if (currentScopeLevel > 0 && scopeStack.empty()) {
            // Error state: scope level > 0 but stack is empty
            // This implies an imbalance.
            // For now, assume calls are balanced.
        }
    }
     if (scopeStack.empty()) { // If all scopes are exited, reset to initial state to allow re-entry (e.g. for multiple files)
        currentScopeLevel = 0;
        enterScope(); // Re-establish a global scope
    }
}

// Adds a symbol to the current scope.
// Returns true on success, false if symbol already declared in current scope.
bool SymbolTable::addSymbol(const SymbolEntry& entry) {
    if (scopeStack.empty()) {
        // Should not happen if constructor initializes a scope
        std::cerr << "SymbolTable Error: No current scope to add symbol '" << entry.name << "'." << std::endl;
        return false; 
    }

    std::unordered_map<std::string, SymbolEntry>& currentScope = scopeStack.front();
    if (currentScope.count(entry.name)) {
        // Symbol already exists in the current scope
        // Error reporting for redefinition should be handled by the caller (semantic analyzer)
        // using information like entry.declarationLine and existing symbol's line.
        return false; 
    }
    currentScope[entry.name] = entry;
    // Update the entry's scope level just to be sure, though it should be set by caller
    // currentScope[entry.name].scopeLevel = currentScopeLevel; 
    return true;
}

// Looks up a symbol by name, starting from current scope and going outwards.
// Returns pointer to entry if found, nullptr otherwise.
SymbolEntry* SymbolTable::lookupSymbol(const std::string& name) {
    if (name.empty()) return nullptr;

    for (auto& scope : scopeStack) { // Iterates from current (front) to outer (back)
        auto it = scope.find(name);
        if (it != scope.end()) {
            return &(it->second); // Return pointer to the found entry
        }
    }
    return nullptr; // Symbol not found in any scope
}

// Looks up a symbol only in the current scope.
// Returns pointer to entry if found, nullptr otherwise.
SymbolEntry* SymbolTable::lookupSymbolCurrentScope(const std::string& name) {
    if (name.empty() || scopeStack.empty()) {
        return nullptr;
    }

    std::unordered_map<std::string, SymbolEntry>& currentScope = scopeStack.front();
    auto it = currentScope.find(name);
    if (it != currentScope.end()) {
        return &(it->second); // Return pointer to the found entry
    }
    return nullptr; // Symbol not found in current scope
}
