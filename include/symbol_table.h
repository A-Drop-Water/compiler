#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <string>
#include <vector>
#include <list> // For stack of scopes
#include <unordered_map>
#include <memory> // For shared_ptr or unique_ptr if needed for types

// Forward declaration if Type information becomes complex
// struct Type; 

enum class SymbolKind {
    VARIABLE,
    CONSTANT,
    FUNCTION,
    PARAMETER // Formal parameter of a function
};

// Basic type representation (SysY only has int and int arrays)
enum class DataType {
    INT,
    VOID, // For function return types
    ARRAY_INT // For int arrays
    // Add more types if language expands
};

struct SymbolEntry {
    std::string name;
    SymbolKind kind;
    DataType type; // The type of the variable/constant, or return type for function
    int scopeLevel;
    int declarationLine;

    // For constants: store its value
    int constantValue; // Only if kind == CONSTANT and type == INT

    // For arrays: store dimension information
    // For SysY, 1D arrays. Store size of dimension if known.
    // If `ConstExp`s are evaluated, this could be actual sizes.
    std::vector<int> arrayDimensions; // e.g., for arr[10][20], stores {10, 20}

    // For functions: store parameter types and order
    std::vector<DataType> paramTypes; // Store type of each parameter
    // We might also need more info like param names for full signature

    SymbolEntry(std::string n, SymbolKind k, DataType t, int sl, int dl)
        : name(n), kind(k), type(t), scopeLevel(sl), declarationLine(dl), constantValue(0) {}
};

class SymbolTable {
public:
    SymbolTable();

    void enterScope();
    void exitScope();

    // Adds a symbol to the current scope.
    // Returns true on success, false if symbol already declared in current scope.
    bool addSymbol(const SymbolEntry& entry);

    // Looks up a symbol by name, starting from current scope and going outwards.
    // Returns pointer to entry if found, nullptr otherwise.
    SymbolEntry* lookupSymbol(const std::string& name);
    
    // Looks up a symbol only in the current scope.
    // Returns pointer to entry if found, nullptr otherwise.
    SymbolEntry* lookupSymbolCurrentScope(const std::string& name);

    int getCurrentScopeLevel() const { return currentScopeLevel; }

private:
    // A list of hash maps, representing a stack of scopes.
    // Each map stores symbol name to SymbolEntry for that scope.
    std::list<std::unordered_map<std::string, SymbolEntry>> scopeStack;
    int currentScopeLevel;
};

#endif // SYMBOL_TABLE_H
