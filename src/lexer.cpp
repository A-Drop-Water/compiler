#include "lexer.h"
#include "token.h" 
#include <iostream> 
#include <cctype>   
#include <unordered_map>
#include <variant> // Required for std::get, std::holds_alternative

// Keyword map
static std::unordered_map<std::string, TokenType> keywords = {
    {"const", TokenType::KEYWORD_CONST},
    {"int", TokenType::KEYWORD_INT},
    {"void", TokenType::KEYWORD_VOID},
    {"main", TokenType::KEYWORD_MAIN},
    {"if", TokenType::KEYWORD_IF},
    {"else", TokenType::KEYWORD_ELSE},
    {"while", TokenType::KEYWORD_WHILE},
    {"break", TokenType::KEYWORD_BREAK},
    {"continue", TokenType::KEYWORD_CONTINUE},
    {"return", TokenType::KEYWORD_RETURN},
    {"getint", TokenType::KEYWORD_GETINT},
    {"printf", TokenType::KEYWORD_PRINTF}
};

Lexer::Lexer(const std::string& filename) : line(1), column(0), currentLineContent("") {
    sourceFile.open(filename);
    if (!sourceFile.is_open()) {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        currentLineContent = ""; 
        sourceFile.setstate(std::ios_base::badbit); 
    } else {
        loadNextLine(); 
    }
}

void Lexer::loadNextLine() {
    if (std::getline(sourceFile, currentLineContent)) {
        currentLineContent += '\n'; 
        column = 0; 
    } else {
        currentLineContent = ""; 
    }
}

char Lexer::currentChar() {
    if (column >= currentLineContent.length()) { 
        if (!sourceFile.is_open() || sourceFile.bad()) {
             return '\0';
        }
        loadNextLine(); 
        if (currentLineContent.empty()) { 
            return '\0';
        }
    }
    return currentLineContent[column];
}

void Lexer::advance() {
    if (column < currentLineContent.length()) {
        if (currentLineContent[column] == '\n') {
            line++; 
            column = currentLineContent.length(); 
        } else {
            column++;
        }
    } else {
        loadNextLine(); 
    }
}

void Lexer::skipWhitespace() {
    while (true) {
        char c = currentChar();
        if (c == '\0' && currentLineContent.empty()) break;

        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else if (c == '\n') {
            advance(); 
        } else {
            break;
        }
    }
}

void Lexer::consumeSingleLineComment() {
    while (currentChar() != '\n' && currentChar() != '\0') {
        advance(); 
    }
    if (currentChar() == '\n') {
        advance(); 
    }
}

void Lexer::consumeMultiLineComment() {
    int startLine = line; 
    char prev_c = '\0';
    while (true) {
        char c = currentChar();
        if (c == '\0') {
            std::cerr << "Error: Unterminated multi-line comment starting at line " << startLine << std::endl;
            break;
        }
        if (prev_c == '*' && c == '/') {
            advance(); 
            break;
        }
        prev_c = c;
        advance(); 
    }
}

Token Lexer::identifier() {
    std::string identStr;
    int tokenLine = line; 
    identStr += currentChar();
    advance();

    while (std::isalnum(currentChar()) || currentChar() == '_') {
        identStr += currentChar();
        advance();
    }

    auto it = keywords.find(identStr);
    if (it != keywords.end()) {
        return Token(it->second, tokenLine);
    }
    return Token(TokenType::IDENTIFIER, identStr, tokenLine);
}

Token Lexer::number() {
    std::string numStr;
    int tokenLine = line;
    numStr += currentChar();
    advance();

    while (std::isdigit(currentChar())) {
        numStr += currentChar();
        advance();
    }
    return Token(TokenType::INT_CONST, std::stoi(numStr), tokenLine);
}

Token Lexer::formatString() {
    std::string strVal;
    int tokenLine = line; 
    advance(); // Consume opening "

    while (true) {
        char c = currentChar();
        if (c == '"') { 
            advance(); 
            break;
        }
        if (c == '\0' || c == '\n') { 
            std::cerr << "Error: Unterminated or invalid (contains newline) string literal at line " << tokenLine << std::endl;
            return Token(TokenType::UNKNOWN, strVal, tokenLine); 
        }

        if (c == '\\') { 
            advance(); 
            char escaped_char = currentChar();
            if (escaped_char == 'n') {
                strVal += '\n'; 
                advance();
            } else {
                std::cerr << "Error: Invalid escape sequence '\\" << escaped_char << "' in string literal at line " << tokenLine << std::endl;
                strVal += '\\'; 
                strVal += escaped_char;
                advance();
            }
        } else {
            strVal += c;
            advance();
        }
    }
    return Token(TokenType::FORMAT_STRING, strVal, tokenLine);
}

Token Lexer::getNextToken() {
    while (true) { 
        skipWhitespace(); 
        int tokenLine = line; 
        char c = currentChar();

        if (c == '\0' && currentLineContent.empty() && (sourceFile.eof() || !sourceFile.is_open() || sourceFile.bad())) {
            return Token(TokenType::END_OF_FILE, tokenLine);
        }
        if (c == '\0') { // Catch-all for unexpected EOF or error states
             if (currentLineContent.empty() && (sourceFile.eof() || !sourceFile.is_open() || sourceFile.bad())) {
                 return Token(TokenType::END_OF_FILE, tokenLine);
             }
        }

        if (c == '/') {
            char next_c = (column + 1 < currentLineContent.length()) ? currentLineContent[column+1] : '\0';
            if (next_c == '/') { 
                advance(); 
                advance(); 
                consumeSingleLineComment();
                continue; 
            } else if (next_c == '*') { 
                advance(); 
                advance(); 
                consumeMultiLineComment();
                continue; 
            }
        }
        
        if (std::isalpha(c) || c == '_') {
            return identifier(); 
        }

        if (std::isdigit(c)) {
            return number(); 
        }

        if (c == '"') {
            return formatString(); 
        }

        advance(); 
        switch (c) {
            case '+': return Token(TokenType::OP_PLUS, tokenLine);
            case '-': return Token(TokenType::OP_MINUS, tokenLine);
            case '*': return Token(TokenType::OP_MULTIPLY, tokenLine);
            case '/': return Token(TokenType::OP_DIVIDE, tokenLine); 
            case '%': return Token(TokenType::OP_MODULO, tokenLine);
            case '=':
                if (currentChar() == '=') {
                    advance(); return Token(TokenType::OP_EQ, tokenLine);
                }
                return Token(TokenType::OP_ASSIGN, tokenLine);
            case '!':
                if (currentChar() == '=') {
                    advance(); return Token(TokenType::OP_NEQ, tokenLine);
                }
                return Token(TokenType::OP_NOT, tokenLine);
            case '<':
                if (currentChar() == '=') {
                    advance(); return Token(TokenType::OP_LTE, tokenLine);
                }
                return Token(TokenType::OP_LT, tokenLine);
            case '>':
                if (currentChar() == '=') {
                    advance(); return Token(TokenType::OP_GTE, tokenLine);
                }
                return Token(TokenType::OP_GT, tokenLine);
            case '&':
                if (currentChar() == '&') {
                    advance(); return Token(TokenType::OP_AND, tokenLine);
                }
                return Token(TokenType::UNKNOWN, std::string(1, c), tokenLine);
            case '|':
                if (currentChar() == '|') {
                    advance(); return Token(TokenType::OP_OR, tokenLine);
                }
                return Token(TokenType::UNKNOWN, std::string(1, c), tokenLine);
            case ';': return Token(TokenType::PUNC_SEMICOLON, tokenLine);
            case ',': return Token(TokenType::PUNC_COMMA, tokenLine);
            case '(': return Token(TokenType::PUNC_LPAREN, tokenLine);
            case ')': return Token(TokenType::PUNC_RPAREN, tokenLine);
            case '[': return Token(TokenType::PUNC_LBRACKET, tokenLine);
            case ']': return Token(TokenType::PUNC_RBRACKET, tokenLine);
            case '{': return Token(TokenType::PUNC_LBRACE, tokenLine);
            case '}': return Token(TokenType::PUNC_RBRACE, tokenLine);
            default:
                if (c == '\0') return Token(TokenType::END_OF_FILE, tokenLine); // Should be caught earlier
                return Token(TokenType::UNKNOWN, std::string(1, c), tokenLine);
        }
    }
}

std::vector<Token> Lexer::getAllTokens() {
    std::vector<Token> tokens;
    if (!sourceFile.is_open() || sourceFile.bad()) { 
        std::cerr << "Lexer Error: Source file is not open or in a bad state." << std::endl;
        tokens.push_back(Token(TokenType::END_OF_FILE, line)); 
        return tokens;
    }
    while (true) {
        Token token = getNextToken();
        tokens.push_back(token);
        if (token.type == TokenType::END_OF_FILE) {
            break;
        }
        if (token.type == TokenType::UNKNOWN) {
             if (std::holds_alternative<std::string>(token.value)) {
                std::cerr << "Lexer Warning: Unknown token '" << std::get<std::string>(token.value)
                          << "' at line " << token.line << std::endl;
            } else {
                 std::cerr << "Lexer Warning: Unknown token (type: " << static_cast<int>(token.type) 
                           << ") at line " << token.line << std::endl;
            }
        }
    }
    return tokens;
}
