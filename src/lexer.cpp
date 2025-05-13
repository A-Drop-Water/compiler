#include "lexer.h"
#include <cctype>

Lexer::Lexer(const std::string &code) : input(code), pos(0), line(1)
{
  // 初始化关键字集合
  keywords = {
      {"main", TokenType::MAINTK},
      {"const", TokenType::CONSTTK},
      {"int", TokenType::INTTK},
      {"if", TokenType::IFTK},
      {"break", TokenType::BREAKTK},
      {"else", TokenType::ELSETK},
      {"continue", TokenType::CONTINUETK},
      {"while", TokenType::WHILETK},
      {"void", TokenType::VOIDTK},
      {"getint", TokenType::GETINTTK},
      {"printf", TokenType::PRINTFTK},
      {"return", TokenType::RETURNTK},
  };
}

std::vector<Token> Lexer::tokenize()
{
  std::vector<Token> tokens;
  while (pos < input.size())
  {
    char current = input[pos];
    if (std::isspace(current))
    {
      handleWhitespace(); // 处理空格和换行
    }
    else if (std::isalpha(current) || current == '_')
    {
      tokens.push_back(parseIdentifierOrKeyword());
    }
    else if (std::isdigit(current))
    {
      tokens.push_back(parseInteger());
      // pos++;
    }
    else if (current == '"')
    {
      tokens.push_back(parseString());
    }
    else if (isZhushi())
    {
      // 是注释跳过注释
      zhushi();
    }
    else
    {
      tokens.push_back(parseSymbol());
      // pos++;
    }
  }
  return tokens;
}

bool Lexer::isZhushi()
{
  return input[pos] == '/' && (pos + 1 < input.size()) && (input[pos + 1] == '/' || input[pos + 1] == '*');
}

// 辅助函数：处理空白符（包括换行）
void Lexer::handleWhitespace()
{
  while (pos < input.size() && std::isspace(input[pos]))
  {
    if (input[pos] == '\n')
      line++;
    pos++;
  }
}

// 解析标识符或关键字
Token Lexer::parseIdentifierOrKeyword()
{
  size_t start = pos;
  while (pos < input.size() && (std::isalnum(input[pos]) || input[pos] == '_'))
  {
    pos++;
  }
  std::string value = input.substr(start, pos - start);
  if (keywords.find(value) != keywords.end())
  {
    return Token(keywords[value], value, line);
  }
  return Token(TokenType::IDENFR, value, line);
}

// 解析整数
Token Lexer::parseInteger()
{
  size_t start = pos;
  while (pos < input.size() && std::isdigit(input[pos]))
  {
    pos++;
  }
  return Token(TokenType::INTCON, input.substr(start, pos - start), line);
}

// 跳过注释
void Lexer::zhushi()
{
  pos += 2; // 跳过 '//'或'/*'
  
  // 如果是 // 跳到下一行
  if (input[pos-1] == '/')
  {
    while (pos < input.size() && input[pos] != '\n')
      pos++;
    // 此时pos执行到\n或文件末尾
  }
  else // 如果是 /* 找到对应的 */
  {
    bool found = false;
    while (pos + 1 < input.size() && !found) 
    {
      if (input[pos] == '*' && input[pos + 1] == '/') {
        found = true;
        pos += 2; // 跳过 */
      } else {
        if (input[pos] == '\n') {
          line++;
        }
        pos++;
      }
    }
    
    // 如果没找到注释结束符，移动到文件末尾
    if (!found && pos < input.size()) {
      pos = input.size();
    }
  }
}

// 解析字符串
Token Lexer::parseString()
{
  pos++; // 跳过起始的 "
  size_t start = pos;
  while (pos < input.size() && input[pos] != '"')
  {
    if (input[pos] == '\n')
      line++; // 处理字符串内的换行
    pos++;
  }
  if (pos >= input.size())
  {
    return Token(TokenType::ERROR, "Unterminated string", line);
  }
  std::string value = input.substr(start, pos - start);
  pos++; // 跳过结束的 "
  return Token(TokenType::STRCON, "\"" + value + "\"", line);
}

// 解析符号（运算符、界符）
Token Lexer::parseSymbol()
{
  char current = input[pos];
  pos++;
  switch (current)
  {
  case '|':
    if (pos < input.size() && input[pos] == '|')
    {
      pos++;
      return Token(TokenType::OR, "||", line);
    }
    return Token(TokenType::ERROR, "别管了", line);
  case '&':
    if (pos < input.size() && input[pos] == '&')
    {
      pos++;
      return Token(TokenType::AND, "&&", line);
    }
    return Token(TokenType::ERROR, "别管了", line);
  case '<':
    if (pos < input.size() && input[pos] == '=')
    {
      pos++;
      return Token(TokenType::LEQ, "<=", line);
    }
    return Token(TokenType::LSS, "<", line);
  case '>':
    if (pos < input.size() && input[pos] == '=')
    {
      pos++;
      return Token(TokenType::GEQ, ">=", line);
    }
    return Token(TokenType::GRE, ">", line);
  case '=':
    if (pos < input.size() && input[pos] == '=')
    {
      pos++;
      return Token(TokenType::EQL, "==", line);
    }
    return Token(TokenType::ASSIGN, "=", line);
  case '!':
    if (pos < input.size() && input[pos] == '=')
    {
      pos++;
      return Token(TokenType::NEQ, "!=", line);
    }
    return Token(TokenType::NOT, "!", line);
  case '+':
    return Token(TokenType::PLUS, "+", line);
  case '-':
    return Token(TokenType::MINU, "-", line);
  case '*':
    return Token(TokenType::MULT, "*", line);
  case '/':
    // 逆天，还要考虑注释
    return Token(TokenType::DIV, "/", line);
  case '%':
    return Token(TokenType::MOD, "%", line);
  case '[':
    return Token(TokenType::LBRACK, "[", line);
  case ']':
    return Token(TokenType::RBRACK, "]", line);
  case '{':
    return Token(TokenType::LBRACE, "{", line);
  case '}':
    return Token(TokenType::RBRACE, "}", line);
  case ';':
    return Token(TokenType::SEMICN, ";", line);
  case ',':
    return Token(TokenType::COMMA, ",", line);
  case '(':
    return Token(TokenType::LPARENT, "(", line);
  case ')':
    return Token(TokenType::RPARENT, ")", line);
  default:
    return Token(TokenType::ERROR, std::string(1, current), line);
  }
}
