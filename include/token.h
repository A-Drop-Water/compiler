#ifndef TOKEN_H
#define TOKEN_H

#include <string>

// Token
enum class TokenType
{
  ERROR,  // 别管了
  IDENFR, // 变量名称
  INTCON, // 常数 10进制 ?
  STRCON, // "xxxx" 这样的字符串
  // 关键字
  MAINTK,     // main 关键字
  CONSTTK,    // const 关键字
  INTTK,      // int
  BREAKTK,    // break
  CONTINUETK, // continue
  IFTK,       // if
  ELSETK,     // else
  WHILETK,    // while
  GETINTTK,   // getint
  PRINTFTK,   // printf
  RETURNTK,   // return
  VOIDTK,     // void
  // 界符、运算符
  NOT, // !
  AND, // &&
  OR,  // ||

  PLUS, // +
  MINU, // -
  MULT, // *
  DIV,  //  /
  MOD,  // %

  ASSIGN, // =
  EQL,    // ==
  NEQ,    // !=
  LSS,    // <
  LEQ,    // <=
  GRE,    // >
  GEQ,    // >=

  SEMICN,  // ;
  COMMA,   // ,
  LPARENT, // (
  RPARENT, // )
  LBRACK,  // [
  RBRACK,  // ]
  LBRACE,  // {
  RBRACE,  // }
};

struct Token
{
  TokenType type;
  std::string value;
  size_t line;

  Token(TokenType t, const std::string &v, size_t l) : type(t), value(v), line(l) {}
};

#endif