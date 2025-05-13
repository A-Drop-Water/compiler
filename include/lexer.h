#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>
#include <unordered_map>
#include "token.h"

/**
 * @brief 词法分析器类，用于将输入代码字符串拆分成token序列
 * 
 * 此类实现了词法分析功能，能够识别语言中的关键字、标识符、
 * 整数常量、字符串常量和运算符等词法单元，同时可以跳过注释和空白符
 */
class Lexer
{
public:
  /**
   * @brief 构造词法分析器
   * @param code 需要进行词法分析的源代码字符串
   */
  Lexer(const std::string &code);

  /**
   * @brief 执行词法分析，将源代码转换为token序列
   * @return 包含所有识别出的token的向量
   */
  std::vector<Token> tokenize();

private:
  std::string input;  // 输入的源代码字符串
  size_t pos;         // 当前处理位置的下标
  size_t line;        // 当前处理的行号，用于错误定位
  std::unordered_map<std::string, TokenType> keywords;  // 关键字映射表

  /**
   * @brief 判断当前位置是否为注释开始
   * @return 如果是注释(//或/*)则返回true，否则返回false
   */
  bool isZhushi();

  /**
   * @brief 处理空白符（空格、制表符、换行符等）
   */
  void handleWhitespace();

  /**
   * @brief 解析标识符或关键字
   * @return 解析得到的标识符或关键字token
   */
  Token parseIdentifierOrKeyword();

  /**
   * @brief 解析整数常量
   * @return 解析得到的整数token
   */
  Token parseInteger();

  /**
   * @brief 跳过注释内容（单行注释//或多行注释）
   */
  void zhushi();

  /**
   * @brief 解析字符串常量
   * @return 解析得到的字符串token
   */
  Token parseString();

  /**
   * @brief 解析符号（运算符、界符等）
   * @return 解析得到的符号token
   */
  Token parseSymbol();
};

#endif
