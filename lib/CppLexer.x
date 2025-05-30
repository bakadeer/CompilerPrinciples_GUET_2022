{
module CppLexer (main) where

import System.IO
}

%wrapper "basic"

$letter = [a-zA-Z]           -- 字母
$digit  = [0-9]              -- 数字
$symbol = [\;\(\)\{\}\=\<\>\"] -- 符号（根据输入定义）
-- $white  = [\ \t\n\r]         -- 空白字符：空格、制表符、换行符（LF 和 CR）

tokens :-

  $white+                  ;                             -- 忽略所有空白字符
  $letter+                 { \s -> ("TokenWord", s) }          -- 匹配单词
  $digit+                  { \s -> ("TokenNum", s) }          -- 匹配数字
  $symbol                  { \s -> ("TokenSym", s) }  -- 匹配单个符号
  .                        ;

{
-- 主函数：读取输入并处理词法分析
main :: IO ()
main = do
  input <- (readFile "test/cpplexer.cpp")
  let tokens = alexScanTokens input
  mapM_ (\(t, s) -> putStrLn (t ++ ": " ++ s)) tokens
}
