{
module Calc.Parser (parseCalc) where
import Calc.Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  num       { TokenNum $$ }
  '+'       { TokenPlus }
  '*'       { TokenMult }

%right '+' -- 加法优先级最低
%left '*'  -- 乘法优先级高于加法

%%

Expr : num                { $1 }          -- 单个数字
     | Expr '+' Expr      { $1 + $3 }     -- 加法
     | Expr '*' Expr      { $1 * $3 }     -- 乘法

{
-- 解析错误处理
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- 解析函数，供外部调用
parseCalc :: String -> Int
parseCalc input = calc (alexScanTokens input)
}
