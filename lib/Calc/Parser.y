{
module Calc.Parser where
import Calc.Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  num      { TokenNum $$ }
  '+'      { TokenPlus }
  '-'      { TokenMinus }
  '*'      { TokenMul }
  '/'      { TokenDiv }
  '('      { TokenLParen }
  ')'      { TokenRParen }

%left '+' '-'
%left '*' '/'
%right NEG

%%

Expr
  : num                { Num $1 }
  | Expr '+' Expr      { Add $1 $3 }
  | Expr '-' Expr      { Sub $1 $3 }
  | Expr '*' Expr      { Mul $1 $3 }
  | Expr '/' Expr      { Div $1 $3 }
  | '-' Expr %prec NEG { Neg $2 }
  | '(' Expr ')'       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show, Eq)
}
