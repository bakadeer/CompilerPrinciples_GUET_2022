module Token where

data Token
  = Const
  | Var
  | Procedure
  | Begin
  | End
  | Odd
  | If
  | Then
  | While
  | Do
  | Read
  | Write
  | Call
  | Integer       Int
  | Identifier    String
  | LeftParen
  | RightParen
  | Semicolon
  | ColonEq
  | Plus
  | Minus
  | Star
  | Slash
  | Eq
  | Number
  | LessThan
  | LessEqThan
  | GreaterThan
  | GreaterEqThan
  | Comma
  | Dot
  deriving (Eq, Show)
