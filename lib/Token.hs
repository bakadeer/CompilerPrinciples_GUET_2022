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
  | Integer Int
  | Identifier String
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
  | IllegalChar String  -- For invalid identifiers or operators
  | IdentTooLong String  -- For identifiers exceeding length limit
  | IntOverflow String  -- For integers exceeding max value
  deriving (Eq, Show)
