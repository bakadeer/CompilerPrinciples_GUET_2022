module TokenPrinter
  ( printTokens,
    isError,
  )
where

import Data.Char (isAlphaNum)
import Lexer (AlexPosn (..), SpannedToken (..))
import Token (Token (..))

-- Helper functions for formatting
resWord :: String -> String
resWord s = "(Reserved, " ++ s ++ ")"

delimiter :: String -> String
delimiter s = "(Delimiter, " ++ s ++ ")"

operator :: String -> String
operator s = "(Operator, " ++ s ++ ")"

-- Check if identifier is valid (alphanumeric, underscore, max length 32)
isValidIdentifier :: String -> Bool
isValidIdentifier ident =
  length ident <= 8 && all (\c -> isAlphaNum c || c == '_') ident

-- Check if integer is within 16-bit signed range (-32768 to 32767)
isValidInteger :: Int -> Bool
isValidInteger n = n >= -32768 && n <= 32767

-- Print a single token with position information for errors
printToken :: SpannedToken -> String
printToken (SpannedToken (AlexPn _ line column) token) =
  case token of
    Const -> resWord "const"
    Var -> resWord "var"
    Procedure -> resWord "procedure"
    Begin -> resWord "begin"
    End -> resWord "end"
    Odd -> resWord "odd"
    If -> resWord "if"
    Then -> resWord "then"
    While -> resWord "while"
    Do -> resWord "do"
    Read -> resWord "read"
    Write -> resWord "write"
    Call -> resWord "call"
    Integer num ->
      if isValidInteger num
        then "(unsigned, " ++ show num ++ ")"
        else "(IntOverflow, " ++ show num ++ ") at line: " ++ show line ++ ", column: " ++ show column
    Identifier ident ->
      if isValidIdentifier ident
        then "(ident, " ++ show ident ++ ")"
        else "(IdentTooLong or SymbolError, " ++ show ident ++ ") at line: " ++ show line ++ ", column: " ++ show column
    LeftParen -> delimiter "("
    RightParen -> delimiter ")"
    Semicolon -> delimiter ";"
    ColonEq -> operator ":="
    Plus -> operator "+"
    Minus -> operator "-"
    Star -> operator "*"
    Slash -> operator "/"
    Eq -> operator "="
    Number -> operator "#"
    LessThan -> operator "<"
    LessEqThan -> operator "<="
    GreaterThan -> operator ">"
    GreaterEqThan -> operator ">="
    Comma -> delimiter ","
    Dot -> delimiter "."

-- Print all tokens
printTokens :: [SpannedToken] -> [String]
printTokens tokens = map printToken tokens

-- Check if any token is erroneous
isError :: [SpannedToken] -> Bool
isError tokens = any hasError tokens
  where
    hasError (SpannedToken _ token) =
      case token of
        Integer num -> not $ isValidInteger num
        Identifier ident -> not $ isValidIdentifier ident
        _ -> False
