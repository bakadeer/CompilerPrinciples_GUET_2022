module TokenPrinter
  ( printTokens,
  )
where

import Lexer (AlexPosn (..), SpannedToken (..))
import Token (Token (IdentTooLong, IntOverflow))
import qualified Token

printToken :: SpannedToken -> String
printToken (SpannedToken (AlexPn _ line column) token) =
  case token of
    Token.Const -> resWord "const"
    Token.Var -> resWord "var"
    Token.Procedure -> resWord "procedure"
    Token.Begin -> resWord "begin"
    Token.End -> resWord "end"
    Token.Odd -> resWord "odd"
    Token.If -> resWord "if"
    Token.Then -> resWord "then"
    Token.While -> resWord "while"
    Token.Do -> resWord "do"
    Token.Read -> resWord "read"
    Token.Write -> resWord "write"
    Token.Call -> resWord "call"
    Token.Integer num -> "(unsigned, " ++ show num ++ ")"
    Token.Identifier ident -> "(ident, " ++ show ident ++ ")"
    Token.LeftParen -> delimiter "("
    Token.RightParen -> delimiter ")"
    Token.Semicolon -> delimiter ";"
    Token.ColonEq -> operator ":="
    Token.Plus -> operator "+"
    Token.Minus -> operator "-"
    Token.Star -> operator "*"
    Token.Slash -> operator "/"
    Token.Eq -> operator "="
    Token.Number -> operator "#"
    Token.LessThan -> operator "<"
    Token.LessEqThan -> operator "<="
    Token.GreaterThan -> operator ">"
    Token.GreaterEqThan -> operator ">="
    Token.Comma -> delimiter ","
    Token.Dot -> delimiter "."
    Token.IllegalChar c -> "(IllegalChar, " ++ show c ++ ") " ++ "at line: " ++ show line ++ ", column: " ++ show column
    Token.IdentTooLong ident -> "(IdentTooLong, " ++ show ident ++ ") " ++ "at line: " ++ show line ++ ", column: " ++ show column
    Token.IntOverflow num -> "(IntOverflow, " ++ show num ++ ") " ++ "at line: " ++ show line ++ ", column: " ++ show column

resWord :: String -> String
resWord s = "(Reserved, " ++ s ++ ")"

delimiter :: String -> String
delimiter s = "(Delimiter, " ++ s ++ ")"

operator :: String -> String
operator s = "(Operator, " ++ s ++ ")"

printTokens :: [SpannedToken] -> [String]
printTokens tokens = map printToken tokens
