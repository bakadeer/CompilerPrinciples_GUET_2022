{
module Lexer (
    spanOf,
    unwrap,
    SpannedToken(..),
    alexScanTokens,
    AlexPosn(..)
) where

import Token
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [$alpha $digit \_]

tokens :-

    $white+                                 ;  -- Skip whitespace
    "//".*                                  ;  -- Single-line comment
    "/*"([^\*]|\*[^\/])*"\*/"              ;  -- Multi-line comment
    const                                   { \p _ -> SpannedToken p Const }
    var                                     { \p _ -> SpannedToken p Var }
    procedure                               { \p _ -> SpannedToken p Procedure }
    begin                                   { \p _ -> SpannedToken p Begin }
    end                                     { \p _ -> SpannedToken p End }
    odd                                     { \p _ -> SpannedToken p Odd }
    if                                      { \p _ -> SpannedToken p If }
    then                                    { \p _ -> SpannedToken p Then }
    while                                   { \p _ -> SpannedToken p While }
    do                                      { \p _ -> SpannedToken p Do }
    call                                    { \p _ -> SpannedToken p Call }
    read                                    { \p _ -> SpannedToken p Read }
    write                                   { \p _ -> SpannedToken p Write }
    $digit+                                 { \p s -> makeInteger p s }
    $alpha $alnum*                          { \p s -> makeIdentifier p s }
    [$alnum $digit]+                        { \p s -> SpannedToken p (IllegalChar s) }
    "+"                                     { \p _ -> SpannedToken p Plus }
    "-"                                     { \p _ -> SpannedToken p Minus }
    "*"                                     { \p _ -> SpannedToken p Star }
    "/"                                     { \p _ -> SpannedToken p Slash }
    ":="                                    { \p _ -> SpannedToken p ColonEq }
    "="                                     { \p _ -> SpannedToken p Eq }
    "<"                                     { \p _ -> SpannedToken p LessThan }
    "<="                                    { \p _ -> SpannedToken p LessEqThan }
    "#"                                     { \p _ -> SpannedToken p Number }
    ">"                                     { \p _ -> SpannedToken p GreaterThan }
    ">="                                    { \p _ -> SpannedToken p GreaterEqThan }
    ","                                     { \p _ -> SpannedToken p Comma }
    ";"                                     { \p _ -> SpannedToken p Semicolon }
    "."                                     { \p _ -> SpannedToken p Dot }
    "("                                     { \p _ -> SpannedToken p LeftParen }
    ")"                                     { \p _ -> SpannedToken p RightParen }
    .                                       { \p s -> SpannedToken p (IllegalChar s) }

{
data SpannedToken = SpannedToken AlexPosn Token deriving (Show, Eq)

spanOf :: SpannedToken -> AlexPosn
spanOf (SpannedToken s _) = s

unwrap :: SpannedToken -> Token
unwrap (SpannedToken _ t) = t

-- Helper to validate integers
makeInteger :: AlexPosn -> String -> SpannedToken
makeInteger p s =
    let val = read s :: Integer
        maxInt = 2^16 - 1
        minInt = - (2^16)
    in if val > fromIntegral maxInt || val < fromIntegral minInt
       then SpannedToken p (IntOverflow s)
       else SpannedToken p (Integer (fromIntegral val))

-- Helper to validate identifiers
makeIdentifier :: AlexPosn -> String -> SpannedToken
makeIdentifier p s =
    if length s > 8
    then SpannedToken p (IdentTooLong s)
    else SpannedToken p (Identifier s)
}
