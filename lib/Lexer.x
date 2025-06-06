{
module Lexer (
	spanOf,
	unwrap,
	SpannedToken(..),
	scanTokens,  -- 使用我们的包装函数
	alexScanTokens,  -- 保留原始函数以防需要
	AlexPosn(..),
) where

import Token
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

	$white+                                 ;
	"//".*                                  ;
  "/*"[.\r\n]*"*/"                        ;
	const                                   { \s _ -> SpannedToken s Const }
	var                                     { \s _ -> SpannedToken s Var }
	procedure                               { \s _ -> SpannedToken s Procedure }
	begin                                   { \s _ -> SpannedToken s Begin }
	end                                     { \s _ -> SpannedToken s End }
	odd                                     { \s _ -> SpannedToken s Odd }
	if                                      { \s _ -> SpannedToken s If }
	then                                    { \s _ -> SpannedToken s Then }
	while                                   { \s _ -> SpannedToken s While }
	do                                      { \s _ -> SpannedToken s Do }
	call                                    { \s _ -> SpannedToken s Call }
	read                                    { \s _ -> SpannedToken s Read }
	write                                   { \s _ -> SpannedToken s Write }
	$digit+                                 { \s v -> SpannedToken s (Integer $ read v) }
	$alpha [$alpha $digit \_]*              { \s v -> SpannedToken s (Identifier v) }
	"+"                                     { \s _ -> SpannedToken s Plus }
	"-"                                     { \s _ -> SpannedToken s Minus }
	"*"                                     { \s _ -> SpannedToken s Star }
	"/"                                     { \s _ -> SpannedToken s Slash }
	":="                                    { \s _ -> SpannedToken s ColonEq }
	"="                                     { \s _ -> SpannedToken s Eq }
	"<"                                     { \s _ -> SpannedToken s LessThan }
	"<="                                    { \s _ -> SpannedToken s LessEqThan }
	"#"                                     { \s _ -> SpannedToken s Number }
	">"                                     { \s _ -> SpannedToken s GreaterThan }
	">="                                    { \s _ -> SpannedToken s GreaterEqThan }
	","                                     { \s _ -> SpannedToken s Comma }
	";"                                     { \s _ -> SpannedToken s Semicolon }
	"."                                     { \s _ -> SpannedToken s Dot }
	"("                                     { \s _ -> SpannedToken s LeftParen }
	")"                                     { \s _ -> SpannedToken s RightParen }
	.                                       { \s v -> SpannedToken s (Identifier v)}

{
data SpannedToken = SpannedToken AlexPosn Token deriving (Show, Eq)

spanOf :: SpannedToken -> AlexPosn
spanOf (SpannedToken s _) = s

unwrap :: SpannedToken -> Token
unwrap (SpannedToken _ t) = t
}
