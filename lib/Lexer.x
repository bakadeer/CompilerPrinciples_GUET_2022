{
module Lexer where

import Token
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

	$white+                                 ;
	"//".*                                  ;
	const                                   { \_ -> Const }
	var                                     { \_ -> Var }
	procedure                               { \_ -> Procedure }
	begin                                   { \_ -> Begin }
	end                                     { \_ -> End }
	odd                                     { \_ -> Odd }
	if                                      { \_ -> If }
	then                                    { \_ -> Then }
	while                                   { \_ -> While }
	do                                      { \_ -> Do }
	call                                    { \_ -> Call }
	read                                    { \_ -> Read }
	write                                   { \_ -> Write }
	$digit+                                 { \s -> Integer $ read s }
	$alpha [$alpha $digit \_]*              { \s -> Identifier s }
	"+"                                     { \_ -> Plus }
	"-"                                     { \_ -> Minus }
	"*"                                     { \_ -> Star }
	"/"                                     { \_ -> Slash }
	":="                                    { \_ -> ColonEq }
	"="                                     { \_ -> Eq }
	"<"                                     { \_ -> LessThan }
	"<="                                    { \_ -> LessEqThan }
	"#"                                     { \_ -> Number }
	">"                                     { \_ -> GreaterThan }
	">="                                    { \_ -> GreaterEqThan }
	","                                     { \_ -> Comma }
	";"                                     { \_ -> Semicolon }
	"."                                     { \_ -> Dot }
	"("                                     { \_ -> LeftParen }
	")"                                     { \_ -> RightParen }

