{
module Parser where

import qualified Token
import Ast hiding (Ast)
}

%name parseToAst
%tokentype { Token.Token }
%error { parseError }

%token
	const               { Token.Const }
	var                 { Token.Var }
	procedure           { Token.Procedure }
	begin               { Token.Begin }
	end                 { Token.End }
	odd                 { Token.Odd }
	if                  { Token.If }
	then                { Token.Then }
	while               { Token.While }
	do                  { Token.Do }
	read                { Token.Read }
	write               { Token.Write }
	call                { Token.Call }
	integer             { Token.Integer $$ }
	identifier          { Token.Identifier $$ }
	'('                 { Token.LeftParen }
	')'                 { Token.RightParen }
	';'                 { Token.Semicolon }
	':='                { Token.ColonEq }
	'+'                 { Token.Plus }
	'-'                 { Token.Minus }
	'*'                 { Token.Star }
	'/'                 { Token.Slash }
	'='                 { Token.Eq }
	'#'                 { Token.Number }
	'<'                 { Token.LessThan }
	'<='                { Token.LessEqThan }
	'>'                 { Token.GreaterThan }
	'>='                { Token.GreaterEqThan }
	','                 { Token.Comma }
	'.'                 { Token.Dot }

%%
-- Utils
Opt(p)          : p                         { Just $1 }
				|                           { Nothing }

DeliList(d, p)  :                           { [] }
				| d p                       { [$2] }
				| d p DeliList(d, p)        { $2: $3 }

BiTuple(d, p)   :                           { [] }
				| d p                       { [($1, $2)] }
				| d p BiTuple(d, p)         { ($1, $2): $3 }

CommaDeli(p)    : DeliList(',', p)          { $1 }

SemiDeli(p)     : DeliList(';', p)          { $1 }


-- Grammar
Program         :: { Program }
				: Subprogram '.'                                            { Program $1 }

Subprogram      :: { Subprogram }
				: Opt(ConstDecl) Opt(VarDecl) Opt(ProcDecl) Stmt            { Subprogram { constDecl = $1, varDecl = $2, procDecl = $3, stmt = $4 } }

ConstDecl       :: { ConstDecl }
				: const ConstDefi CommaDeli(ConstDefi) ';'                  { ConstDecl $ $2: $3 }

ConstDefi       :: { ConstDefi }
				: identifier '=' integer                                    { ConstDefi $1 $3 }

VarDecl         :: { VarDecl }
				: var identifier CommaDeli(identifier) ';'                  { VarDecl $ $2: $3 }

ProcDecl        :: { ProcDecl }
				: ProcHeader Subprogram ';' MoreProc                        { ProcDecl { header = $1, subprogram = $2, moreProcs = $4 } }

MoreProc        :: { [ ProcDecl ] }
				:                                                           { [] }
				| ProcDecl ';'                                              { [$1] }
				| ProcDecl ';' MoreProc                                     { $1: $3 }

ProcHeader      :: { ProcHeader }
				: procedure identifier ';'                                  { ProcHeader $2 }

Stmt            :: { Stmt }
				: AssignStmt                                                { $1 }
				| CondStmt                                                  { $1 }
				| LoopStmt                                                  { $1 }
				| CallStmt                                                  { $1 }
				| WriteStmt                                                 { $1 }
				| ReadStmt                                                  { $1 }
				| CompoundStmt                                              { $1 }
				|                                                           { NullStmt }

AssignStmt      :: { Stmt }
				: identifier ':=' Expr                                      { AssignStmt $1 $3 }

CompoundStmt    :: { Stmt }
				: begin Stmt SemiDeli(Stmt) end                             { CompoundStmt $ $2: $3 }

Cond            :: { Cond }
				: Expr RelaOp Expr                                          { Cond $1 $2 $3 }
				| odd Expr                                                  { OddCond $2 }

Expr            :: { Expr }
				: Opt(Sign) Item BiTuple(AddSubOp, Item)                    { Expr $1 $2 $3 }

Sign            :: { Sign }
				: '+'                                                       { Positive }
				| '-'                                                       { Negative }

Item            :: { Item }
				: Factor BiTuple(MulDivOp, Factor)                          { Item $1 $ $2 }

Factor          :: { Factor }
				: identifier                                                { Identifier $1 }
				| integer                                                   { Number $1 }
				| '(' Expr ')'                                              { ParenedExpr $2 }

AddSubOp        :: { AddSubOp }
				: '+'                                                       { Add }
				| '-'                                                       { Sub }

MulDivOp        :: { MulDivOp }
				: '*'                                                       { Mul }
				| '/'                                                       { Div }

RelaOp          :: { RelaOp }
				: '='                                                       { Eq }
				| '#'                                                       { NotEq }
				| '<'                                                       { LessThan }
				| '<='                                                      { LessEqThan }
				| '>'                                                       { GreaterThan }
				| '>='                                                      { GreaterEqThan }

CondStmt        :: { Stmt }
				: if Cond then Stmt                                         { CondStmt $2 $4 }

CallStmt        :: { Stmt }
				: call identifier                                           { CallStmt $2 }

LoopStmt        :: { Stmt }
				: while Cond do Stmt                                        { LoopStmt $2 $4 }

ReadStmt        :: { Stmt }
				: read '(' identifier CommaDeli(identifier) ')'             { ReadStmt $ $3: $4 }

WriteStmt       :: { Stmt }
				: write '(' Expr CommaDeli(Expr) ')'                        { WriteStmt $ $3: $4 }

{
parseError :: [Token.Token] -> a
parseError s = error $ show $ s !! 0
} 
