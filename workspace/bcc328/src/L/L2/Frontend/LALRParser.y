{
module L.L2.Frontend.LALRParser where

import L.L2.Frontend.Lexer hiding (lexer)
import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var
}

%name lalrHappyParser Prog
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error { parseError }
%lexer {lexer}{Token _ TEOF}

%token
	num	{Token _ (TNumber $$)}
	id	{Token _ (TId $$)}
	str	{Token _ (TString $$)}
	TLParen	{Token _ TLParen}
	TRParen	{Token _ TRParen}
	TPlus	{Token _ TPlus}
	TMinus	{Token _ TMinus}
	TTimes	{Token _ TTimes}
        TDiv	{Token _ TDiv }
	TAssign	{Token _ TAssign}
	TEOC	{Token _ TEOC}
	TComma	{Token _ TComma}
	TRead	{Token _ TRead}
	TPrint	{Token _ TPrint}
	TDef	{Token _ TDef}
	TIn	{Token _ TIn}
	TEnd	{Token _ TEnd}

%%

Prog : StmtList { L2 $1 }

StmtList :			{ [] }
	 | Stmt StmtList        { $1 : $2 }

Stmt : SimpleStmt TEOC	{ $1 }
     | CompoundStmt	{ $1 }

SimpleStmt : id TAssign Expr				{ LAssign (Var $1) $3 }
	   | TRead TLParen str TComma id TRParen	{ LRead $3 (Var $5) }
           | TPrint TLParen Expr TRParen		{ LPrint $3 }

CompoundStmt : TDef id TAssign Expr TIn StmtList TEnd   { LDef (Var $2) $4 $6 }

Expr : Term TPlus Expr	{ LAdd $1 $3 }
     | Term TMinus Expr	{ LMinus $1 $3 }
     | Term		{ $1 }

Term : Factor TTimes Term 	{ LMul $1 $3 }
     | Factor TDiv Term 	{ LDiv $1 $3 }
     | Factor          		{ $1 }

Factor : num	{ LVal (VInt $1) }
       | str	{ LVal (VStr $1) }
       | id	{ LVar (Var $1) }
{

parserTest :: String -> IO ()
parserTest s = do
  r <- lalrParser s
  print r

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

lalrParser :: String -> IO (Either String L2)
lalrParser s = pure $ runAlex s lalrHappyParser

}
