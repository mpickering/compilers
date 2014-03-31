{
module Parse where

import Lexer
import Debug.Trace
import Types
import Keiko (Op(..))
import Dict

}



%name parseFile program
%error {happyError}
%lexer  {lexwrap} {EOF}
%monad {Alex}
%tokentype {Token}
%token
  IDENT    {IDENT $$}
  MONOP    {MONOP $$}
  MULOP    {MULOP $$} 
  ADDOP    {ADDOP $$}
  RELOP    {RELOP $$}
  NUMBER   {NUMBER $$}
  SEMI     {SEMI}
  DOT      {DOT}
  COLON    {COLON}
  LPAR     {LPAR}
  RPAR     {RPAR}
  COMMA    {COMMA}
  MINUS    {MINUS}
  ASSIGN   {ASSIGN}
  OF       {OF}
  BEGIN    {BEGIN}
  DO       {DO}
  ELSE     {ELSE}
  END      {END}
  IF       {IF} 
  ELSIF    {ELSIF }
  THEN     {THEN}
  WHILE    {WHILE}
  PRINT    {PRINT}
  NEWLINE  {NEWLINE}
  SUB      {SUB}
  BUS      {BUS}
  VAR      {VAR}
  INTEGER  {INTEGER}
  BOOLEAN  {BOOLEAN}
  ARRAY    {ARRAY}

%%


program :: {Program} :       
    decls BEGIN stmts END DOT           { Program $1 $3 } 

decls :: {[Decl]} :
    {- empty -}                         { [] } 
  | decl decls                          { $1 : $2 }

decl :: {Decl} :
    VAR name_list COLON typexp SEMI     { Decl $2 $4 }   

name_list :: {[Name]} :
    name                                { [$1] }
  | name COMMA name_list                { $1 : $3 }

typexp :: { Type } :
    INTEGER                             { Integer }
  | BOOLEAN                             { Boolean }
  | ARRAY NUMBER OF typexp              { Array $2 $4 }

stmts :: {Stmt} : 
    stmt_list                           { mkSeq $1 } 

stmt_list ::{ [Stmt] }:
    stmt                                { [$1] }
  | stmt SEMI stmt_list                 { $1 : $3 } 

stmt :: {Stmt} :  
                                        { Skip }
  | name ASSIGN expr                    { Assign $1 $3 }
  | PRINT expr                          { Print $2 }
  | NEWLINE                             { Newline }
  | IF expr THEN stmts END              { IfStmt $2 $4 Skip }
  | IF expr THEN stmts ELSE stmts END   { IfStmt $2 $4 $6 }
  | IF expr THEN stmts elsifs END       { IfStmt $2 $4 $5 }
  | WHILE expr DO stmts END             { WhileStmt $2 $4 } 

 
elsifs :: { Stmt } :
                                        {Skip}
  | ELSIF expr THEN stmts elsifs        {IfStmt $2 $4 $5}
  | ELSIF expr THEN stmts ELSE stmts    {IfStmt $2 $4 $6}

expr :: { Expr } :
    simple                              { $1 }
  | expr RELOP simple                   { makeExpr $ Binop $2 $1 $3 } 
    
simple :: { Expr } :
    term                                { $1 }
  | simple ADDOP term                   { makeExpr $ Binop $2 $1 $3 }
  | simple MINUS term                   { makeExpr $ Binop Minus $1 $3 } 

term :: { Expr } :
    factor                              { $1 }
  | term MULOP factor                   { makeExpr $ Binop $2 $1 $3 } 

factor :: { Expr } :
    variable                            { $1 }
  | NUMBER                              { makeExpr $ Number $1 }
  | MONOP factor                        { makeExpr $ Monop $1 $2 }
  | MINUS factor                        { makeExpr $ Monop Uminus $2 }
  | LPAR expr RPAR                      { $2 } 

variable :: { Expr } :
    name                                { makeExpr $ Variable $1 }
  | variable SUB expr BUS               { makeExpr $ Sub $1 $3 } 
 

name :: { Name } :
    IDENT                               {% do 
                                            (line, _) <- getPosn
                                            return $ Name $1 line (Def $1 Nothing ('_':$1)) } 



{

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return $ (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  alexError (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")  

parse s = runAlex s parseFile

}

