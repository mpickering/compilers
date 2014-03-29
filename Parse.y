{
module Parse where

import Lexer
import Debug.Trace
import Types
import Keiko (Op(..))

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
  VBAR     {VBAR}
  ASSIGN   {ASSIGN}
  EOF      {EOF}
  BADTOK   {BADTOK}
  CASE     {CASE}
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
  REPEAT   {REPEAT}
  UNTIL    {UNTIL}
  LOOP     {LOOP}
  EXIT     {EXIT}


%%


program :: {Program} :       
    BEGIN stmts END DOT                 { Program $2 } 

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
  | REPEAT stmts UNTIL expr             { RepeatStmt $2 $4 }
  | LOOP stmts END                      { LoopStmt $2 }
  | EXIT                                { Exit }
  | CASE expr OF cases END              { CaseStmt $2 $4 Skip }
  | CASE expr OF cases ELSE stmts END   { CaseStmt $2 $4 $6 }

cases :: {[([Int], Stmt)]} :
                            { [] }
  | case                    { [$1] }
  | case VBAR cases         { $1 : $3 } 

case ::  {([Int], Stmt)} :
   nums COLON stmts        { ($1, $3) }

nums :: { [Int] }: 
                                        { []   }
  | NUMBER                              { [$1] } 
  | NUMBER COMMA nums                   { $1 : $3 }
 
elsifs :: { Stmt } :
                                        {Skip}
  | ELSIF expr THEN stmts elsifs        {IfStmt $2 $4 $5}
  | ELSIF expr THEN stmts ELSE stmts    {IfStmt $2 $4 $6}

expr :: { Expr } :
    simple                              { $1 }
  | expr RELOP simple                   { Binop $2 $1 $3 } 
    
simple :: { Expr } :
    term                                { $1 }
  | simple ADDOP term                   { Binop $2 $1 $3 }
  | simple MINUS term                   { Binop Minus $1 $3 } 

term :: { Expr } :
    factor                              { $1 }
  | term MULOP factor                   { Binop $2 $1 $3 } 

factor :: { Expr } :
    name                                { Variable $1 }
  | NUMBER                              { Number $1 }
  | MONOP factor                        { Monop $1 $2 }
  | MINUS factor                        { Monop Uminus $2 }
  | LPAR expr RPAR                      { $2 } 

name :: { Name } :
    IDENT                               {% do {a <- getPosn; return $ Name $1 "" (fst a)} } 



{

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  alexError (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")  

parse s = runAlex s parseFile

}

