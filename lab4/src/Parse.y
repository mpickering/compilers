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
  BEGIN    {BEGIN}
  DO       {DO}
  ELSE     {ELSE}
  END      {END}
  IF       {IF} 
  THEN     {THEN}
  WHILE    {WHILE}
  PRINT    {PRINT}
  VAR      {VAR}
  NEWLINE  {NEWLINE}
  PROC     {PROC}
  RETURN   {RETURN} 

%%


program :: {Program} :       
    block DOT           { Program $1 } 

block :
    var_decl proc_decls BEGIN stmts END  { Block $1 $2 $4 } 

var_decl :
    {- empty -}                         { [] }
  | VAR ident_list SEMI                 { $2 } 

ident_list :
    IDENT                               { [$1] }
  | IDENT COMMA ident_list              { $1 : $3 } 

proc_decls :
    {- empty -}                         { [] }
  | proc_decl proc_decls                { $1 :$2 } 

proc_decl :
    PROC name formals SEMI block SEMI   { Proc $2 $3 $5 } 

formals :
    LPAR RPAR                           { [] } 
  | LPAR ident_list RPAR                { $2 } 

stmts :
    stmt_list                           { Seq $1 } 

stmt_list :
    stmt                                { [$1] }
  | stmt SEMI stmt_list                 { $1 : $3 } 

stmt :
    {- empty -}                         { Skip }
  | name ASSIGN expr                    { Assign $1 $3 }
  | RETURN expr                         { Return $2 }
  | IF expr THEN stmts END              { IfStmt $2 $4 Skip }
  | IF expr THEN stmts ELSE stmts END   { IfStmt $2 $4 $6 }
  | WHILE expr DO stmts END             { WhileStmt $2 $4 }
  | PRINT expr                          { Print $2 } 
  | NEWLINE                             { Newline } 

actuals :
    LPAR RPAR                           { [] }
  | LPAR expr_list RPAR                 { $2 } 

expr_list :
    expr                                { [$1] }
  | expr COMMA expr_list                { $1 : $3 } 

expr :
    simple                              { $1 }
  | expr RELOP simple                   { Binop $2 $1 $3 } 

simple :
    term                                { $1 }
  | simple ADDOP term                   { Binop $2 $1 $3 }
  | simple MINUS term                   { Binop Minus $1 $3 } 

term :
    factor                              { $1 }
  | term MULOP factor                   { Binop $2 $1 $3 } 

factor :
    NUMBER                              { Number $1 } 
  | name                                { Variable $1 }
  | name actuals                        { Call $1 $2 }
  | MONOP factor                        { Monop $1 $2 }
  | MINUS factor                        { Monop Uminus $2 }
  | LPAR expr RPAR                      { $2 } 


name :: { Name } :
    IDENT                               {% do 
                                            (line, _) <- getPosn
                                            return $ makeName $1 line  } 



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

