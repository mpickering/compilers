module Types where

import Keiko (Op)
type Ident = String

data Name = Name 
    { name :: Ident 
    , lab  :: String
    , line :: Int
    } deriving (Ord, Show, Eq)

mkSeq :: [Stmt] -> Stmt
mkSeq []  = Skip
mkSeq [x] = x
mkSeq xs  = Seq xs

data Program = Program Stmt deriving (Ord, Show, Eq)

data Stmt =
    Skip
  | Seq [Stmt]
  | Assign Name Expr
  | Print Expr
  | Newline
  | IfStmt Expr Stmt Stmt
  | WhileStmt Expr Stmt
  | RepeatStmt Stmt Expr
  | LoopStmt Stmt
  | CaseStmt Expr [([Int], Stmt)] Stmt 
  | Exit deriving (Show, Eq, Ord)

data Expr =
    Number Int
  | Variable Name
  | Monop Op Expr
  | Binop Op Expr Expr deriving (Ord, Show, Eq)
 

data Token =
    IDENT Ident 
  | MONOP Op  
  | MULOP Op 
  | ADDOP Op 
  | RELOP Op 
  | NUMBER Int 
  | SEMI 
  | DOT 
  | COLON 
  | LPAR 
  | RPAR 
  | COMMA 
  | MINUS 
  | VBAR 
  | ASSIGN 
  | EOF 
  | BADTOK 
  | CASE 
  | OF 
  | BEGIN 
  | DO 
  | ELSE 
  | END 
  | IF 
  | ELSIF 
  | THEN 
  | WHILE 
  | PRINT 
  | NEWLINE 
  | REPEAT 
  | UNTIL 
  | LOOP 
  | EXIT  deriving (Show, Eq, Ord)

