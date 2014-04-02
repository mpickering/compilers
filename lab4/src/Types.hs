module Types where

import Keiko (Op, opName)
import Data.List (intercalate)
import Dict hiding (Ident)

type Ident = String

data Name = Name 
    { name :: Ident 
    , line :: Int
    , def  :: Maybe Def 
    } deriving (Ord, Eq)

makeName :: Ident -> Int -> Name
makeName n l = Name n l Nothing 



instance Show Name where
  show = show . def

mkSeq :: [Stmt] -> Stmt
mkSeq []  = Skip
mkSeq [x] = x
mkSeq xs  = Seq xs

data Program = Program Block deriving (Ord, Eq)

instance Show Program where
  show (Program b) = enc "PROGRAM" (nest [show b]) 

data Block = Block [Ident] [Proc] Stmt deriving (Ord, Eq)

instance Show Block where
  show (Block is ps s) = enc "BLOCK" (nest [show is, show ps, show s])

data Proc = Proc Name [Ident] Block deriving (Ord, Eq)

instance Show Proc where
  show (Proc n is b) = enc "PROC" (nest [show n, show is, show b]) 



data Stmt =
    Skip
  | Seq [Stmt]
  | Assign Name Expr
  | Return Expr
  | Print Expr
  | Newline
  | IfStmt Expr Stmt Stmt
  | WhileStmt Expr Stmt
    deriving (Eq, Ord)

instance Show Stmt where
  show s = 
    case s of
      Skip -> "(SKIP)"
      Seq ss -> enc "SEQ" (nest (map show ss))
      Assign x e -> enc "ASSIGN" (show x ++ " " ++ show e)
      Print e -> enc "PRINT" (show e)
      Newline -> "(NEWLINE)"
      IfStmt e s1 s2 -> enc "IF" (nest  [show e, show s1, show s2])
      WhileStmt e s -> enc "WHILE" (nest [show e, show s])
      Return e -> enc "RETURN" (nest [show e])




--makeExpr :: ExprGuts -> Expr
--makeExpr = flip Expr Nothing

data Expr =
    Number Int
  | Sub Expr Expr
  | Variable Name
  | Monop Op Expr
  | Binop Op Expr Expr 
  | Call Name [Expr] deriving (Ord, Eq)

 
instance Show Expr where
  show e = 
    case e of
      Number n -> enc "NUMBER" (show n)
      Variable x -> enc "VARIABLE" (show $ def x)
      Monop w e -> enc (opName w) (show e)
      Binop w e1 e2 ->  enc (opName w) (show e1 ++ " " ++ show e2)
      Call n es -> enc "CALL" (nest [show n, show es])
        
      _ -> "???"

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
  | ASSIGN 
  | EOF 
  | BADTOK 
  | BEGIN 
  | DO 
  | ELSE 
  | END 
  | IF 
  | THEN 
  | WHILE 
  | PRINT 
  | NEWLINE 
  | VAR
  | SUB
  | BUS
  | PROC
  | RETURN
    deriving (Show, Eq, Ord)




enc s x = "("++ s ++ " " ++  x ++ ")"

nest :: [String] -> String
nest xs =  "\n  " ++ intercalate "\n  " (concatMap lines xs)

  
