module Types where

import Keiko (Op, opName)
import Data.List (intercalate)
import Dict


data Name = Name 
    { name :: Ident 
    , line :: Int
    , def  :: Def 
    } deriving (Ord, Eq)


instance Show Name where
  show = show . def

mkSeq :: [Stmt] -> Stmt
mkSeq []  = Skip
mkSeq [x] = x
mkSeq xs  = Seq xs

data Program = Program [Decl] Stmt deriving (Ord, Eq)

instance Show Program where
  show (Program ds p) = enc "PROGRAM" (nest [intercalate "\n" (map show ds), show p]) 

data Decl = Decl [Name] Type deriving (Ord, Eq)

instance Show Decl where
  show (Decl ns t) = enc "DECL" (show ns ++ " " ++ show t) 

data Stmt =
    Skip
  | Seq [Stmt]
  | Assign Expr Expr
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

data Expr = Expr 
    { eguts :: ExprGuts 
    , etype :: Maybe Type
    }  deriving (Eq,  Ord)

instance Show Expr where
  show = show . eguts


makeExpr :: ExprGuts -> Expr
makeExpr = flip Expr Nothing

data ExprGuts =
    Number Int
  | Sub Expr Expr
  | Variable Name
  | Monop Op Expr
  | Binop Op Expr Expr deriving (Ord, Eq)
 
instance Show ExprGuts where
  show e = 
    case e of
      Number n -> enc "NUMBER" (show n)
      Variable x -> enc "VARIABLE" (show $ def x)
      Monop w e -> enc (opName w) (show e)
      Binop w e1 e2 ->  enc (opName w) (show e1 ++ " " ++ show e2)
      Sub a e -> enc "SUB" (show a ++ " " ++ show e)
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
  | VAR
  | SUB
  | BUS
  | INTEGER
  | BOOLEAN
  | ARRAY
    deriving (Show, Eq, Ord)




enc s x = "("++ s ++ " " ++  x ++ ")"

nest :: [String] -> String
nest xs =  "\n  " ++ intercalate "\n  " (concatMap lines xs)

  
