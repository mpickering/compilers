module Types where

import Keiko (Op, opName)
import Data.List (intercalate)
type Ident = String

data Name = Name 
    { name :: Ident 
    , lab  :: String
    , line :: Int
    } deriving (Ord, Eq)

instance Show Name where
  show = name

mkSeq :: [Stmt] -> Stmt
mkSeq []  = Skip
mkSeq [x] = x
mkSeq xs  = Seq xs

data Program = Program Stmt deriving (Ord, Eq)

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
  | Exit deriving (Eq, Ord)

data Expr =
    Number Int
  | Variable Name
  | Monop Op Expr
  | Binop Op Expr Expr deriving (Ord, Eq)
 

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

instance Show Program where
  show (Program p) = show p 

instance Show Stmt where
  show s = 
    case s of
      Skip -> "(SKIP)"
      Seq ss -> enc "SEQ" (nest (map show ss))
      Assign x e -> enc "ASSIGN" (name x ++ " " ++ show e)
      Print e -> enc "PRINT" (show e)
      Newline -> "(NEWLINE)"
      IfStmt e s1 s2 -> enc "IF" (nest  [show e, show s1, show s2])
      WhileStmt e s -> enc "WHILE" (nest [show e, show s])
      RepeatStmt s e -> enc "REPEAT" (nest [show s, show e])
      LoopStmt s -> enc "LOOP" (nest [show s])
      Exit -> "(EXIT)\n"
      CaseStmt e cases elsept -> "???"
    {-    let fArm (labs, body) = 
        fMeta "(ARM $ $)" [fList(fNum) labs; fStmt body] in
        fMeta "(CASE $ $ $)" [fExpr e; fList(fArm) cases; fStmt elsept]
-}

enc s x = "("++ s ++ " " ++  x ++ ")"

nest :: [String] -> String
nest xs =  "\n  " ++ intercalate "\n  " (concatMap lines xs)

instance Show Expr where
  show e = 
    case e of
      Number n -> enc "NUMBER" (show n)
      Variable x -> enc "VARIABLE" (name x)
      Monop w e -> enc (opName w) (show e)
      Binop w e1 e2 ->  "(" ++ opName w ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  
