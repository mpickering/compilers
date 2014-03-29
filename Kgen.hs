{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Kgen (translate) where

import Prelude hiding (negate)
import Types hiding (MONOP)
import Keiko 
import Control.Monad.State
import Control.Applicative
import Control.Lens

type Label = Int

data Compiler = Compiler {_l :: Label, _scope :: Label} deriving (Show)

makeLenses ''Compiler

newtype Gen a = Gen {codeGen :: State Compiler a} deriving (Functor, Applicative, Monad, MonadState Compiler)

label :: Gen Label 
label = do
  a <- use l
  l .= (a+1)
  return a

getScope :: Gen Label
getScope = use scope

genExpr :: Expr -> Code 
genExpr (Variable x) = 
  SEQ [LINE $ line x, LDGW $ name x]
genExpr (Number x)       = CONST x
genExpr (Monop w e1)     = SEQ [genExpr e1, MONOP w]
genExpr (Binop w e1 e2)  = SEQ [genExpr e1, genExpr e2, BINOP w]


negate :: Op -> Op      
negate Eq = Neq
negate Neq = Eq
negate Lt = Geq
negate Leq = Gt
negate Gt = Leq
negate Geq = Lt
negate _   = error "negate"

-- Generate code to jump to |lab| if |e| has value |sense| 
genCond :: Expr -> Bool -> Label -> Gen Code
genCond (Number x) sense lab 
  | b == sense = return $ JUMP lab
  | otherwise = return NOP
  where b = (x /= 0) 
genCond (Monop Not e) sense lab = 
  genCond e (not sense) lab
genCond (Binop And e1 e2) sense lab
  | sense = do
      lab1 <- label
      c1  <- genCond e1 False lab1 
      c2  <- genCond e2 True lab 
      return $ SEQ [c1, c2, LABEL lab1]
  | otherwise = do
      c1 <- genCond e1 False lab 
      c2 <- genCond e2 False lab 
      return $ SEQ [c1, c2]
genCond (Binop Or e1 e2) sense lab 
  | sense = do
      c1  <- genCond e1 True lab 
      c2  <- genCond e2 True lab 
      return $ SEQ [c1, c2]
  | otherwise = do
      lab1 <- label 
      c1 <- genCond e1 True lab1
      c2 <- genCond e2 False lab 
      return $ SEQ [c1, c2, LABEL lab1]
genCond e@(Binop w e1 e2) sense lab
  | w `elem` [Eq, Neq, Lt, Gt, Leq, Geq] =
    return $ SEQ [genExpr e1, genExpr e2, JUMPC sense' lab] 
  | otherwise = genCond e sense lab
  where
    sense' = if sense then w else negate w
genCond e sense lab = return $ SEQ [genExpr e, JUMPB sense lab]


-- |gen_stmt| -- generate code for a statement 
genStmt :: Stmt -> Gen Code
genStmt Skip = return NOP
genStmt (Seq stmts) = SEQ <$> mapM genStmt stmts
genStmt (Assign v e) = return $
  SEQ  [LINE $ line v, genExpr e, STGW $ lab v]
genStmt (Print e) = return $
  SEQ  [genExpr e, CONST 0, GLOBAL "Lib.Print", PCALL 1]
genStmt Newline = return $
  SEQ [CONST 0, GLOBAL "Lib.Newline", PCALL 0]
genStmt (IfStmt test thenpt elsept)  = do
  lab1 <- label
  lab2 <- label
  cTest <- genCond test False lab1 
  cThenPt <- genStmt thenpt
  cElsePt <- genStmt elsept
  return $ SEQ [ cTest, cThenPt, JUMP lab2
               , LABEL lab1, cElsePt
               , LABEL lab2]
genStmt (WhileStmt test body)  = do
  lab1 <- label
  lab2 <- label
  (c1, c2) <- liftM2 (,) (genCond test False lab2) (genStmt body)
  return $ SEQ [ LABEL lab1, c1, c2
               , JUMP lab1, LABEL lab2]
genStmt (RepeatStmt body test)  = do
  lab1 <- label
  cBody <- genStmt body
  cTest <- genCond test False lab1
  return $ SEQ [LABEL lab1, cBody, cTest]
genStmt (LoopStmt body)  = do
  lab1 <- label
  lab2 <- label
  scope .= lab2
  c <- genStmt body
  return $ SEQ [LABEL lab1, c, JUMP lab1, LABEL lab2]
genStmt (Exit)  = JUMP <$> getScope
genStmt (CaseStmt test cs elsept)  = do
  ls <- mapM (const label) cs 
  let lls = concat (zipWith (\(xs,_) l -> map (\x -> (x,l)) xs) cs ls) 
  (def_lab, exit_lab) <- liftM2 (,) label label
  let caseStmt l (_,c) = do
        code <- genStmt c
        return $ SEQ [LABEL l,  code, JUMP exit_lab] 

  cases <- mapM (uncurry caseStmt) (zip ls cs)
  cElse <- genStmt elsept
  return $  SEQ [ genExpr test
                , CASEJUMP (length lls)
                , SEQ (map (uncurry CASEARM) lls)
                , JUMP def_lab
                , SEQ cases
                , LABEL def_lab
                , cElse
                , LABEL exit_lab
                ]
            
translate :: Program -> Code
translate (Program p)  = 
  let c = evalState (codeGen $ genStmt p)  (Compiler 1 0) in
  SEQ [c, LABEL 0]
