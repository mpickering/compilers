{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Kgen (translate) where

import Prelude hiding (negate, lines)
import Types hiding (MONOP)
import Dict
import Keiko 
import Control.Monad.State
import Control.Applicative 
import Control.Lens
import qualified Data.Set as S
import Debug.Trace
import Data.Maybe
import Data.List(intercalate)

type Label = Int

slink = 12


data Compiler = Compiler {_l :: Label, _lines :: S.Set Int, _scopeLevel :: Int} deriving (Show)

makeLenses ''Compiler

newtype Gen a = Gen {codeGen :: State Compiler a} deriving (Functor, Applicative, Monad, MonadState Compiler)

            
translate :: [String] -> Program -> String
translate f p = evalState (codeGen $ checkProg f p) (Compiler 0 S.empty 0)

checkProg :: [String] -> Program -> Gen String
checkProg f (Program (Block vars procs body)) = do
  a <- output f <$> genStmt body
  b <- mapM (genProc f) procs
  return $ intercalate "\n" ["PROC MAIN 0 0 0", a, "RETURN", "END\n", concat b]

lineNumber :: Expr -> Int
lineNumber (Variable x) = line x
            

label :: Gen Label 
label = do
  a <- use l
  l .= (a+1)
  return a

showLine :: Int -> Gen Code
showLine l = do
  unseen <- S.notMember l <$> use lines
  lines %= S.insert l
  return $ SEQ [LINE l | unseen]

genAddr :: Def -> Gen Code 
genAddr d = 
  case kind d of 
    VarDef -> do
      clevel <- use scopeLevel
      if level d == 0 then return $ GLOBAL (lab d)
        else if level d /= clevel then do
          let findSlink = SEQ [CONST slink, BINOP PlusA, LOADW]
          let (m, n) = (level d, clevel)
          return $ SEQ [ LOCAL 0, SEQ (replicate (n-m) findSlink)
                   , CONST $ off d, BINOP PlusA]
          else 
            let offset = off d in return $ LOCAL offset
    ProcDef _ -> return $ GLOBAL $ lab d

    _ -> error $ "gen addr " ++ show d

      
    

genExpr :: Expr -> Gen Code 
genExpr e = case e of
  Variable x -> do
    let d = getDef x
    l <- showLine (lineNumber e)  
    dAddr <- genAddr d
    case kind d of 
      VarDef -> return $ SEQ [l, dAddr, LOADW]
      ProcDef _ -> error "no procedure values"
  Number x -> return $ CONST x
  Monop w e1 -> do
    e <- genExpr e1 
    return $ SEQ [e, MONOP w]
  Binop w e1 e2 -> do
    e1' <- genExpr e1
    e2' <- genExpr e2
    return $ SEQ [e1', e2', BINOP w]
  Call p args -> do
    compArgs <- mapM genExpr (reverse args)
    let d = getDef p
    clevel <- use scopeLevel
    let findSlink = SEQ [CONST slink, BINOP PlusA, LOADW]
    let (m, n) = (level d + 1 , clevel)
    li <- showLine (line p)
    dAddr <- genAddr d
    return $ SEQ [li, SEQ compArgs,  LOCAL 0, SEQ (replicate (n-m) findSlink), dAddr, PCALLW $ length args]
    



negate :: Op -> Op      
negate Eq = Neq
negate Neq = Eq
negate Lt = Geq
negate Leq = Gt
negate Gt = Leq
negate Geq = Lt
negate _   = error "negate"



genCond :: Expr -> Bool -> Label -> Gen Code
genCond e sense lab = case e of 
  Number x -> 
    let sense' = sense == (x /= 0) in 
      return $ if sense' then JUMP lab else NOP
  (Monop Not e) -> 
    genCond e (not sense) lab
  (Binop And e1 e2) ->
    if  sense then do
      lab1 <- label
      c1  <- genCond e1 False lab1 
      c2  <- genCond e2 True lab 
      return $ SEQ [c1, c2, LABEL lab1]
    else do
      c1 <- genCond e1 False lab 
      c2 <- genCond e2 False lab 
      return $ SEQ [c1, c2]
  (Binop Or e1 e2) ->
    if sense then do 
      c1  <- genCond e1 True lab 
      c2  <- genCond e2 True lab 
      return $ SEQ [c1, c2]
    else do
      lab1 <- label 
      c1 <- genCond e1 True lab1
      c2 <- genCond e2 False lab 
      return $ SEQ [c1, c2, LABEL lab1]
  (Binop w e1 e2) ->
    if w `elem` [Eq, Neq, Lt, Gt, Leq, Geq] then do
      let sense' = if sense then w else negate w
      (e1', e2') <- liftM2 (,) (genExpr e1) (genExpr e2)
      return $ SEQ [e1', e2', JUMPC sense' lab] 
    else genCond e sense lab
  _ -> do 
    e' <- genExpr e  
    return $ SEQ [e', JUMPB sense lab]

getDef :: Name -> Def
getDef = fromJust . def

-- |gen_stmt| -- generate code for a statement 
genStmt :: Stmt -> Gen Code
genStmt Skip = return NOP
genStmt (Seq stmts) = SEQ <$> mapM genStmt stmts
genStmt (Assign v e) = do
  let d = getDef v
  e' <- genExpr e
  address <- genAddr d
  case kind d of 
    VarDef ->  return $ SEQ [e', address, STOREW] 
    _ -> error "Assign"
genStmt (Print e) = do
  e' <- genExpr e
  return $ SEQ [e', CONST 0, GLOBAL "Lib.Print", PCALL 1]
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
genStmt (Return e) = do 
  e' <- genExpr e
  return $ SEQ [e', RETURNW]

genProc :: [String] -> Proc -> Gen String  
genProc f (Proc p formals (Block vars procs body)) = do
  let d = getDef p
  body' <-  withContext scopeLevel (level d) (genStmt body)
  procs' <- mapM (\x -> withContext scopeLevel (level d) (genProc f x)) procs
  let sBody = output f body' 
  let header = "PROC " ++ lab d ++ " " ++ show (4 * length vars) ++ " 0 0"
  return $ intercalate "\n" [header , sBody, "ERROR E_RETURN 0", "END\n",  concat procs']
  

-- |output| -- output code sequence 
output :: [String] -> Code -> String
output f (SEQ xs) = intercalate "\n" (map (output f) xs)
output f (NOP)    = ""
output f (LINE n) = "! " ++ f !! (n - 1) 
output f x        = show x

withContext :: Lens' Compiler a -> a  -> Gen b -> Gen b 
withContext l v m = do
  old <- use l
  l .= v
  c <- m
  l .= old
  return c 

