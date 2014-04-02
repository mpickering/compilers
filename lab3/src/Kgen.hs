{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes #-}

module Kgen (output) where

import Prelude hiding (negate, lines)
import Types hiding (MONOP)
import Dict
import Keiko 
import Control.Monad.State
import Control.Applicative
import Control.Lens
import qualified Data.Set as S
import Data.Maybe
import Data.List (intercalate)

type Label = Int


data Compiler = Compiler {_l :: Label} deriving (Show)

makeLenses ''Compiler

newtype Gen a = Gen {codeGen :: State Compiler a} deriving (Functor, Applicative, Monad, MonadState Compiler)

lineNumber :: Expr -> Int
lineNumber e = 
  case eguts e of
    (Variable x) -> line x
    (Sub a e)  -> lineNumber a
    _ -> 999
            

label :: Gen Label 
label = do
  a <- use l
  l .= (a+1)
  return a


genAddr :: Expr -> Gen Code 
genAddr v = 
  case eguts v of 
    Variable x -> do
      return $ SEQ [LINE $ line x, GLOBAL $ (lab . def) x]
    Sub a e -> do
        let sizeA = typeSize (baseType $ fromJust $ etype a)
        let s = typeSize (fromJust $ etype e) 
        baseAddr <- genAddr a 
        ecode <- genExpr e
        return $ SEQ [baseAddr, ecode, CONST sizeA,  BINOP Times, BINOP PlusA]
    _ -> error $ "gen addr " ++ show v 

      
    

genExpr :: Expr -> Gen Code 
genExpr e = case eguts e of
  Variable x -> do
    eAddr <- genAddr e
    let t = fromJust $ etype e
    return $ SEQ [eAddr, loadInst t]
  Sub v s -> do
    eAddr <- genAddr e
    return $ SEQ [eAddr, loadInst (fromJust $ etype e)]
  Number x -> return $ CONST x
  Monop w e1 -> do
    e <- genExpr e1 
    return $ SEQ [e, MONOP w]
  Binop w e1 e2 -> do
    e1' <- genExpr e1
    e2' <- genExpr e2
    return $ SEQ [e1', e2', BINOP w]

loadInst :: Type -> Code
loadInst t
  | s == 1 = LOADC
  | s == 4 = LOADW 
  | otherwise = error "incorrect type size"
  where
    s = typeSize t


negate :: Op -> Op      
negate Eq = Neq
negate Neq = Eq
negate Lt = Geq
negate Leq = Gt
negate Gt = Leq
negate Geq = Lt
negate _   = error "negate"



genCond :: Expr -> Bool -> Label -> Gen Code
genCond e sense lab = case eguts e of 
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


-- |gen_stmt| -- generate code for a statement 
genStmt :: Stmt -> Gen Code
genStmt Skip = return NOP
genStmt (Seq stmts) = SEQ <$> mapM genStmt stmts
genStmt (Assign v e) = do
  e' <- genExpr e
  address <- genAddr v
  let st = fromJust $ etype e
  let storeInst = case typeSize st of 
                      4 -> STOREW
                      1 -> STOREC
  return $ SEQ [LINE $ lineNumber v, e', address, storeInst] 
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


withContext :: Lens' Compiler a -> a  -> Gen Code -> Gen Code 
withContext l v m = do
  old <- use l
  l .= v
  c <- m
  l .= old
  return c 
            
translate :: Program -> Code
translate (Program ds p)  = evalState (codeGen $ genStmt p)  (Compiler 2)

output :: [String] -> Program -> IO ()
output f p@(Program ds _) = do
  putStrLn "MODULE Main 0 0"
  putStrLn "IMPORT Lib 0" 
  putStrLn "ENDHDR\n" 

  putStrLn "PROC MAIN 0 0 0"
  let code = translate p
  runStateT (outCode code) 1

  putStrLn "RETURN"
  putStrLn "END\n" 
  
  let showDecl (Decl xs t) = map (\x -> "GLOVAR _" ++ name x ++ " " ++ show s) xs
        where
          s = typeSize t
      
  putStrLn (intercalate "\n" (concatMap showDecl ds))
  where 
    outCode :: Code -> StateT Int IO ()
    outCode  (SEQ xs) = mapM_ outCode xs
    outCode  (NOP)    = return () 
    outCode  (LINE n) = do
      curLine <- get
      when (curLine /= n) (put n >> liftIO (putStrLn $ "! " ++ f !! (n - 1)))
    outCode  x        = liftIO $ print x
