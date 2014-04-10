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
import Data.List(intercalate)

type Label = Int

slink = 12


data Compiler = Compiler {_l :: Label,  _scopeLevel :: Int} deriving (Show)

makeLenses ''Compiler

newtype Gen a = Gen {codeGen :: State Compiler a} deriving (Functor, Applicative, Monad, MonadState Compiler)

withContext :: Lens' Compiler a -> a  -> Gen b -> Gen b 
withContext l v m = do
  old <- use l
  l .= v
  c <- m
  l .= old
  return c 

            
--output :: [String] -> Program -> String
--output f p = evalState (codeGen $ checkProg f p) (Compiler 0 S.empty 0)

output :: [String] -> Program -> IO ()
output f (Program (Block is ps s)) = do
  putStrLn  "MODULE Main 0 0\nIMPORT Lib 0\nENDHDR\n"


  putStrLn "PROC MAIN 0 0 0"
  let (body, procs) = unravel $ liftM2 (,) (outMain f s) (mapM (outProc f) ps)
  evalStateT (do
    body
    liftIO $ putStrLn "RETURN"
    liftIO $ putStrLn "END\n"     
    unless (null ps) (foldl1 (>>) procs)) 1
  mapM_ (putStrLn . (\x -> "GLOVAR _" ++ x ++" 4")) (is)
  
unravel :: Gen a -> a
unravel ms = (evalState (codeGen ms) (Compiler 1 0)) 

outProc :: [String] -> Proc -> Gen (StateT Int IO ())
outProc f (Proc n formals (Block vars procs body)) = do
  let d = getDef n
  let clevel = level d
  stmt <- withContext scopeLevel clevel (genStmt body)
  procs <- withContext scopeLevel clevel $ mapM (outProc f) procs
  let memAloc = show $ 4 * length vars
  return (do
    liftIO $ putStrLn ("PROC " ++ lab d ++ " " ++ memAloc ++ " 0 0")
    outCode f stmt
    liftIO $ putStrLn "ERROR E_RETURN 0"
    liftIO $ putStrLn "END"
    unless (null procs) (foldl1 (>>) procs)
    )
    

outMain :: [String] -> Stmt -> Gen (StateT Int IO ())
outMain f b = outCode f <$> genStmt b


-- |output| -- output code sequence 
outCode :: [String] -> Code -> StateT Int IO ()
outCode f (SEQ xs) = mapM_ (outCode f) xs
outCode f (NOP)    = return ()
outCode f (LINE n) = do
  curLine <- get
  when (curLine /= n) (put n >> liftIO (putStrLn $ "! " ++ f !! (n - 1)))
outCode f x        = liftIO $ print x


label :: Gen Label 
label = do
  a <- use l
  l .= (a+1)
  return a


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


      
    

genExpr :: Expr -> Gen Code 
genExpr e = case e of
  Variable x -> do
    let d = getDef x
    dAddr <- genAddr d
    case kind d of 
      VarDef -> return $ SEQ [LINE $ line x, dAddr, LOADW]
      ProcDef _ -> do 
        clevel <- use scopeLevel
        let findSlink = SEQ [CONST slink, BINOP PlusA, LOADW]
        let (m, n) = (level d + 1, clevel)
        return $ SEQ [LINE $ line x, LOCAL 0, SEQ $ replicate (n-m) findSlink, dAddr, PACK]
 
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
    case kind d of 
      ProcDef _ -> do
        clevel <- use scopeLevel
        let findSlink = SEQ [CONST slink, BINOP PlusA, LOADW]
        let (m, n) = (level d + 1 , clevel)
        dAddr <- genAddr d
        return $ SEQ [LINE $ line p, SEQ compArgs,  LOCAL 0, SEQ (replicate (n-m) findSlink), dAddr, PCALLW $ length args]
      VarDef -> do
        vAddr <- genAddr d
        return $ SEQ [LINE $ line p, SEQ compArgs, vAddr, LOADW, UNPACK, PCALLW $ length args]
    



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
