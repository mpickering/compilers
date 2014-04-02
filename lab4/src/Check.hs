{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Check (typeCheck) where
import Prelude hiding (lookup)
import Data.List (nub)
import Types
import Keiko
import Dict hiding (Ident)
import Control.Applicative ((<$>), Applicative)
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Set (singleton)

data CheckState = CheckState { env :: Environment, inProc :: Bool, lab' :: Int }

locBase = 0
argBase = 16


newtype Check a = Check {runCheck :: StateT CheckState (ErrorT String Identity) a } deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)


typeCheck :: Program -> Either String Program
typeCheck p  = runIdentity $ runErrorT (evalStateT (runCheck $ checkProg p) (CheckState empty False 0))

label :: Check Int
label = do
  a <- get
  let l = lab' a
  put (a{lab' = l + 1})
  return l

addDef :: Def -> Check ()
addDef d = do
  s <- get
  let e = env s
  put (s{env = define d e}) 

declareProc :: Int -> Proc -> Check ()
declareProc lev (Proc p formals body) = do
  l <- label 
  let newLab = name p ++ "_" ++ show l 
  let d = Def (name p) (ProcDef $ length formals) lev newLab 0
  addDef d

declareGlobal :: Ident -> Check ()
declareGlobal x = do
  let d = Def x VarDef 0 ('_':x) 0
  addDef d

declareLocal :: Int -> (Ident, Int) -> Check ()
declareLocal lev (x,i) = do
  let d = Def x VarDef lev "" (locBase - 4*(i+1))
  addDef d

declareArg :: Int -> (Ident, Int)  -> Check ()
declareArg lev (x, i)= do
  let d = Def x VarDef lev "" (argBase + 4*i)
  addDef d 
  
nbEnv m = do
  a <- get
  return $ withEnv (newBlock (env a))
  

withEnv e m = do
  a <- get
  put (a {env = e })
  r <- m
  newState <- get
  put (newState {env = (env a)})
  return r
 
procScope s m = do
  a <- get
  put (a {inProc = s })
  r <- m
  newState <- get
  put (newState {inProc = (inProc a)})
  return r
  
  

checkProg :: Program -> Check Program
checkProg (Program (Block vars procs body)) = do
  mapM_ declareGlobal vars
  mapM (declareProc 1) procs
  a <- get
  procs' <- withEnv (env a) (mapM (checkProc 1) procs)
  body' <- procScope True (checkStmt body)
  return $ Program (Block vars procs' body')
 
serialize xs = zip xs [0..]

checkProc lev (Proc p formals (Block vars procs body)) = do
  let errLine = line p
  modify (\x -> x {env = newBlock (env x)})
  mapM_ (declareArg lev) (serialize formals)
  mapM_ (declareLocal lev) (serialize vars)
  mapM_ (declareProc (lev + 1)) procs
  procs' <- mapM (checkProc (lev+1)) procs
  stmt' <- procScope True (checkStmt body)
  p' <- checkName p
  return $ Proc p' formals (Block vars procs' stmt')

lookupDef :: Ident -> Check Def
lookupDef x = do
  e <- env <$> get
  case lookup x e of
    Nothing -> throwError "Not defined"
    Just v -> return v

checkStmt :: Stmt -> Check Stmt
checkStmt Skip = return Skip
checkStmt (Seq ss) = liftM Seq (mapM checkStmt ss)
checkStmt (Assign lhs rhs) = do
  e <- env <$> get
  d <- lookupDef (name lhs)
  case kind d of 
    VarDef -> liftM2 Assign (return $ lhs{def = Just d} ) (checkExpr rhs)
    ProcDef _ -> throwError (show lhs ++ " is not a variable")
checkStmt (Return e) = do
  ip <- inProc <$> get
  unless ip (throwError "return statement only allowed in procedure")
  liftM Return (checkExpr e)
checkStmt (Print e) = liftM Print (checkExpr e)
checkStmt (Newline) = return Newline
checkStmt (IfStmt cond thenpt elsept) =  liftM3 IfStmt (checkExpr cond) (checkStmt thenpt) (checkStmt elsept)
checkStmt (WhileStmt cond body) = liftM2 WhileStmt (checkExpr cond) (checkStmt body) 


checkName :: Name -> Check Name 
checkName x = do
  varL <- (\s -> lookup (name x) (env s)) <$> get
  case varL of 
    Nothing -> throwError "Undefined variable"
    Just d  -> return (x{def = Just d})  

checkExpr :: Expr -> Check Expr 
checkExpr e = 
  case e of 
    Variable x -> liftM Variable (checkName x)
    Number n -> return (Number n)
    Monop w e1 -> liftM (Monop w) (checkExpr e1) 
    Binop w e1 e2 -> liftM2 (Binop w) (checkExpr e1) (checkExpr e2)
    Call p args -> do
      d <- lookupDef (name p)
      case kind d of
        VarDef -> throwError ((show $ name p) ++ "is not a procedure")
        ProcDef nargs -> do
          when (length args /= nargs) (throwError "Procedure has incorrect number of arguments")
          liftM2 Call (checkName p) (mapM checkExpr args)  



