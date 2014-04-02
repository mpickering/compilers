{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Check (typeCheck) where
import Prelude hiding (lookup)
import Data.List (nub)
import Types
import Keiko
import Dict
import Control.Applicative ((<$>), Applicative)
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Set (singleton)

newtype Check a = Check {runCheck :: ReaderT Environment (ErrorT String Identity) a } deriving (Functor, Applicative, Monad, MonadReader Environment, MonadError String)

makeDef ident t = Def ident (Just t) ('_':ident)

typeCheck :: Program -> Either String Program
typeCheck p  = runIdentity $ runErrorT (runReaderT (runCheck $ checkProg p) empty)

checkProg :: Program -> Check Program
checkProg (Program ds p) = do
  let env = checkDecls ds
  s <- either throwError (\x -> local (const x) (checkStmt p)) env
  return $ Program ds s
 
checkDecls :: [Decl] -> Either String Environment
checkDecls ds = case foldM checkDecl empty ds of
                  Nothing -> Left "Variable is declared twice"
                  Just e  -> Right e

checkDecl :: Environment -> Decl -> Maybe Environment 
checkDecl e (Decl ds t) = foldM (defineV t) e (map name ds)

defineV :: Type -> Environment -> Ident -> Maybe Environment 
defineV t e i =  do
  guard (not $ member i e) 
  return $ define (makeDef i t) e 

checkStmt :: Stmt -> Check Stmt
checkStmt Skip = return Skip
checkStmt (Seq ss) = liftM Seq (mapM checkStmt ss)
checkStmt (Assign lhs rhs) = do
  w@(Assign lhs' rhs') <- liftM2 Assign (checkExpr lhs) (checkExpr rhs)
  let ta = getType lhs'
  let tb = getType rhs'
  if ((ta /= tb) || isArray ta || isArray tb) then throwError "Unable to assign to arrays" 
    else return w 
checkStmt (Print e) = do
  p@(Print e') <- liftM Print (checkExpr e)
  let t = getType e'
  if t /= Integer then throwError "print needs an integer"
    else return p
checkStmt (Newline) = return Newline
checkStmt (IfStmt cond thenpt elsept) = do
  e@(IfStmt cond' _ _) <- liftM3 IfStmt (checkExpr cond) (checkStmt thenpt) (checkStmt elsept)
  let t = getType cond'
  when (t /= Boolean) (throwError "Boolean needed in if statement")
  return e
checkStmt (WhileStmt cond body) = do
  e@(WhileStmt cond' _) <- liftM2 WhileStmt (checkExpr cond) (checkStmt body) 
  let t = getType cond'
  when (t /= Boolean) (throwError "Need boolean after while")
  return e


checkName :: Name -> Check Name 
checkName x = do
  varL <- asks (lookup (name x))
  case varL of 
    Nothing -> throwError "Undefined variable"
    Just d  -> return (x{def = d})  

checkExpr :: Expr -> Check Expr 
checkExpr e = 
  case eguts e of
    Variable x -> (\x -> e{etype = x}) . ptype . def  <$> checkName x
    Sub e1 e2 -> do
      e'@(Sub e1' e2') <- liftM2 Sub (checkExpr e1) (checkExpr e2)
      let v = getType e1'
      let s = getType e2'
      varType <- checkSub v s
      return $ Expr e' (Just varType)
    Number n -> 
      return e{etype = Just Integer}
    Monop w e1 -> do
      e'@(Monop _ e1') <- liftM (Monop w) (checkExpr e1) 
      let t = getType e1'
      varType <- checkMonop w t
      return (Expr e' (Just varType))
    Binop w e1 e2 -> do
      e'@(Binop _ e1' e2') <- liftM2 (Binop w) (checkExpr e1) (checkExpr e2)
      let (ta, tb) = (getType e1', getType e2')
      varType <- checkBinop w ta tb 
      return (Expr e' (Just varType))

getType :: Expr -> Type
getType = fromJust . etype 

checkSub :: Type -> Type -> Check Type
checkSub (Array _ t) Integer = return t
checkSub (Array _ _) _ = throwError "Subscript must be integer"
checkSub _ _     = throwError "Can only subscript arrays"

checkMonop :: Op -> Type -> Check Type
checkMonop Uminus t = if t /= Integer then throwError "Semantic error" else return Integer
checkMonop Not t = if t /= Boolean then throwError "Semantic error" else return Boolean 
checkMonop _ _ = throwError "Invalid monop"

checkBinop :: Op -> Type -> Type -> Check Type
checkBinop w ta tb
  | w `elem` [Plus, Minus, Times, Div, Mod] = 
    if (ta /= Integer || tb /= Integer) then throwError "Semantic error"
      else return Integer
  | w `elem` [Eq, Lt, Gt, Leq, Geq, Neq] =
    if (ta /= tb) then throwError "Semantic error"
      else return Boolean
  | w `elem` [And, Or] = 
    if ta /= Boolean || tb /= Boolean then throwError "Semantic error"
      else return Boolean
  | otherwise = throwError "bad binop"

