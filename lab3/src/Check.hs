{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Check where
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

newtype Check a = Check {runCheck :: ReaderT Environment (ErrorT String Identity) a } deriving (Functor, Applicative, Monad, MonadReader Environment, MonadError String)

makeDef ident t = Def ident (Just t) ('_':ident)

check :: Program -> Environment -> Either String Program
check p d  = runIdentity $ runErrorT $ (runReaderT (runCheck $ checkProg p) d)


checkProg :: Program -> Check Program
checkProg (Program ds p) = do
  let env = checkDecls ds
  s <- either (throwError) (\x -> local (const x) (checkStmt p)) env 
  return $ Program ds s
 
checkDecls :: [Decl] -> Either String Environment
checkDecls ds = case foldM checkDecl empty ds of
                  Nothing -> Left "Variable is declared twice"
                  Just e  -> Right e

checkDecl :: Environment -> Decl -> Maybe Environment 
checkDecl e (Decl ds t) = foldM (defineV t) empty (map name ds)

defineV :: Type -> Environment -> Ident -> Maybe Environment 
defineV t e i =  do
  guard (not $ member i e) 
  return $ define (makeDef i t) e 

checkStmt :: Stmt -> Check Stmt
checkStmt Skip = return Skip
checkStmt (Seq ss) = liftM Seq (mapM checkStmt ss)
checkStmt (Assign lhs rhs) = do
  ta <- fromJust . ptype . def <$> checkName lhs
  tb <- (getType rhs)
  if ((ta /= tb) || isArray ta || isArray tb) then throwError "Unable to assign to arrays" 
    else liftM2 Assign (checkName lhs) (checkExpr rhs)
checkStmt (Print e) = do
  t <- getType e
  if t /= Integer then throwError "print needs an integer"
    else liftM Print (checkExpr e)
checkStmt (Newline) = return Newline
checkStmt (IfStmt cond thenpt elsept) = do
  t <- getType cond
  when (t /= Boolean) (throwError "Boolean needed in if statement")
  liftM3 IfStmt (checkExpr cond) (checkStmt thenpt) (checkStmt elsept)
checkStmt (WhileStmt cond body) = do
  t <- getType cond
  when (t /= Boolean) (throwError "Need boolean after while")
  liftM2 WhileStmt (checkExpr cond) (checkStmt body) 


checkName :: Name -> Check Name 
checkName x = do
  varL <- asks (lookup (name x))
  case varL of 
    Nothing -> throwError "Undefined variable"
    Just v  -> return x 

checkExpr :: Expr -> Check Expr 
checkExpr e = 
  case eguts e of
    Variable x -> (\x -> e{etype = x}) . ptype . def  <$> checkName x
    Sub e1 e2 -> do
      (v, s) <- liftM2 (,) (getType e1) (getType e2)
      varType <- checkSub v s
      return (e{etype = Just varType})
    Number n -> do
      return (e{etype = Just Integer})
    Monop w e1 -> do
      t <- getType e1
      varType <- checkMonop w t
      return (e {etype = Just varType})
    Binop w e1 e2 -> do
        ta <- getType e1
        tb <- getType e2
        varType <- checkBinop w ta tb 
        return (e {etype = Just varType})

getType :: Expr -> Check Type
getType e = checkExpr e >>= return . fromJust . etype

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

