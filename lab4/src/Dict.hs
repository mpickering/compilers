
module Dict where

import Data.Map as M (Map, lookup, insert, member, empty)
import Data.Maybe

type Ident = String

data Def = Def 
    { tag   :: Ident
    , kind  :: Kind   
    , level :: Int
    , lab   :: String
    , off   :: Int
    } deriving (Ord, Show, Eq)  


data Kind = VarDef | ProcDef Int deriving (Eq, Show, Ord)


data Environment = Env [Def] (M.Map Ident Def)

empty :: Environment
empty = Env [] M.empty

define :: Def -> Environment -> Environment
define d e@(Env l m) 
  | d `elem` l = e 
  | otherwise = Env (d:l) (M.insert (tag d) d m) 

lookup :: Ident -> Environment -> Maybe Def
lookup i (Env l m) = M.lookup i m 

--member :: Ident -> Environment -> Bool
--member = M.member

newBlock :: Environment -> Environment
newBlock (Env l m) = Env [] m



