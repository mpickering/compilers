module Dict where

import Data.Map as M (Map, lookup, insert, member, empty)
import Data.Maybe

type Ident = String

data Def = Def 
    { tag   :: Ident
    , ptype :: Maybe Type   
    , lab   :: String
    } deriving (Ord, Eq)  

instance Show Def where
  show = tag

data Type = Integer | Boolean | Array Int Type  deriving (Eq, Show, Ord)

type Environment = M.Map Ident Def

empty :: Environment
empty = M.empty

define :: Def -> Environment -> Environment
define = insert =<< tag 

lookup :: Ident -> Environment -> Maybe Def
lookup = M.lookup  

member :: Ident -> Environment -> Bool
member = M.member

typeSize :: Type -> Int
typeSize (Integer)   = 4
typeSize (Boolean)   = 1
typeSize (Array n t) = n * typeSize t

isArray :: Type -> Bool
isArray (Array _ _) = True
isArray _ = False

baseType :: Type -> Type 
baseType (Array _ t) = t
baseType _ = error "Base type not found" 
