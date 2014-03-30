module Keiko where

import Data.Char

-- |codelab| -- type of code labels 
type Codelab = Int

data Op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA deriving (Show, Eq, Ord)

data Code =
    CONST Int                -- Push constant (value) 
  | GLOBAL String            -- Push global address (name) 
  | LOCAL Int                -- Push local adddress (offset) 
  | LOADW                       -- Load word 
  | STOREW                      -- Store word 
  | LOADC                       -- Load character 
  | STOREC                      -- Store character 
  | LDGW String              -- Load value (name) 
  | STGW String              -- Store (name) 
  | MONOP Op                 -- Perform unary operation (op) 
  | BINOP Op                 -- Perform binary operation (op) 
  | LABEL Codelab            -- Set code label 
  | JUMP Codelab             -- Unconditional branch (dest) 
  | JUMPB Bool Codelab     -- Branch on boolean (val, dest) 
  | JUMPC Op Codelab       -- Conditional branch (op, dest) 
  | PCALL Int                -- Call procedure 
  | PCALLW Int              -- Proc call with result (nargs) 
  | RETURNW                     -- Return from procedure 
  | BOUND Int             -- Bounds check 
  | CASEJUMP Int          -- Case jump (num cases) 
  | CASEARM Int Codelab    -- Case value and label 
  | PACK                        -- Pack two values into one 
  | UNPACK                      -- Unpack one value into two 
  | LINE Int
  | SEQ [Code] 
  | NOP deriving (Eq, Ord)



-- opName -- map an operator to its name *)
opName = map toUpper . show 

instance Show Code where
  show = fInst 

-- |fInst| -- format an instruction for |printf| *)
fInst :: Code -> String
fInst c = case c of
  CONST x ->        "CONST " ++ show x
  GLOBAL a ->       "GLOBAL " ++ a
  LOCAL n ->        "LOCAL " ++ show n
  LOADW ->          "LOADW"
  STOREW ->         "STOREW"
  LOADC ->          "LOADC"
  STOREC ->         "STOREC"
  LDGW a ->         "LDGW " ++ a
  STGW a ->         "STGW " ++ a
  MONOP w ->        opName w
  BINOP w ->        opName w
  LABEL l ->        "LABEL " ++ show l
  JUMP l ->         "JUMP " ++ show l 
  JUMPB b l ->   if b then "JUMPT" else "JUMPF" ++ " " ++ show l 
  JUMPC w l ->   "J" ++ opName w ++ " " ++ show l
  PCALL n ->        "PCALL " ++ show n
  PCALLW n ->       "PCALLW " ++ show n
  RETURNW ->        "RETURNW"
  BOUND n ->        "BOUND " ++ show n
  CASEJUMP n ->     "CASEJUMP " ++ show n
  CASEARM v l -> "CASEARM " ++ show v ++ " " ++ show l 
  PACK ->           "PACK"
  UNPACK ->         "UNPACK"

  LINE n ->         "LINE " ++ show n 
  SEQ _ ->          "SEQ ..."
  NOP ->            "NOP"


