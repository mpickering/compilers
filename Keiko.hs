module Keiko where

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
  | NOP deriving (Show, Eq, Ord)


{-
-- op_name -- map an operator to its name *)
opName = (map toUpper) . show 

-- |fInst| -- format an instruction for |printf| *)
fInstr :: Code -> String
fInst c = case c of
  CONST x ->        fMeta "CONST $" [fNum x]
  GLOBAL a ->       fMeta "GLOBAL $" [fStr a]
  LOCAL n ->        fMeta "LOCAL $" [fNum n]
  LOADW ->          fStr "LOADW"
  STOREW ->         fStr "STOREW"
  LOADC ->          fStr "LOADC"
  STOREC ->         fStr "STOREC"
  LDGW a ->         fMeta "LDGW $" [fStr a]
  STGW a ->         fMeta "STGW $" [fStr a]
  MONOP w ->        fMeta "$" [fStr (op_name w)]
  BINOP w ->        fMeta "$" [fStr (op_name w)]
  LABEL l ->        fMeta "LABEL $" [fLab l]
  JUMP l ->         fMeta "JUMP $" [fLab l]
  JUMPB (b, l) ->   fMeta "$ $" 
                      [fStr (if b then "JUMPT" else "JUMPF"); fLab l]
  JUMPC (w, l) ->   fMeta "J$ $" [fStr (op_name w); fLab l]
  PCALL n ->        fMeta "PCALL $" [fNum n]
  PCALLW n ->       fMeta "PCALLW $" [fNum n]
  RETURNW ->        fStr "RETURNW"
  BOUND n ->        fMeta "BOUND $" [fNum n]
  CASEJUMP n ->     fMeta "CASEJUMP $" [fNum n]
  CASEARM (v, l) -> fMeta "CASEARM $ $" [fNum v; fLab l]
  PACK ->           fStr "PACK"
  UNPACK ->         fStr "UNPACK"

  LINE n ->         fMeta "LINE $" [fNum n]
  SEQ _ ->          fStr "SEQ ..."
  NOP ->            fStr "NOP"

-- |output| -- output code sequence *)
let output code =
  let line = ref 0 in
  let rec out =
    function 
        SEQ xs -> List.iter out xs
      | NOP -> ()
      | LINE n -> 
          if n <> 0 && !line <> n then begin
            printf "! $\n" [fStr (Source.get_line n)];
            line := n
          end
      | x -> printf "$\n" [fInst x] in
  out code
-}
