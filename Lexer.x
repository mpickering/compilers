{
module Lexer where

import Types
import Keiko (Op(..))
import Data.Map as M (Map, lookup, fromList)
import Control.Monad.State
import Data.Char
}

%wrapper "monad"



$spaces = [\ \t]
$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]


tokens :- 
      [a-z][A-Za-z0-9_]*  { lookup' kwtable }
      [0-9]+              { mkL read NUMBER  }
      ";"                 { cM SEMI }
      "."                 { cM DOT }
     ":"                  { cM COLON  }
     "("                  { cM LPAR }
     ")"                  { cM RPAR }
     ","                  { cM COMMA }
     "="               { cM $ RELOP Eq }
     "+"               { cM $ ADDOP Plus }
     "-"               { cM MINUS }
     "*"               { cM $ ADDOP Times }
     "<"               { cM $ RELOP Lt}
     ">"               { cM $ RELOP Gt}
     "<>"              { cM $ RELOP Neq }
     "<="              { cM $ RELOP Leq }
     ">="              { cM $ RELOP Geq }
     ":="              { cM $ ASSIGN  }
     $white+           { skip }
     "(*"              { nested_comment }
     [\n]              { skip }
     "|"               { cM $ VBAR }
     _                 { cM $ BADTOK }

{
cM f _ b = return f 

nested_comment :: AlexInput -> Int -> Alex Token
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where 
    go 0 input = do alexSetInput input; alexMonadScan
    go n input = do
            case alexGetByte input of
              Nothing  -> err input
              Just (c,input) -> do
                case chr (fromIntegral c) of
                  '*' -> do
                    case alexGetByte input of
                      Nothing  -> err input
                      Just (41,input) -> go (n-1) input
                      Just (c,input)   -> go n input
                  '(' -> do
                    case alexGetByte input of
                      Nothing  -> err input
                      Just (c,input) | c == fromIntegral (ord '*') -> go (n+1) input
                      Just (c,input)   -> go n input
                  c -> go n input
    err input = do alexSetInput input; alexError "error in nested comment"  



lookup' :: M.Map Ident Token -> AlexInput -> Int -> Alex Token
lookup' m ((AlexPn _ p _), _, _, str) len  = return $ case l of
              Just l -> l
              Nothing -> IDENT t
  where 
    l = M.lookup t m
    t = take len str


mkL :: (String -> a) -> (a -> Token)  -> AlexInput -> Int -> Alex Token
mkL r c ((AlexPn _ p _), _, _, str) len = return $ c (r t) 
    where
        t = take len str

kws :: [(Ident, Token)]
kws = [ ("begin", BEGIN), ("do", DO), ("if", IF ), ("elsif", ELSIF), ("else", ELSE), 
      ("end", END), ("then", THEN), ("while", WHILE), ("print", PRINT),
      ("newline", NEWLINE), ("and", MULOP And), ("div", MULOP Div), 
      ("or", ADDOP Or), ("not", MONOP Not), ("mod", MULOP Mod), ("until", UNTIL), ("repeat", REPEAT),
      ("loop", LOOP), ("exit", EXIT), ("case", CASE), ("of", OF) ]

kwtable :: M.Map Ident Token
kwtable = fromList kws

alexEOF :: Alex (Token)
alexEOF = return EOF  

lexwrap = (alexMonadScan >>=)


}
