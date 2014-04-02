module Main where

import Parse
import Kgen
import Keiko (Code(..))
import Control.Applicative
import System.Environment
import System.Exit
import Control.Monad

main = do
  (x:xs) <- getArgs
  let dflag = case x of 
                "-d" -> True
                otherwise -> False
  file <- case xs of 
            [] -> if dflag then die else readFile x
            [f]  -> if dflag then readFile f else die 
            _ -> die
  program <- case parse file of
                Left e -> error e
                Right p -> return p
  when dflag (print program)
  
  output (lines file) program
  
  exit
  

 
exit    = exitSuccess
die     = exitWith (ExitFailure 1) 
