module Main where

import Parse
import Kgen
import Control.Applicative
import System.Environment
import System.Exit

main = do
  (x:xs) <- getArgs
  let dflag = case x of 
                "-d" -> True
                otherwise -> False
  file <- case xs of 
            [] -> if dflag then die else readFile x
            [f]  -> if dflag then readFile f else die 
            _ -> die
  let Right program = parse file
  print (translate program)



 
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1) 
