module Main where

import Parse
import Kgen
import Types
import Check
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
  let Right checked = typeCheck program
  let (code) = translate (lines file) checked
  when dflag (print program)
  putStrLn "MODULE Main 0 0"
  putStrLn "IMPORT Lib 0" 
  putStrLn "ENDHDR\n" 
  
  putStrLn code
  
  let vars (Program (Block v _ _)) = v

  mapM_ (putStrLn . (\x -> "GLOVAR _" ++ x ++" 4")) (vars program)
  

  exit
  

 
exit    = exitSuccess
die     = exitWith (ExitFailure 1) 
