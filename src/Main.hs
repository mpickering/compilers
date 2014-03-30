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
  let (code, vars) = translate program 
  when dflag (print program)
  putStrLn "MODULE Main 0 0"
  putStrLn "IMPORT Lib 0" 
  putStrLn "ENDHDR\n" 

  putStrLn "PROC MAIN 0 0 0"
  output (lines file) code

  putStrLn "RETURN"
  putStrLn "END\n" 

  mapM_ (putStrLn . (\x -> "GLOVAR _" ++ x ++" 4")) vars
  

  exit
  

-- |output| -- output code sequence 
output :: [String] -> Code -> IO ()
output f (SEQ xs) = mapM_ (output f) xs
output f (NOP)    = return () 
output f (LINE n) = putStrLn $ "! " ++ f !! (n - 1) 
output f x        = print x
 
exit    = exitSuccess
die     = exitWith (ExitFailure 1) 
