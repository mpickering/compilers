module Main where

import Parse
import Kgen
import Keiko (Code(..))
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
  
  putStrLn "MODULE Main 0 0"
  putStrLn "IMPORT Lib 0" 
  putStrLn "ENDHDR\n" 

  output (lines file) (translate program)

  putStrLn "RETURN"
  putStrLn "END\n" 
  

  exit
  

-- |output| -- output code sequence 
output :: [String] -> Code -> IO ()
output f (SEQ xs) = mapM_ (output f) xs
output f (NOP)    = return () 
output f (LINE n) = print $ "! " ++ f !! (n - 1) 
output f x        = print x
 
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1) 
