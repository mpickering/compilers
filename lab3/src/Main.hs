module Main where

import Parse
import Kgen
import Types
import Dict(typeSize)
import Keiko (Code(..))
import Check
import Control.Applicative
import System.Environment
import System.Exit
import Control.Monad
import Data.List (intercalate)

main = do
  (x:xs) <- getArgs
  let dflag = case x of 
                "-d" -> True
                otherwise -> False
  file <- case xs of 
            [] -> if dflag then die else readFile x
            [f]  -> if dflag then readFile f else die 
            _ -> die
  program@(Program ds _) <- case parse file of
                Left e -> error e
                Right p -> return p 
  let checked = typeCheck program 
  let Right c = checked
  let code = translate c
  when dflag (print program)
  putStrLn "MODULE Main 0 0"
  putStrLn "IMPORT Lib 0" 
  putStrLn "ENDHDR\n" 

  putStrLn "PROC MAIN 0 0 0"
  output (lines file) code

  putStrLn "RETURN"
  putStrLn "END\n" 
  
  let showDecl (Decl xs t) = map (\x -> "GLOVAR _" ++ name x ++ " " ++ show s) xs
        where
          s = typeSize t
      
  
  putStrLn (intercalate "\n" (concatMap showDecl ds))
  

  exit
  


-- |output| -- output code sequence 
output :: [String] -> Code -> IO ()
output f (SEQ xs) = mapM_ (output f) xs
output f (NOP)    = return () 
output f (LINE n) = putStrLn $ "! " ++ f !! (n - 1) 
output f x        = print x
 
exit    = exitSuccess
die     = exitWith (ExitFailure 1) 
