module Main where

import Clean

import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    (rlfile:rest) 
      -> case rest of
           ("compact-strings":_) -> compact rlfile
           ("find-repetition":_) -> repetitiveDefs rlfile
           ("sort-by-target":_) -> sortByTarget rlfile
           _ -> do compact rlfile
                   repetitiveDefs rlfile
                   sortByTarget rlfile
    _ -> usage

usage :: IO ()
usage = 
  do putStr "usage: stack exec cg-clean <file>.rlx " 
     putStrLn " [ compact-strings | find-repetition | sort-by-target ]"
     putStrLn "(If no option given after rule file, it does everything.)"
     putStr "Output will be generated in <file>.compact.rlx, " 
     putStrLn " <file>.repetitive.txt, <file>.sorted.rlx."
    
