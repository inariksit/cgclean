module Main where


import CGHS
import CGHS.Compact 

import System.Environment ( getArgs )
import System.FilePath ( takeBaseName )

main :: IO ()
main = do
  args <- getArgs
  case args of
    (rlfile:rest) 
      -> case rest of
           ("all":_) -> do compact rlfile
                           repetitiveDefs rlfile
                           sortByTarget rlfile
           ("compact-strings":_) -> compact rlfile
           ("find-repetition":_) -> repetitiveDefs rlfile
           ("sort-by-target":_) -> sortByTarget rlfile
           _ -> usage
    _ -> usage

usage :: IO ()
usage = do putStr   "usage: stack exec cg-clean <file>.rlx " 
           putStrLn " < all | compact-strings | find-repetition | sort-by-target >"
           putStrLn "Output will be generated in <file>.compact.rlx, <file>.repetitive.txt, <file>.sorted.rlx"
    
compact :: FilePath -> IO ()
compact rlfile = do
  compactGrammar <- printGrammar True `fmap` readFile rlfile
  let compactName = takeBaseName rlfile ++ ".compact.rlx"
  writeFile compactName compactGrammar

repetitiveDefs :: FilePath -> IO ()
repetitiveDefs rlfile = do
  (defs,rules) <- parse False `fmap` readFile rlfile

  -- Compacting a tagset of the form ("a" c) OR ("a" d) OR ("b" c) OR ("b" d),
  -- into ("a" OR "b") + (c OR d).
  let compactDefs = [ compactTagset def | (name,def) <- defs ]
  let diffDefs = [ (nm,odef,comp) 
                    | ((nm,odef),comp) <- zip defs compactDefs
                    , odef /= comp ]

  
  -- Tagsets with some repetition, but not entirely like the previous:
  let repDefs = [ (,) (nm,def) [ (rd,occs)
                    | (rd,occs) <- findRepetition def 
                    , occs > 2 ]
                  | (nm,def) <- defs ]
  --TODO: take the tagsets that have >2 occurrences (groupBy or something)
  --and do compactTagset to them.
  -- Then show the new tagset like "this is how you could split <xyz>; 
  -- these <åäö> are still different"

  let repFileName = takeBaseName rlfile ++ ".repetitive.txt"
  writeFile repFileName (concatMap showCompDef diffDefs)
  appendFile repFileName (concatMap showRepDef repDefs)

 where
  showCompDef (nm,def,c) = "\nOriginal definition:\n"
                           ++ (nm ++ " = " ++ showInline def)
                           ++ "\nCompact tagsets:\n"
                           ++ (nm ++ " = " ++ showInline c) ++ "\n"
  showRepDef (_,[])   = []
  showRepDef ((nm,def),occs) = "\nOriginal definition:\n"
                             ++ (nm ++ " = " ++ showInline def)
                             ++ "\nRepetitive elements:\n"
                             ++ (unwords $ map (\(x,y) -> (show x ++ ": " ++ show y)) occs)
                             ++ "\n"
                          
-- | This should use the SAT-based methods eventually, as soon as I get it work properly for Basque.
sortByTarget :: FilePath -> IO ()
sortByTarget rules = putStrLn "sortByTarget: not implemented yet"