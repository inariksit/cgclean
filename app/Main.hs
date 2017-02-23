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
     putStrLn " <file>.repetitive.txt, <file>.sorted.txt"
    
-------------------------------------------------------------------------------

compact :: FilePath -> IO ()
compact rlfile = do
  putStrLn "Changing wordforms into case-insensitive regular expressions."
  putStrLn "Then, remove redundant rules:"
  putStrLn "e.g.  `REMOVE \"<x>\" if y'   and   `REMOVE \"<X>\" IF y'"
  putStrLn "will be merged into one rule."
  putStrLn ""
  compactGrammar <- printGrammar True `fmap` readFile rlfile
  let compactName = takeBaseName rlfile ++ ".compact.rlx"
  writeFile compactName compactGrammar

repetitiveDefs :: FilePath -> IO ()
repetitiveDefs rlfile = do
  putStrLn "Finding repetitive set definitions"
  putStrLn ""
  (defs,rules) <- parse False `fmap` readFile rlfile

  let repDefs = [ (def, groupRepetitiveTagset def)
                   | (name,def) <- defs
                    , not $ null (groupRepetitiveTagset def) ]

  let repFileName = takeBaseName rlfile ++ ".repetitive.txt"

  writeFile repFileName (concatMap showRepDef repDefs)

 where
  showCompDef (nm,def,c) = "\nOriginal definition:\n"
                           ++ (nm ++ " = " ++ showInline def)
                           ++ "\nCompact tagsets:\n"
                           ++ (nm ++ " = " ++ showInline c) ++ "\n"
  showRepDef (def,cdef) = "\nOriginal tagset:"
                       ++ "\n----------------\n"
                       ++ (show def ++ " = " ++ showInline def)
                       ++ "\n\nSome possibilities for regrouping:"
                       ++ "\n----------------------------------\n"
                       ++ unlines (breakInSets def `fmap` cdef)
                       ++ "\n=====================================\n"

-- | Given the original tagset and the compacted version, print it nicely
breakInSets :: TagSet -> TagSet -> String
breakInSets odef (Cart l@(Set _ lexs) m@(Set _ mors)) 
    | length (getOrList lexs) == 1 
         = unlines [ "LIST " ++ morSetName ++ " = " ++ showInline m ++ " ;"
                   , "SET " ++ show odef ++ " = " ++ showInline l ++ " + " ++ morSetName ++ " ;"]
    | length (getOrList mors) == 1
        = unlines [ "LIST " ++ lexSetName ++ " = " ++ showInline l ++ " ;"
                  , "SET " ++ show odef ++ " = " ++ lexSetName ++ " + " ++ showInline m ++ " ;"]

    | otherwise 
        = unlines [ "LIST " ++ lexSetName ++ " = " ++ showInline l ++ " ;"
                  , "LIST " ++ morSetName ++ " = " ++ showInline m ++ " ;"
                  , "SET " ++ show odef ++ " = " ++ lexSetName ++ " + " ++ morSetName ++ " ;"]
 where
  lexSetName = show odef ++ "_LEX"
  morSetName = show odef ++ "_MORPH"

breakInSets _ x = show x

-------------------------------------------------------------------------------


-- | This should use the SAT-based methods eventually, 
-- as soon as I get it to work properly for Basque.
sortByTarget :: FilePath -> IO ()
sortByTarget rlfile = do
  putStrLn "Grouping rules by targets"
  putStrLn ""
  let compact = True
  (defs,rules) <- parse compact `fmap` readFile rlfile

  let groupedRls = groupRules (concat rules) :: [[Rule]]
  let groupedSortedRls = map sortByContext groupedRls
  
  let sortedFile = takeBaseName rlfile ++ ".sorted.txt"

  --writeFile sortedFile (printDefs compact rlfile)
 
  writeFile sortedFile (unlines $ map showGroup groupedSortedRls)

 where
  showGroup :: [Rule] -> String
  showGroup rules = 
    let trg = show $ target (head rules)
     in unlines $ ("\n# Rules that target " ++ trg ++ "\n"):
                   map show rules

-- Maybe add a proper printDefs function in CGHS.Parse
printDefs :: Bool -> String -> String
printDefs b = unlines . filter isSet . lines . printGrammar b
 where 
  isSet ('S':'E':'T':_)     = True
  isSet ('L':'I':'S':'T':_) = True
  isSet _ = False 