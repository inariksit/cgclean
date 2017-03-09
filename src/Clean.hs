module Clean
    ( compact
    , repetitiveDefs
    , sortByTarget
    ) where


import CGHS
import CGHS.Compact

import CGSAT
import Analyse

import SAT ( newSolver, deleteSolver )

import System.FilePath ( takeBaseName )

-------------------------------------------------------------------------------

compact :: FilePath -> IO ()
compact rlfile = do
  compactGrammar <- printGrammar True `fmap` readFile rlfile
  let compactName = takeBaseName rlfile ++ ".compact.rlx"
  writeFile compactName compactGrammar

-------------------------------------------------------------------------------

repetitiveDefs :: FilePath -> IO ()
repetitiveDefs rlfile = do
  let compact = False
  (defs,rules) <- parse compact `fmap` readFile rlfile

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
  grammar <- readFile rlfile
  let compact = False
  let (defs,rules) = parse compact grammar
  let groupedRls = map sortByContext $ groupRules $ concat rules

  s <- newSolver
  (env,_) <- envRules ("eus",[]) s -- TODO: parametrise for any other language
  conflicts <- mapM (satCheck env) (take 2 groupedRls)

  putStrLn "Found the following conflicts:"
  mapM_ (mapM_ print) conflicts

  deleteSolver s

  let sortedFile = takeBaseName rlfile ++ ".sorted.rlx"

  writeFile sortedFile $ printDefs compact grammar
  appendFile sortedFile $ showGroups groupedRls

 where
  showGroups = unlines . concatMap showGroup

  showGroup rules = 
    let trg = show $ target (head rules)
     in ("\nSECTION\n# Rules that target " ++ trg ++ "\n"):
        map show rules

-- Maybe add a proper printDefs function in CGHS.Parse
printDefs :: Bool -> String -> String
printDefs b = unlines . filter isSet . lines . printGrammar b
 where 
  isSet ('D':'E':'L':_)     = True
  isSet ('S':'E':'T':_)     = True
  isSet ('L':'I':'S':'T':_) = True
  isSet _ = False 

satCheck :: Env -> [Rule] -> IO [(Rule,Conflict)]
satCheck env rules = do
  let largestWidth = maximum $ map (fst . width) rules
  config <- evalRWSE env (mkConfig largestWidth)
  (maybeConflicts,_,log_) <- rwse env config $ testRules True rules

  mapM_ putStrLn log_
  case maybeConflicts of
    Left err -> do print err
                   return []
    Right cs -> return (onlyConflicts rules cs)

onlyConflicts :: [Rule] -> [Conflict] -> [(Rule,Conflict)]
onlyConflicts rules confs = filter (isConflict.snd) $ zip rules confs

