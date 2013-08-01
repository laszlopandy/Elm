module Initialize (buildFromSource, getSortedModuleNames) where

import Data.List (lookup,nub)
import qualified Data.Map as Map

import Data.List (intercalate,partition)
import Parse.Parser (parseDependencies)
import qualified Libraries as Libs
import Build (buildFromSource)
import System.Exit
import System.FilePath

getSortedModuleNames :: FilePath -> IO [String]
getSortedModuleNames root =
    sortDeps =<< readDeps [] root

type Deps = (String, [String])

sortDeps :: [Deps] -> IO [String]
sortDeps deps = go [] (nub deps)
    where
      msg = "A cyclical or missing module dependency or was detected in: "

      go :: [String] -> [Deps] -> IO [String]
      go sorted [] = return (map toFilePath sorted)
      go sorted unsorted =
          case partition (all (`elem` sorted) . snd) unsorted of
            ([],m:ms) -> do putStrLn (msg ++ intercalate ", " (map fst (m:ms)) ++ show sorted ++ show unsorted)
                            exitFailure
            (srtd,unsrtd) -> go (sorted ++ map fst srtd) unsrtd

readDeps :: [FilePath] -> FilePath -> IO [Deps]
readDeps seen root = do
  txt <- readFile root
  case parseDependencies txt of
    Left err ->
        let msg = "Error resolving dependencies in " ++ root ++ ":\n" in
        putStrLn (msg ++ err) >> exitFailure
    Right (name,deps) ->
        do rest <- mapM (readDeps seen' . toFilePath) newDeps
           return ((name, realDeps) : concat rest)
        where
          realDeps = filter (`notElem` builtIns) deps
          newDeps = filter (\d -> d `notElem` seen && not (isNative d)) realDeps
          seen' = root : seen ++ newDeps
          builtIns = Map.keys Libs.libraries
                       

isNative name = takeWhile (/='.') name == "Native"

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
