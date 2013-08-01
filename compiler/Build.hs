module Build (buildFromSource) where

import Control.Monad.Error

import Ast
import Optimize
import Types.Hints (hints)
import Types.Unify (unify)
import Types.Alias (mistakes)
import Rename
import Parse.Parser (parseProgram)
import qualified Libraries as Libs

checkMistakes :: Module -> Either String Module
checkMistakes modul@(Module name ex im stmts) = 
  case mistakes stmts of
    m:ms -> Left (unlines (m:ms))
    []   -> return modul

checkTypes :: Module -> Either String Module
checkTypes modul =
  do subs <- unify hints modul
     subs `seq` return (optimize (renameModule modul))

check :: Module -> Either String Module
check = checkMistakes >=> checkTypes

buildFromSource :: Bool -> String -> Either String Module
buildFromSource noPrelude src =
    let add = if noPrelude then id else Libs.addPrelude in
    (check . add) =<< (parseProgram src)