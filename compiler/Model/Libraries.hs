module Libraries (libraries, addPrelude) where

import Ast
import qualified Data.Map as Map
import Data.List (inits)
import PlatformUtils (getLibs)

addPrelude :: Module -> Module
addPrelude (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where customIms = concatMap addModule prelude

          addModule (n, method) = case lookup n ims of
                                    Nothing     -> [(n, method)]
                                    Just (As m) -> [(n, method)]
                                    Just _      -> []

prelude = text : map (\n -> (n, Hiding [])) modules
  where
    text = ("Text", Hiding ["link", "color", "height"])
    modules = [ "Prelude", "Signal", "List", "Maybe", "Time"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

libraries :: Map.Map String (Map.Map String String)
libraries = Map.unionWith Map.union getLibs nilAndTuples
    where nilAndTuples = Map.singleton "Prelude" (Map.fromList pairs)
          pairs =
              [ ("Cons", "a -> [a] -> [a]")
              , ("Nil", "[a]")
              ] ++ map makeTuple (inits ['a'..'i'])

          makeTuple cs =
              let name = "Tuple" ++ show (length cs)
              in  (name, concatMap (\c -> c : " -> ") cs ++
                         name ++ concatMap (\c -> [' ',c]) cs)
