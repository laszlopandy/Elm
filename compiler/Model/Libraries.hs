module Libraries (libraries, addPrelude) where

import Ast
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import Data.List (inits)
import Text.JSON
import PlatformUtils (loadDocs)

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
libraries =
    case getLibs of
      Error err -> error err
      Ok libs   -> Map.unionWith Map.union libs nilAndTuples
          where nilAndTuples = Map.singleton "Prelude" (Map.fromList pairs)
                pairs =
                    [ ("Cons", "a -> [a] -> [a]")
                    , ("Nil", "[a]")
                    ] ++ map makeTuple (inits ['a'..'i'])
                
                makeTuple cs = 
                    let name = "Tuple" ++ show (length cs)
                    in  (name, concatMap (\c -> c : " -> ") cs ++
                               name ++ concatMap (\c -> [' ',c]) cs)

getLibs :: Result (Map.Map String (Map.Map String String))
getLibs = do
  obj <- decodeStrict loadDocs :: Result (JSObject JSValue)
  modules <- valFromObj "modules" obj :: Result [JSObject JSValue]
  Map.fromList `fmap` mapM getValues modules


get :: String -> JSObject JSValue -> Result String
get = valFromObj

getValue :: JSObject JSValue -> Result (String,String)
getValue obj = (,) <$> get "name" obj <*> get "type" obj

getValues :: JSObject JSValue -> Result (String, Map.Map String String)
getValues obj = do
  name <- get "name" obj
  vs   <- valFromObj "values" obj
  vals <- mapM getValue vs
  return (name, Map.fromList vals)