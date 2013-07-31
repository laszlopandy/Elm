module PlatformUtils (getLibs, printWarning) where

import LoadLibraries as Libs
import System.IO.Unsafe (unsafePerformIO)
import Text.JSON
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map

printWarning :: String -> ()
printWarning msg = unsafePerformIO (putStrLn msg) `seq` ()

getLibs :: Map.Map String (Map.Map String String)
getLibs = case getLibsFromJson of
	Error err -> error err
	Ok libs -> libs

getLibsFromJson :: Result (Map.Map String (Map.Map String String))
getLibsFromJson = do
  obj <- decodeStrict Libs.docs :: Result (JSObject JSValue)
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
