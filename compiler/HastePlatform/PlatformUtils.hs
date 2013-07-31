module PlatformUtils (getLibs, printWarning) where

import Haste (toJSString, fromJSStr, JSString)
import Haste.Prim (Ptr, fromPtr)
import Haste.JSON (JSON(Str, Arr), (~>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map


foreign import ccall consoleLog :: JSString -> IO ()
foreign import ccall "getElmDocs" getElmDocs :: Ptr JSON


printWarning :: String -> ()
printWarning msg = unsafePerformIO (consoleLog (toJSString msg))

getLibs :: Map.Map String (Map.Map String String)
getLibs = case getLibsFromDocs of
    Just libs -> libs
    Nothing -> error "Cannot read elm docs JSON"

getLibsFromDocs :: Maybe (Map.Map String (Map.Map String String))
getLibsFromDocs = do
    let json = fromPtr getElmDocs
    mods <- getArray =<< get json "modules"
    values <- mapM getValues mods
    return (Map.fromList values)
  where getValues obj = do
            name <- getStr =<< get obj "name"
            jsonValues <- getArray =<< get obj "values"
            values <- mapM getValue jsonValues
            return (name, Map.fromList values)
        getValue obj = do
            name <- getStr =<< get obj "name"
            t <- getStr =<< get obj "type"
            return (name, t)

get :: JSON -> String -> Maybe JSON
get json s = json ~> (toJSString s)

getStr :: JSON -> Maybe String
getStr x =
    case x of
        Str jsStr -> Just (fromJSStr jsStr)
        _ -> Nothing

getArray :: JSON -> Maybe [JSON]
getArray x =
    case x of
        Arr objs -> Just objs
        _ -> Nothing