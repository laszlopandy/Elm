module PlatformUtils where

import Haste (toJSString, JSString)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall consoleLog :: JSString -> IO ()

loadDocs :: String
loadDocs = "{}"

printWarning :: String -> ()
printWarning msg = unsafePerformIO (consoleLog (toJSString msg))