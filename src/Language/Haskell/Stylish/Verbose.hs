--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Verbose
    ( Verbose
    , makeVerbose
    ) where


--------------------------------------------------------------------------------
import           System.IO (hPutStrLn, stderr)


--------------------------------------------------------------------------------
type Verbose = String -> IO ()


--------------------------------------------------------------------------------
makeVerbose :: Bool -> Verbose
makeVerbose verbose
    | verbose   = hPutStrLn stderr
    | otherwise = const $ return ()
