module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         ((>=>))
import           System.Environment    (getArgs)


--------------------------------------------------------------------------------
import           StylishHaskellImports


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> interact stylishHaskellImports
        _  -> mapM_ (readFile >=> putStr . stylishHaskellImports) args
