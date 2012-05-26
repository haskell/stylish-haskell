--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Data.Maybe             (listToMaybe)
import           System.Environment     (getArgs)


--------------------------------------------------------------------------------
import           StylishHaskell.Imports
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
stylishHaskell :: Maybe FilePath -> String -> IO ()
stylishHaskell mfp string = case parseModule mfp string of
    Left err      -> error err
    Right module' -> do
        let ls = stylish (lines string) module'
        putStr $ unlines ls


--------------------------------------------------------------------------------
main :: IO ()
main = do
    filePath <- listToMaybe <$> getArgs
    contents <- maybe getContents readFile filePath
    stylishHaskell filePath contents
