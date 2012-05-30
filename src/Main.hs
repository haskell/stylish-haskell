--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe             (listToMaybe)
import           System.Console.CmdArgs


--------------------------------------------------------------------------------
import           StylishHaskell
import           StylishHaskell.Config


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { config :: Maybe FilePath
    , files  :: [FilePath]
    } deriving (Data, Show, Typeable)


--------------------------------------------------------------------------------
stylishArgs :: StylishArgs
stylishArgs = StylishArgs
    { config = Nothing &= typFile &= help "Configuration file"
    , files  = []      &= typFile &= args
    } &= summary "stylish-haskell"


--------------------------------------------------------------------------------
main :: IO ()
main = do
    sa   <- cmdArgs stylishArgs
    conf <- loadConfig (config sa)
    let filePath = listToMaybe $ files sa
        stylish  = configStylish conf
    contents <- maybe getContents readFile filePath
    putStr $ unlines $ chainStylish filePath stylish $ lines contents
