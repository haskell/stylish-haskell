--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                        (listToMaybe)
import           System.Console.CmdArgs


--------------------------------------------------------------------------------
import           StylishHaskell
import qualified StylishHaskell.Imports
import qualified StylishHaskell.LanguagePragmas
import qualified StylishHaskell.TrailingWhitespace


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
    sa <- cmdArgs stylishArgs
    let filePath = listToMaybe $ files sa
    contents <- maybe getContents readFile filePath
    putStr $ unlines $ chainStylish filePath filters $ lines contents
  where
    filters =
        [ StylishHaskell.Imports.stylish
        , StylishHaskell.LanguagePragmas.stylish
        -- , StylishHaskell.Tabs.stylish
        , StylishHaskell.TrailingWhitespace.stylish
        ]
