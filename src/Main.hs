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
import qualified StylishHaskell.Stylish.Imports
import qualified StylishHaskell.Stylish.LanguagePragmas
import qualified StylishHaskell.Stylish.TrailingWhitespace


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
        [ StylishHaskell.Stylish.Imports.stylish
        , StylishHaskell.Stylish.LanguagePragmas.stylish
        -- , StylishHaskell.Tabs.stylish
        , StylishHaskell.Stylish.TrailingWhitespace.stylish
        ]
