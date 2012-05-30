--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, listToMaybe)
import           System.Console.CmdArgs


--------------------------------------------------------------------------------
import           StylishHaskell
import           StylishHaskell.Stylish.Catalog


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
    filters = catMaybes $ map (`M.lookup` catalog)
        [ "Imports"
        , "LanguagePragmas"
        -- , "Tabs"
        , "TrailingWhitespace"
        ]
