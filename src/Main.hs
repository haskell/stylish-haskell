--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative               ((<$>))
import           Data.Maybe                        (listToMaybe)
import           System.Environment                (getArgs)


--------------------------------------------------------------------------------
import qualified StylishHaskell.Imports
import qualified StylishHaskell.LanguagePragmas
import           StylishHaskell.Parse
import           StylishHaskell.Stylish
import qualified StylishHaskell.Tabs
import qualified StylishHaskell.TrailingWhitespace


--------------------------------------------------------------------------------
runStylish :: Maybe FilePath -> Stylish -> Lines -> Lines
runStylish mfp f ls = case parseModule mfp (unlines ls) of
    Left err      -> error err  -- TODO: maybe return original lines?
    Right module' -> f ls module'


--------------------------------------------------------------------------------
chainStylish :: Maybe FilePath -> [Stylish] -> Lines -> Lines
chainStylish mfp filters = foldr (.) id filters'
  where
    filters' :: [Lines -> Lines]
    filters' = map (runStylish mfp) filters


--------------------------------------------------------------------------------
main :: IO ()
main = do
    filePath <- listToMaybe <$> getArgs
    contents <- maybe getContents readFile filePath
    putStr $ unlines $ chainStylish filePath filters $ lines contents
  where
    filters =
        [ StylishHaskell.Imports.stylish
        , StylishHaskell.LanguagePragmas.stylish
        , StylishHaskell.Tabs.stylish
        , StylishHaskell.TrailingWhitespace.stylish
        ]
