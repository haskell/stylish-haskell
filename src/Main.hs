--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative               ((<$>))
import           Data.Maybe                        (listToMaybe)
import           System.Environment                (getArgs)


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import qualified StylishHaskell.Imports
import qualified StylishHaskell.LanguagePragmas
import           StylishHaskell.Parse
import qualified StylishHaskell.TrailingWhitespace


--------------------------------------------------------------------------------
runFilter :: Maybe FilePath -> (Lines -> Module -> Lines) -> Lines -> Lines
runFilter mfp f ls = case parseModule mfp (unlines ls) of
    Left err      -> error err  -- TODO: maybe return original lines?
    Right module' -> f ls module'


--------------------------------------------------------------------------------
runFilters :: Maybe FilePath -> [Lines -> Module -> Lines] -> Lines -> Lines
runFilters mfp filters = foldr (.) id filters'
  where
    filters' :: [Lines -> Lines]
    filters' = map (runFilter mfp) filters


--------------------------------------------------------------------------------
main :: IO ()
main = do
    filePath <- listToMaybe <$> getArgs
    contents <- maybe getContents readFile filePath
    putStr $ unlines $ runFilters filePath filters $ lines contents
  where
    filters =
        [ StylishHaskell.Imports.stylish
        , StylishHaskell.LanguagePragmas.stylish
        , StylishHaskell.TrailingWhitespace.stylish
        ]
