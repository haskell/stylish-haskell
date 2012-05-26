module StylishHaskell.Tests.Util
    ( testStylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
testStylish :: (Lines -> Module -> Lines) -> String -> String
testStylish stylish str = case parseModule Nothing str of
    Left err      -> error err
    Right module' -> unlines $ stylish ls module'
  where
    ls = lines str
