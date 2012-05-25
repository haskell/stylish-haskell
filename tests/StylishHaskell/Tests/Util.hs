module StylishHaskell.Tests.Util
    ( testStylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
testStylish :: (Lines -> Module -> [Change]) -> String -> String
testStylish stylish str = case parseModule Nothing str of
    Left err      -> error err
    Right module' -> unlines $ applyChanges (stylish ls module') ls
  where
    ls = lines str
