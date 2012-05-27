module StylishHaskell.Tests.Util
    ( testStylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Parse
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
testStylish :: Stylish -> String -> String
testStylish stylish str = case parseModule Nothing str of
    Left err      -> error err
    Right module' -> unlines $ stylish ls module'
  where
    ls = lines str
