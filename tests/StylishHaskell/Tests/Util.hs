module StylishHaskell.Tests.Util
    ( testStep
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Parse
import           StylishHaskell.Step


--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep step str = case parseModule Nothing str of
    Left err      -> error err
    Right module' -> unlines $ step ls module'
  where
    ls = lines str
