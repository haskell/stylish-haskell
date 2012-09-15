module Language.Haskell.Stylish.Tests.Util
    ( testStep
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep step str = case parseModule [] Nothing str of
    Left err      -> error err
    Right module' -> unlines $ stepFilter step ls module'
  where
    ls = lines str
