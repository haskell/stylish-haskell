--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.TrailingWhitespace
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Char           (isSpace)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse


--------------------------------------------------------------------------------
step :: Step
step = makeStep "TrailingWhitespace" $ \ls _ -> map dropTrailingWhitespace' ls
  where
    dropTrailingWhitespace' l = case l of
      -- Preserve page breaks
      "\12" -> l
      _     -> dropTrailingWhitespace l
