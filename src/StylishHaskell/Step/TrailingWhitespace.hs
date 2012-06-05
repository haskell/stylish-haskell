--------------------------------------------------------------------------------
module StylishHaskell.Step.TrailingWhitespace
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Char            (isSpace)


--------------------------------------------------------------------------------
import           StylishHaskell.Step


--------------------------------------------------------------------------------
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse


--------------------------------------------------------------------------------
step :: Step
step ls _ = map dropTrailingWhitespace ls
