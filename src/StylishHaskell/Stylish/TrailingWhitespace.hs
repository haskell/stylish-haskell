--------------------------------------------------------------------------------
module StylishHaskell.Stylish.TrailingWhitespace
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Data.Char            (isSpace)


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse


--------------------------------------------------------------------------------
stylish :: Stylish
stylish ls _ = map dropTrailingWhitespace ls
