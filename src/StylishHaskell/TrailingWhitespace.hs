--------------------------------------------------------------------------------
module StylishHaskell.TrailingWhitespace
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Data.Char            (isSpace)


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse


--------------------------------------------------------------------------------
stylish :: Lines -> Module -> Lines
stylish ls _ = map dropTrailingWhitespace ls
