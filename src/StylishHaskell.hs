--------------------------------------------------------------------------------
module StylishHaskell
    ( runStylish
    , chainStylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Parse
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
runStylish :: Maybe FilePath -> Stylish -> Lines -> Lines
runStylish mfp f ls = case parseModule mfp (unlines ls) of
    Left err      -> error err  -- TODO: maybe return original lines?
    Right module' -> f ls module'


--------------------------------------------------------------------------------
chainStylish :: Maybe FilePath -> [Stylish] -> Lines -> Lines
chainStylish mfp = foldr (flip (.)) id . map (runStylish mfp)
