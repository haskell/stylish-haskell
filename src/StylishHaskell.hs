--------------------------------------------------------------------------------
module StylishHaskell
    ( runStep
    , runSteps
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Config
import           StylishHaskell.Parse
import           StylishHaskell.Step


--------------------------------------------------------------------------------
runStep :: Extensions -> Maybe FilePath -> Step -> Lines -> Lines
runStep exts mfp step ls = case parseModule exts mfp (unlines ls) of
    Left err      -> error err  -- TODO: maybe return original lines?
    Right module' -> stepFilter step ls module'


--------------------------------------------------------------------------------
runSteps :: Extensions -> Maybe FilePath -> [Step] -> Lines -> Lines
runSteps exts mfp = foldr (flip (.)) id . map (runStep exts mfp)
