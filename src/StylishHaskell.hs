--------------------------------------------------------------------------------
module StylishHaskell
    ( runStep
    , runSteps
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>))
import           Control.Monad         (foldM)


--------------------------------------------------------------------------------
import           StylishHaskell.Config
import           StylishHaskell.Parse
import           StylishHaskell.Step


--------------------------------------------------------------------------------
runStep :: Extensions -> Maybe FilePath -> Lines -> Step -> Either String Lines
runStep exts mfp ls step =
    stepFilter step ls <$> parseModule exts mfp (unlines ls)


--------------------------------------------------------------------------------
runSteps :: Extensions -> Maybe FilePath -> [Step] -> Lines
         -> Either String Lines
runSteps exts mfp steps ls = foldM (runStep exts mfp) ls steps
