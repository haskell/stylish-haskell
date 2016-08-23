--------------------------------------------------------------------------------
module Language.Haskell.Stylish
    ( -- * Run
      runSteps
      -- * Steps
    , simpleAlign
    , imports
    , languagePragmas
    , tabs
    , trailingWhitespace
    , unicodeSyntax
      -- ** Helpers
    , stepName
      -- * Config
    , module Language.Haskell.Stylish.Config
      -- * Misc
    , module Language.Haskell.Stylish.Verbose
    , version
    , Lines
    , Step
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (foldM)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports            as Imports
import qualified Language.Haskell.Stylish.Step.LanguagePragmas    as LanguagePragmas
import qualified Language.Haskell.Stylish.Step.SimpleAlign        as SimpleAlign
import qualified Language.Haskell.Stylish.Step.Tabs               as Tabs
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace as TrailingWhitespace
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax      as UnicodeSyntax
import           Language.Haskell.Stylish.Verbose
import           Paths_stylish_haskell                            (version)


--------------------------------------------------------------------------------
simpleAlign :: Int  -- ^ Columns
            -> SimpleAlign.Config
            -> Step
simpleAlign = SimpleAlign.step


--------------------------------------------------------------------------------
imports :: Int -- ^ columns
        -> Imports.Options
        -> Step
imports = Imports.step


--------------------------------------------------------------------------------
languagePragmas :: Int -- ^ columns
                -> LanguagePragmas.Style
                -> Bool -- ^ Pad to same length in vertical mode?
                -> Bool -- ^ remove redundant?
                -> Step
languagePragmas = LanguagePragmas.step


--------------------------------------------------------------------------------
tabs :: Int -- ^ number of spaces
     -> Step
tabs = Tabs.step


--------------------------------------------------------------------------------
trailingWhitespace :: Step
trailingWhitespace = TrailingWhitespace.step


--------------------------------------------------------------------------------
unicodeSyntax :: Bool -- ^ add language pragma?
              -> Step
unicodeSyntax = UnicodeSyntax.step


--------------------------------------------------------------------------------
runStep :: Extensions -> Maybe FilePath -> Lines -> Step -> Either String Lines
runStep exts mfp ls step =
    stepFilter step ls <$> parseModule exts mfp (unlines ls)


--------------------------------------------------------------------------------
runSteps :: Extensions -> Maybe FilePath -> [Step] -> Lines
         -> Either String Lines
runSteps exts mfp steps ls = foldM (runStep exts mfp) ls steps
