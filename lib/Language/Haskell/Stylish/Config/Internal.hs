--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Config.Internal
    ( ancestors
    ) where


--------------------------------------------------------------------------------
import           Data.List       (inits)
import           System.FilePath (joinPath, splitPath)


--------------------------------------------------------------------------------
-- All ancestors of a dir (including that dir)
ancestors :: FilePath -> [FilePath]
ancestors = map joinPath . reverse . dropWhile null . inits . splitPath
