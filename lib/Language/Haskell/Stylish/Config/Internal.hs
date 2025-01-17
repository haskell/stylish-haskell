--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Config.Internal
    ( ConfigSearchStrategy (..)
    , ancestors
    ) where


--------------------------------------------------------------------------------
import           Data.List       (inits)
import           System.FilePath (joinPath, splitPath)


--------------------------------------------------------------------------------
-- All ancestors of a dir (including that dir)
ancestors :: FilePath -> [FilePath]
ancestors = map joinPath . reverse . dropWhile null . inits . splitPath


--------------------------------------------------------------------------------
data ConfigSearchStrategy
    = -- | Don't try to search, just use given config file
      UseConfig FilePath
    | -- | Search for @.stylish-haskell.yaml@ starting from given directory.
      -- If not found, try all ancestor directories, @$XDG_CONFIG\/stylish-haskell\/config.yaml@ and @$HOME\/.stylish-haskell.yaml@ in order.
      -- If no config is found, default built-in config will be used.
      SearchFromDirectory FilePath
    | -- | Like SearchFromDirectory, but using current working directory as a starting point
      SearchFromCurrentDirectory
