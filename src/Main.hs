--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Data.Maybe             (listToMaybe)
import           Data.Version           (Version(..))
import           System.Console.CmdArgs


--------------------------------------------------------------------------------
import           Paths_stylish_haskell  (version)
import           StylishHaskell
import           StylishHaskell.Config


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { config :: Maybe FilePath
    , files  :: [FilePath]
    } deriving (Data, Show, Typeable)


--------------------------------------------------------------------------------
stylishArgs :: StylishArgs
stylishArgs = StylishArgs
    { config = Nothing &= typFile &= help "Configuration file"
    , files  = []      &= typFile &= args
    } &= summary ("stylish-haskell-" ++ versionString version)
  where
    versionString = intercalate "." . map show . versionBranch


--------------------------------------------------------------------------------
main :: IO ()
main = do
    sa   <- cmdArgs stylishArgs
    conf <- loadConfig (config sa)
    let filePath = listToMaybe $ files sa
        stylish  = configStylish conf
    contents <- maybe getContents readFile filePath
    putStr $ unlines $ chainStylish filePath stylish $ lines contents
