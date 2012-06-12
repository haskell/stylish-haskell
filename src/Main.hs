--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forM_)
import           Data.List              (intercalate)
import           Data.Maybe             (listToMaybe)
import           Data.Version           (Version(..))
import           System.Console.CmdArgs
import           System.IO.Strict       (readFile)
import           Prelude                hiding (readFile)


--------------------------------------------------------------------------------
import           Paths_stylish_haskell  (version)
import           StylishHaskell
import           StylishHaskell.Config
import           StylishHaskell.Step
import           StylishHaskell.Verbose


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { config   :: Maybe FilePath
    , verbose  :: Bool
    , defaults :: Bool
    , inPlace  :: Bool
    , files    :: [FilePath]
    } deriving (Data, Show, Typeable)


--------------------------------------------------------------------------------
stylishArgs :: StylishArgs
stylishArgs = StylishArgs
    { config   = Nothing &= typFile &= help "Configuration file"
    , verbose  = False              &= help "Run in verbose mode"
    , defaults = False              &= help "Dump default config and exit"
    , inPlace  = False              &= help "Overwrite the given files in place"
    , files    = []      &= typFile &= args
    } &= summary ("stylish-haskell-" ++ versionString version)
  where
    versionString = intercalate "." . map show . versionBranch


--------------------------------------------------------------------------------
main :: IO ()
main = cmdArgs stylishArgs >>= stylishHaskell


--------------------------------------------------------------------------------
stylishHaskell :: StylishArgs -> IO ()
stylishHaskell sa
    | defaults sa = do
        fileName <- defaultConfigFilePath
        verbose' $ "Dumping config from " ++ fileName
        readFile fileName >>= putStr
    | otherwise   = do
        conf <- loadConfig verbose' (config sa)
        let steps = configSteps conf
        forM_ steps $ \s -> verbose' $ "Enabled " ++ stepName s ++ " step"
        contents <- maybe getContents readFile filePath
        write $ unlines $ runSteps filePath steps $ lines contents
  where
    verbose' = makeVerbose (verbose sa)
    filePath = listToMaybe $ files sa
    write = maybe putStr
                  (if inPlace sa then writeFile else const putStr)
                  filePath
