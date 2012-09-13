--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forM_)
import           Data.List              (intercalate)
import           Data.Version           (Version(..))
import           Prelude                hiding (readFile)
import           System.Console.CmdArgs
import           System.IO              (hPutStrLn, stderr)
import           System.IO.Strict       (readFile)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish


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
        verbose' $ "Extra language extensions: " ++
            show (configLanguageExtensions conf)
        mapM_ (file sa conf) files'
  where
    verbose' = makeVerbose (verbose sa)
    files'   = if null (files sa) then [Nothing] else map Just (files sa)


--------------------------------------------------------------------------------
-- | Processes a single file, or stdin if no filepath is given
file :: StylishArgs -> Config -> Maybe FilePath -> IO ()
file sa conf mfp = do
    contents <- maybe getContents readFile mfp
    let result = runSteps (configLanguageExtensions conf)
            mfp (configSteps conf) $ lines contents

    case result of
        Left  err -> hPutStrLn stderr err >> write contents
        Right ok  -> write $ unlines ok
  where
    write = maybe putStr (if inPlace sa then writeFile else const putStr) mfp
