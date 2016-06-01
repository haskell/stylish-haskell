--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (forM_, when)
import           Data.List                (intercalate)
import           Data.Version             (Version (..))
import           System.Console.CmdArgs
import qualified System.IO                as IO
import           System.IO.Strict         (hGetContents)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { config   :: Maybe FilePath
    , verbose  :: Bool
    , defaults :: Bool
    , inPlace  :: Bool
    , utf8     :: Bool
    , files    :: [FilePath]
    } deriving (Data, Show, Typeable)


--------------------------------------------------------------------------------
stylishArgs :: StylishArgs
stylishArgs = StylishArgs
    { config   = Nothing &= typFile &= help "Configuration file"
    , verbose  = False              &= help "Run in verbose mode"
    , defaults = False              &= help "Dump default config and exit"
    , inPlace  = False              &= help "Overwrite the given files in place"
    , utf8     = False              &= help "Force UTF-8 stdin/stdout"
    , files    = []      &= typFile &= args
    } &= summary ("stylish-haskell-" ++ versionString version)
  where
    versionString = intercalate "." . map show . versionBranch


--------------------------------------------------------------------------------
main :: IO ()
main = cmdArgs stylishArgs >>= stylishHaskell


--------------------------------------------------------------------------------
stylishHaskell :: StylishArgs -> IO ()
stylishHaskell sa = do
    when (utf8 sa) $
        mapM_ (`IO.hSetEncoding` IO.utf8) [IO.stdin, IO.stdout]
    case defaults sa of
        True -> do
            fileName <- defaultConfigFilePath
            verbose' $ "Dumping config from " ++ fileName
            readUTF8File fileName >>= putStr
        False -> do
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
    contents <- maybe getContents readUTF8File mfp
    let result = runSteps (configLanguageExtensions conf)
            mfp (configSteps conf) $ lines contents
    case result of
        Left  err -> IO.hPutStrLn IO.stderr err >> write contents contents
        Right ok  -> write contents $ unlines ok
  where
    write old new = case mfp of
                Nothing -> putStr new
                Just _    | not (inPlace sa) -> putStr new
                Just path | length new /= 0 && old /= new  -> writeFile path new
                _ -> return ()

readUTF8File :: FilePath -> IO String
readUTF8File fp =
     IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetEncoding h IO.utf8
        content <- IO.hGetContents h
        return content
