{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (forM, forM_, unless)
import qualified Data.ByteString.Char8    as BC8
import           Data.List                (nub, (\\))
import           Data.Monoid              ((<>))
import           Data.Version             (showVersion)
import qualified Options.Applicative      as OA
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           listDirectory)
import           System.Exit              (exitFailure)
import           System.FilePath          (takeExtension, (</>))
import qualified System.IO                as IO
import qualified System.IO.Strict         as IO.Strict


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { saVersion   :: Bool
    , saConfig    :: Maybe FilePath
    , saRecursive :: Maybe FilePath
    , saBlacklist :: Maybe [FilePath]
    , saVerbose   :: Bool
    , saDefaults  :: Bool
    , saInPlace   :: Bool
    , saNoUtf8    :: Bool
    , saPathes    :: [FilePath]
    }
    deriving (Show)


--------------------------------------------------------------------------------
parseStylishArgs :: OA.Parser StylishArgs
parseStylishArgs = StylishArgs
    <$> OA.switch (
            OA.help  "Show version information" <>
            OA.long  "version"                  <>
            OA.hidden)
    <*> OA.optional (OA.strOption $
            OA.metavar "CONFIG"              <>
            OA.help    "Configuration file"  <>
            OA.long    "config"              <>
            OA.short   'c'                   <>
            OA.hidden)
    <*> OA.optional (OA.strOption $
            OA.metavar "RECURSIVE"             <>
            OA.help    "Recursive file search" <>
            OA.long    "recursive"             <>
            OA.short   'r'                     <>
            OA.hidden)
    <*> OA.optional (OA.many $ OA.strOption $
            OA.metavar "BLACKLIST"                             <>
            OA.help    "File(s)/folder(s) not to be processed" <>
            OA.long    "except"                                <>
            OA.short   'e'                                     <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Run in verbose mode" <>
            OA.long  "verbose"             <>
            OA.short 'v'                   <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Dump default config and exit" <>
            OA.long  "defaults"                     <>
            OA.short 'd'                            <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Overwrite the given files in place" <>
            OA.long  "inplace"                            <>
            OA.short 'i'                                  <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Don't force UTF-8 stdin/stdout" <>
            OA.long  "no-utf8"                        <>
            OA.hidden)
    <*> OA.many (OA.strArgument $
            OA.metavar "FILENAME" <>
            OA.help    "Input file(s)")


--------------------------------------------------------------------------------
stylishHaskellVersion :: String
stylishHaskellVersion = "stylish-haskell " <> showVersion version


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo StylishArgs
parserInfo = OA.info (OA.helper <*> parseStylishArgs) $
    OA.fullDesc <>
    OA.header stylishHaskellVersion


--------------------------------------------------------------------------------
main :: IO ()
main = OA.execParser parserInfo >>= stylishHaskell


--------------------------------------------------------------------------------
stylishHaskell :: StylishArgs -> IO ()
stylishHaskell sa = do
    unless (saNoUtf8 sa) $
        mapM_ (`IO.hSetEncoding` IO.utf8) [IO.stdin, IO.stdout]
    if saVersion sa then
        putStrLn stylishHaskellVersion

        else if saDefaults sa then do
            verbose' "Dumping embedded config..."
            BC8.putStr defaultConfigBytes

        else do
            conf <- loadConfig verbose' (saConfig sa)
            except <- findExceptions (saVerbose sa) (saBlacklist sa)
            filesR <- findFiles (saVerbose sa) (saRecursive sa)
            let steps = configSteps conf
            forM_ steps $ \s -> verbose' $ "Enabled " ++ stepName s ++ " step"
            verbose' $ "Extra language extensions: " ++
                show (configLanguageExtensions conf)
            mapM_ (file sa conf) $ files' $ ((saPathes sa) <> filesR) \\ except
  where
    verbose' = makeVerbose (saVerbose sa)
    files' x = if null x then [Nothing] else map Just x


--------------------------------------------------------------------------------
-- | Searches given files/folders to build a list of exceptions.
findExceptions :: Bool -> Maybe [FilePath] -> IO [FilePath]
findExceptions v fs@Nothing =
  makeVerbose v ("Exception-list: " <> show fs) >> return []

findExceptions v (Just fs) = do
  es <- forM fs $ \x -> do
    d <- doesDirectoryExist x >>= \case
      True  -> findFiles v (Just x)
      False -> doesFileExist x >>= \case
          True  -> return [x]
          False -> makeVerbose v ("Not accessible: " <> show x) >> return []
    return . concat $ d
  let es' = nub es
  makeVerbose v ("Exception-list: " <> show es')
  return es'


--------------------------------------------------------------------------------
-- | Searches Haskell source files in any given folder recursively.
findFiles :: Bool -> Maybe FilePath -> IO [FilePath]
findFiles _ Nothing    = return []
findFiles v (Just dir) = do
  doesDirectoryExist dir >>= \case
    True  -> findFilesRecursive dir >>=
      return . filter (\x -> takeExtension x == ".hs")
    False -> do
      makeVerbose v ("Input folder does not exists: " <> dir)
      findFiles v Nothing
  where
    findFilesRecursive :: FilePath -> IO [FilePath]
    findFilesRecursive = listDirectoryFiles findFilesRecursive

    listDirectoryFiles :: (FilePath -> IO [FilePath])
                       -> FilePath -> IO [FilePath]
    listDirectoryFiles go topdir = do
      ps <- listDirectory topdir >>=
        mapM (\x -> do
                 let path = topdir </> x
                 doesDirectoryExist path >>= \case
                   True  -> go path
                   False -> return [path])
      return $ concat ps


--------------------------------------------------------------------------------
-- | Processes a single file, or stdin if no filepath is given
file :: StylishArgs -> Config -> Maybe FilePath -> IO ()
file sa conf mfp = do
    contents <- maybe getContents readUTF8File mfp
    let result = runSteps (configLanguageExtensions conf)
            mfp (configSteps conf) $ lines contents
    case result of
        Right ok  -> write contents $ unlines ok
        Left  err -> do
            IO.hPutStrLn IO.stderr err
            exitFailure
  where
    write old new = case mfp of
                Nothing -> putStrNewline new
                Just _    | not (saInPlace sa) -> putStrNewline new
                Just path | not (null new) && old /= new  ->
                    IO.withFile path IO.WriteMode $ \h -> do
                        setNewlineMode h
                        IO.hPutStr h new
                _ -> return ()
    setNewlineMode h = do
      let nl = configNewline conf
      let mode = IO.NewlineMode IO.nativeNewline nl
      IO.hSetNewlineMode h mode
    putStrNewline txt = setNewlineMode IO.stdout >> putStr txt

readUTF8File :: FilePath -> IO String
readUTF8File fp =
     IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetEncoding h IO.utf8
        IO.Strict.hGetContents h
