{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Config.Cabal
    ( findLanguageExtensions
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                            (unless)
import qualified Data.ByteString.Char8                    as BS
import           Data.Foldable                            (traverse_)
import           Data.List                                (nub)
import           Data.Maybe                               (maybeToList)
import qualified Distribution.PackageDescription          as Cabal
import qualified Distribution.PackageDescription.Parsec   as Cabal
import qualified Distribution.Parsec                      as Cabal
import qualified Distribution.Simple.Utils                as Cabal
import qualified Distribution.Utils.Path                  as Cabal
import qualified Distribution.Verbosity                   as Cabal
import           GHC.Data.Maybe                           (mapMaybe)
import qualified Language.Haskell.Extension               as Language
import           Language.Haskell.Stylish.Config.Internal
import           Language.Haskell.Stylish.Verbose
import           System.Directory                         (doesFileExist,
                                                           getCurrentDirectory)


--------------------------------------------------------------------------------
findLanguageExtensions
    :: Verbose -> ConfigSearchStrategy -> IO [(Language.KnownExtension, Bool)]
findLanguageExtensions verbose configSearchStrategy =
    findCabalFile verbose configSearchStrategy >>=
    maybe (pure []) (readDefaultLanguageExtensions verbose)


--------------------------------------------------------------------------------
-- | Find the closest .cabal file, possibly going up the directory structure.
findCabalFile :: Verbose -> ConfigSearchStrategy -> IO (Maybe FilePath)
findCabalFile verbose configSearchStrategy = case configSearchStrategy of
  -- If the invocation pointed us to a specific config file, it doesn't make
  -- much sense to search for cabal files manually (the config file could be
  -- somewhere like /etc, not necessarily a Haskell project).
  UseConfig _                -> pure Nothing
  SearchFromDirectory path   -> go [] $ ancestors path
  SearchFromCurrentDirectory -> getCurrentDirectory >>= go [] . ancestors
 where
  go :: [FilePath] -> [FilePath] -> IO (Maybe FilePath)
  go searched [] = do
    verbose $ ".cabal file not found, directories searched: " <>
      show searched
    verbose $ "Stylish Haskell will work basing on LANGUAGE pragmas in source files."
    return Nothing
  go searched (p : ps) = do

#if MIN_VERSION_Cabal(3,14,0)
    let projectRoot = Just $ Cabal.makeSymbolicPath p
    potentialCabalFile <- Cabal.findPackageDesc projectRoot
#else
    potentialCabalFile <- Cabal.findPackageDesc p
#endif
    case potentialCabalFile of
      Right cabalFile -> pure $ Just $
#if MIN_VERSION_Cabal(3,14,0)
        Cabal.interpretSymbolicPath projectRoot cabalFile
#else
        cabalFile
#endif
      _ -> go (p : searched) ps


--------------------------------------------------------------------------------
-- | Cabal 3.18 split 'Cabal.Verbosity' into flags plus output handles, so the
-- verbosity constants are no longer a 'Cabal.Verbosity' on their own.
silentVerbosity :: Cabal.Verbosity
#if MIN_VERSION_Cabal(3,18,0)
silentVerbosity = Cabal.mkVerbosity Cabal.defaultVerbosityHandles Cabal.silent
#else
silentVerbosity = Cabal.silent
#endif


--------------------------------------------------------------------------------
-- | Flatten a 'Cabal.CondTree'.  Cabal 3.18 dropped the constraint type
-- parameter, so 'Cabal.ignoreConditions' no longer returns a tuple.
#if MIN_VERSION_Cabal(3,18,0)
ignoreConds :: Semigroup a => Cabal.CondTree v a -> a
ignoreConds = Cabal.ignoreConditions
#else
ignoreConds :: (Semigroup a, Semigroup c) => Cabal.CondTree v c a -> a
ignoreConds = fst . Cabal.ignoreConditions
#endif


--------------------------------------------------------------------------------
-- | Extract @default-extensions@ fields from a @.cabal@ file
readDefaultLanguageExtensions :: Verbose -> FilePath -> IO [(Language.KnownExtension, Bool)]
readDefaultLanguageExtensions verbose cabalFile = do
  verbose $ "Parsing " <> cabalFile <> "..."
  packageDescription <- readGenericPackageDescription silentVerbosity cabalFile
  let library :: [Cabal.Library]
      library = maybeToList $ ignoreConds <$>
        Cabal.condLibrary packageDescription

      subLibraries :: [Cabal.Library]
      subLibraries = ignoreConds . snd <$>
        Cabal.condSubLibraries packageDescription

      executables :: [Cabal.Executable]
      executables = ignoreConds . snd <$>
        Cabal.condExecutables packageDescription

      testSuites :: [Cabal.TestSuite]
      testSuites = ignoreConds . snd <$>
        Cabal.condTestSuites packageDescription

      benchmarks :: [Cabal.Benchmark]
      benchmarks = ignoreConds . snd <$>
        Cabal.condBenchmarks packageDescription

      gatherBuildInfos :: [Cabal.BuildInfo]
      gatherBuildInfos = map Cabal.libBuildInfo library <>
                         map Cabal.libBuildInfo subLibraries <>
                         map Cabal.buildInfo executables <>
                         map Cabal.testBuildInfo testSuites <>
                         map Cabal.benchmarkBuildInfo benchmarks

      defaultExtensions :: [(Language.KnownExtension, Bool)]
      defaultExtensions = mapMaybe toPair $
        concatMap Cabal.defaultExtensions gatherBuildInfos
        where toPair (Language.EnableExtension x)  = Just (x, True)
              toPair (Language.DisableExtension x) = Just (x, False)
              toPair _                             = Nothing
  verbose $ "Gathered default-extensions: " <> show defaultExtensions
  pure $ nub defaultExtensions

readGenericPackageDescription :: Cabal.Verbosity -> FilePath -> IO Cabal.GenericPackageDescription
readGenericPackageDescription = readAndParseFile Cabal.parseGenericPackageDescription
  where
    readAndParseFile parser verbosity fpath = do
      exists <- doesFileExist fpath
      unless exists $
        Cabal.die' verbosity $
          "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
      bs <- BS.readFile fpath
      parseString parser verbosity fpath bs

    parseString parser verbosity name bs = do
      let (warnings, result) = Cabal.runParseResult (parser bs)
#if MIN_VERSION_Cabal(3,18,0)
      traverse_ (Cabal.warn verbosity . Cabal.showPWarning name . Cabal.pwarning) warnings
#else
      traverse_ (Cabal.warn verbosity . Cabal.showPWarning name) warnings
#endif
      case result of
          Right x -> return x
          Left (_, errors) -> do
#if MIN_VERSION_Cabal(3,18,0)
              traverse_ (Cabal.warn verbosity . Cabal.showPError name . Cabal.perror) errors
#else
              traverse_ (Cabal.warn verbosity . Cabal.showPError name) errors
#endif
              Cabal.die' verbosity $ "Failed parsing \"" ++ name ++ "\"."
