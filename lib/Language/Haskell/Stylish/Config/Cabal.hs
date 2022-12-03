--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Config.Cabal
    ( findLanguageExtensions
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                            (unless)
import qualified Data.ByteString.Char8                    as BS
import           Data.Either                              (isRight)
import           Data.Foldable                            (traverse_)
import           Data.List                                (nub)
import           Data.Maybe                               (maybeToList)
import qualified Distribution.PackageDescription          as Cabal
import qualified Distribution.PackageDescription.Parsec   as Cabal
import qualified Distribution.Parsec                      as Cabal
import qualified Distribution.Simple.Utils                as Cabal
import qualified Distribution.Verbosity                   as Cabal
import qualified Language.Haskell.Extension               as Language
import           Language.Haskell.Stylish.Verbose
import           System.Directory                         (doesFileExist,
                                                           getCurrentDirectory)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config.Internal


--------------------------------------------------------------------------------
findLanguageExtensions :: Verbose -> IO [Language.KnownExtension]
findLanguageExtensions verbose =
    findCabalFile verbose >>=
    maybe (pure []) (readDefaultLanguageExtensions verbose)


--------------------------------------------------------------------------------
-- | Find the closest .cabal file, possibly going up the directory structure.
findCabalFile :: Verbose -> IO (Maybe FilePath)
findCabalFile verbose = do
  potentialProjectRoots <- ancestors <$> getCurrentDirectory
  potentialCabalFile <- filter isRight <$>
    traverse Cabal.findPackageDesc potentialProjectRoots
  case potentialCabalFile of
    [Right cabalFile] -> return (Just cabalFile)
    _ -> do
      verbose $ ".cabal file not found, directories searched: " <>
        show potentialProjectRoots
      verbose $ "Stylish Haskell will work basing on LANGUAGE pragmas in source files."
      return Nothing


--------------------------------------------------------------------------------
-- | Extract @default-extensions@ fields from a @.cabal@ file
readDefaultLanguageExtensions :: Verbose -> FilePath -> IO [Language.KnownExtension]
readDefaultLanguageExtensions verbose cabalFile = do
  verbose $ "Parsing " <> cabalFile <> "..."
  packageDescription <- readGenericPackageDescription Cabal.silent cabalFile
  let library :: [Cabal.Library]
      library = maybeToList $ fst . Cabal.ignoreConditions <$>
        Cabal.condLibrary packageDescription

      subLibraries :: [Cabal.Library]
      subLibraries = fst . Cabal.ignoreConditions . snd <$>
        Cabal.condSubLibraries packageDescription

      executables :: [Cabal.Executable]
      executables = fst . Cabal.ignoreConditions . snd <$>
        Cabal.condExecutables packageDescription

      testSuites :: [Cabal.TestSuite]
      testSuites = fst . Cabal.ignoreConditions . snd <$>
        Cabal.condTestSuites packageDescription

      benchmarks :: [Cabal.Benchmark]
      benchmarks = fst . Cabal.ignoreConditions . snd <$>
        Cabal.condBenchmarks packageDescription

      gatherBuildInfos :: [Cabal.BuildInfo]
      gatherBuildInfos = map Cabal.libBuildInfo library <>
                         map Cabal.libBuildInfo subLibraries <>
                         map Cabal.buildInfo executables <>
                         map Cabal.testBuildInfo testSuites <>
                         map Cabal.benchmarkBuildInfo benchmarks

      defaultExtensions :: [Language.KnownExtension]
      defaultExtensions = map fromEnabled . filter isEnabled $
        concatMap Cabal.defaultExtensions gatherBuildInfos
        where isEnabled (Language.EnableExtension _) = True
              isEnabled _                            = False

              fromEnabled (Language.EnableExtension x) = x
              fromEnabled x                             =
                error $ "Language.Haskell.Stylish.Config.readLanguageExtensions: " <>
                        "invalid LANGUAGE pragma:  " <> show x
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
      traverse_ (Cabal.warn verbosity . Cabal.showPWarning name) warnings
      case result of
          Right x -> return x
          Left (_, errors) -> do
              traverse_ (Cabal.warn verbosity . Cabal.showPError name) errors
              Cabal.die' verbosity $ "Failed parsing \"" ++ name ++ "\"."
