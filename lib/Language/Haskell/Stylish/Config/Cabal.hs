--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Config.Cabal
    ( findLanguageExtensions
    ) where


--------------------------------------------------------------------------------
import           Data.Either                              (isRight)
import           Data.List                                (nub)
import           Data.Maybe                               (maybeToList)
import qualified Distribution.PackageDescription          as Cabal
import qualified Distribution.PackageDescription.Parsec   as Cabal
import qualified Distribution.Simple.Utils                as Cabal
import qualified Distribution.Verbosity                   as Cabal
import qualified Language.Haskell.Extension               as Language
import           Language.Haskell.Stylish.Verbose
import           System.Directory                         (getCurrentDirectory)


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
  packageDescription <- Cabal.readGenericPackageDescription Cabal.silent cabalFile
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
