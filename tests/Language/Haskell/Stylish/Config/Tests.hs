module Language.Haskell.Stylish.Config.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Exception               hiding (assert)
import qualified Data.Set                        as Set
import           System.Directory
import           System.FilePath                 ((</>))
import           System.IO.Error
import           System.Random
import           Test.Framework                  (Test, testGroup)
import           Test.Framework.Providers.HUnit  (testCase)
import           Test.HUnit                      (Assertion, assert)
--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config

--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Config"
    [ testCase "Extensions extracted correctly from .cabal file"
               testExtensionsFromDotCabal
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml file"
               testExtensionsFromDotStylish
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml and .cabal files"
               testExtensionsFromBoth
    ]
--------------------------------------------------------------------------------

-- | Create a temporary directory with a randomised name built from the template provided
createTempDirectory :: String -> IO FilePath
createTempDirectory template  = do
  tmpRootDir <- getTemporaryDirectory
  dirId <- randomIO :: IO Word
  findTempName tmpRootDir dirId
  where
    findTempName :: FilePath -> Word -> IO FilePath
    findTempName tmpRootDir x = do
      let dirpath = tmpRootDir </> template ++ show x
      r <- try $ createDirectory dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName tmpRootDir (x+1)
                | otherwise              -> ioError e

-- | Perform an action inside a temporary directory tree and purge the tree afterwords
withTestDirTree :: IO a -> IO a
withTestDirTree action = bracket
    ((,) <$> getCurrentDirectory <*> createTempDirectory "stylish_haskell")
    (\(current, temp) ->
        setCurrentDirectory current *>
        removeDirectoryRecursive temp)
    (\(_, temp) -> setCurrentDirectory temp *> action)

-- | Put an example config files (.cabal/.stylish-haskell.yaml/both)
-- into the current directory and extract extensions from it.
createFilesAndGetExtensions :: [(FilePath, String)] -> IO Extensions
createFilesAndGetExtensions files = withTestDirTree $ do
  mapM_ (\(k, v) -> writeFile k v) files
  -- create an empty directory and change into it
  createDirectory "src"
  setCurrentDirectory "src"
  -- from that directory read the config file and extract extensions
  -- to make sure the search for .cabal file works
  config <- loadConfig (const (pure ())) Nothing
  pure $ configLanguageExtensions config

--------------------------------------------------------------------------------
testExtensionsFromDotCabal :: Assertion
testExtensionsFromDotCabal =
  assert $ (expected ==) . Set.fromList <$>
    createFilesAndGetExtensions [("test.cabal", dotCabal True)]
    where
      expected = Set.fromList ["ScopedTypeVariables", "DataKinds"]

--------------------------------------------------------------------------------
testExtensionsFromDotStylish :: Assertion
testExtensionsFromDotStylish =
  assert $ (expected ==) . Set.fromList <$>
    createFilesAndGetExtensions [(".stylish-haskell.yaml", dotStylish)]
    where
      expected = Set.fromList ["TemplateHaskell", "QuasiQuotes"]

--------------------------------------------------------------------------------
testExtensionsFromBoth :: Assertion
testExtensionsFromBoth =
  assert $ (expected ==) . Set.fromList <$>
    createFilesAndGetExtensions [ ("test.cabal", dotCabal True)
                                , (".stylish-haskell.yaml", dotStylish)]
    where
      expected = Set.fromList
        ["ScopedTypeVariables", "DataKinds", "TemplateHaskell", "QuasiQuotes"]

-- | Example cabal file borrowed from
--   https://www.haskell.org/cabal/users-guide/developing-packages.html
--   with some default-extensions added
dotCabal :: Bool -> String
dotCabal includeExtensions = unlines $
  [ "name:            TestPackage"
  , "version:         0.0"
  , "synopsis:        Package with library and two programs"
  , "license:         BSD3"
  , "author:          Angela Author"
  , "build-type:      Simple"
  , "cabal-version:   >= 1.2"
  , ""
  , "library"
  , "   build-depends:   HUnit"
  , "   exposed-modules: A, B, C"
  ] ++
  [if includeExtensions then "   default-extensions: ScopedTypeVariables"
                        else ""]
  ++
  [ ""
  , "executable program1"
  , "   main-is:         Main.hs"
  , "   hs-source-dirs:  prog1"
  , "   other-modules:   A, B"
  ] ++
  [if includeExtensions then "   default-extensions: DataKinds"
                        else ""]

-- | Example .stylish-haskell.yaml
dotStylish :: String
dotStylish = unlines $
  [ "steps:"
  , "  - imports:"
  , "      align: none"
  , "      list_align: after_alias"
  , "      long_list_align: inline"
  , "      separate_lists: true"
  , "  - language_pragmas:"
  , "      style: vertical"
  , "      align: false"
  , "      remove_redundant: true"
  , "  - trailing_whitespace: {}"
  , "columns: 110"
  , "language_extensions:"
  , "  - TemplateHaskell"
  , "  - QuasiQuotes"
  ]
