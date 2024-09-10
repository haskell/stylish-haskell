{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Config.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Types                    as Aeson
import qualified Data.ByteString.Lazy.Char8          as BL8
import qualified Data.Set                            as Set
import qualified Data.YAML.Aeson                     as Yaml
import           System.Directory
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Config"
    [ testCase "Extensions extracted correctly from .cabal file"
               testExtensionsFromDotCabal
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml file"
               testExtensionsFromDotStylish
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml and .cabal files"
               testExtensionsFromBoth
    , testCase "NoXyz extensions from .stylish-haskell.yaml file"
               testStylishNoXyz
    , testCase "NoXyz extensions from .cabal file"
               testCabalNoXyz
    , testCase "Correctly read .stylish-haskell.yaml file with default max column number"
               testDefaultColumns
    , testCase "Correctly read .stylish-haskell.yaml file with specified max column number"
               testSpecifiedColumns
    , testCase "Correctly read .stylish-haskell.yaml file with no max column number"
               testNoColumns
    , testCase "Backwards-compatible align options"
               testBoolSimpleAlign
    ]



--------------------------------------------------------------------------------
type ExtensionName = String

data ConfigFile = ConfigFile
  { fileName   :: FilePath
  , contents   :: String
  , extensions :: [ExtensionName]
  }

stylishCfg :: ([ExtensionName] -> String) -> [ExtensionName] -> ConfigFile
stylishCfg template exts = ConfigFile
  { fileName   = ".stylish-haskell.yaml"
  , contents   = template exts
  , extensions = exts
  }

cabalCfg :: ([ExtensionName] -> [ExtensionName] -> String) ->
  [ExtensionName] -> [ExtensionName] -> ConfigFile
cabalCfg template exts1 exts2 = ConfigFile
  { fileName   = "test.cabal"
  , contents   = template exts1 exts2
  , extensions = exts1 ++ exts2
  }


--------------------------------------------------------------------------------
testExtensions :: [ConfigFile] -> Assertion
testExtensions cfgFiles = do
  cfg' <- createFilesAndGetConfig cfgFiles
  let expected = Set.fromList (concatMap extensions cfgFiles)
      actual   = Set.fromList (configLanguageExtensions cfg')
  actual @?= expected

testColumns :: Maybe Int -> [ConfigFile] -> Assertion
testColumns expected cfgFiles = do
  cfg' <- createFilesAndGetConfig cfgFiles
  let actual = configColumns cfg'
  actual @?= expected


--------------------------------------------------------------------------------
-- | Put an example config files (.cabal/.stylish-haskell.yaml/both)
-- into the current directory and extract extensions from it.
createFilesAndGetConfig :: [ConfigFile] -> IO Config
createFilesAndGetConfig files = withTestDirTree $ do
  mapM_ (\ConfigFile{..} -> writeFile fileName contents) files
  -- create an empty directory and change into it
  createDirectory "src"
  setCurrentDirectory "src"
  -- from that directory read the config file and extract extensions
  -- to make sure the search for .cabal file works
  loadConfig (const (pure ())) SearchFromCurrentDirectory


--------------------------------------------------------------------------------
testExtensionsFromDotCabal :: Assertion
testExtensionsFromDotCabal = testExtensions
    [ cabalCfg dotCabal ["ScopedTypeVariables"] ["DataKinds"] ]

--------------------------------------------------------------------------------
testExtensionsFromDotStylish :: Assertion
testExtensionsFromDotStylish = testExtensions
    [ stylishCfg dotStylish ["TemplateHaskell", "QuasiQuotes"] ]

--------------------------------------------------------------------------------
testExtensionsFromBoth :: Assertion
testExtensionsFromBoth = testExtensions
    [ cabalCfg dotCabal ["ScopedTypeVariables"] ["DataKinds"]
    , stylishCfg dotStylish ["TemplateHaskell", "QuasiQuotes"]
    ]

--------------------------------------------------------------------------------
testStylishNoXyz :: Assertion
testStylishNoXyz = testExtensions
    [ stylishCfg dotStylish ["NoStarIsType", "NoTypeOperators"] ]

--------------------------------------------------------------------------------
testCabalNoXyz :: Assertion
testCabalNoXyz = testExtensions
    [ cabalCfg dotCabal ["NoStarIsType"] ["NoTypeOperators"] ]


--------------------------------------------------------------------------------
testSpecifiedColumns :: Assertion
testSpecifiedColumns = testColumns (Just 110)
    [ stylishCfg dotStylish [] ]


--------------------------------------------------------------------------------
testDefaultColumns :: Assertion
testDefaultColumns = testColumns (Just 80)
    [ stylishCfg dotStylish2 ["DataKinds"] ]


--------------------------------------------------------------------------------
testNoColumns :: Assertion
testNoColumns = testColumns Nothing
    [ stylishCfg dotStylish3 ["DataKinds"] ]


--------------------------------------------------------------------------------
testBoolSimpleAlign :: Assertion
testBoolSimpleAlign = do
    Right val <- pure $ Yaml.decode1 $ BL8.pack config
    Aeson.Success conf <- pure $ Aeson.parse parseConfig val
    length (configSteps conf) @?= 1
  where
    config = unlines
      [ "steps:"
      , "  - simple_align:"
      , "      cases: true"
      , "      top_level_patterns: always"
      , "      records: false"
      ]


-- | Example cabal file borrowed from
--   https://www.haskell.org/cabal/users-guide/developing-packages.html
--   with some default-extensions added
dotCabal :: [ExtensionName] -> [ExtensionName] -> String
dotCabal exts1 exts2 = unlines $
  [ "name:            TestPackage"
  , "version:         0.0"
  , "synopsis:        Package with library and two programs"
  , "license:         BSD3"
  , "author:          Angela Author"
  , "build-type:      Simple"
  , "cabal-version:   >= 1.10"
  , ""
  , "library"
  , "   build-depends:   HUnit"
  , "   exposed-modules: A, B, C"
  , "   default-extensions:"
  ] ++
  map ("        " ++) exts1
  ++
  [ ""
  , "executable program1"
  , "   main-is:         Main.hs"
  , "   hs-source-dirs:  prog1"
  , "   other-modules:   A, B"
  , "   default-extensions:"
  ] ++
  map ("       " ++) exts2

-- | Example .stylish-haskell.yaml
dotStylish :: [ExtensionName] -> String
dotStylish exts = unlines $
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
  , "  - records:"
  , "      equals: \"same_line\""
  , "      first_field: \"indent 2\""
  , "      field_comment: 2"
  , "      deriving: 4"
  , "      via: \"indent 2\""
  , "columns: 110"
  , "language_extensions:"
  ] ++
  map ("  - " ++) exts

-- | Example .stylish-haskell.yaml
dotStylish2 :: [ExtensionName] -> String
dotStylish2 exts = unlines $
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
  , "language_extensions:"
  ] ++
  map ("  - " ++) exts


-- | Example .stylish-haskell.yaml
dotStylish3 :: [ExtensionName] -> String
dotStylish3 exts = unlines $
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
  , "columns: null"
  , "language_extensions:"
  ] ++
  map ("  - " ++) exts
