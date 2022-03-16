{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies   #-}
module Language.Haskell.Stylish.Tests.Util
    ( dumpAst
    , dumpModule
    , Snippet (..)
    , assertSnippet
    , withTestDirTree
    ) where


--------------------------------------------------------------------------------
import           Control.Exception               (bracket, try)
import           Data.Data                       (Data (..))
import           GHC.Exts                        (IsList (..))
import           GHC.Hs.Dump                     (BlankEpAnnotations (..),
                                                  BlankSrcSpan (..),
                                                  showAstData)
import           System.Directory                (createDirectory,
                                                  getCurrentDirectory,
                                                  getTemporaryDirectory,
                                                  removeDirectoryRecursive,
                                                  setCurrentDirectory)
import           System.FilePath                 ((</>))
import           System.IO.Error                 (isAlreadyExistsError)
import           System.Random                   (randomIO)
import           Test.HUnit                      (Assertion, (@=?))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC    (showOutputable)
import           Language.Haskell.Stylish.Module (Module)
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step

--------------------------------------------------------------------------------
-- | Takes a Haskell source as an argument and parse it into a Module.
-- Extract function selects element from that Module record and returns
-- its String representation.
--
-- This function should be used when trying to understand how particular
-- Haskell code will be represented by ghc-parser's AST
dumpAst :: Data a => (Module -> a) -> String -> String
dumpAst extract str =
  let Right(theModule) = parseModule [] Nothing str
      ast              = extract theModule
      sdoc             = showAstData BlankSrcSpan BlankEpAnnotations ast
  in  showOutputable sdoc

dumpModule :: String -> String
dumpModule = dumpAst id

--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep s str = case s of
  Step _ step ->
    case parseModule [] Nothing str of
      Left err      -> error err
      Right module' -> unlines $ step ls module'
  where
    ls = lines str


--------------------------------------------------------------------------------
-- | 'Lines' that show as a normal string.
newtype Snippet = Snippet {unSnippet :: Lines} deriving (Eq)

-- Prefix with one newline since so HUnit will use a newline after `got: ` or
-- `expected: `.
instance Show Snippet where show = unlines . ("" :) . unSnippet

instance IsList Snippet where
    type Item Snippet = String
    fromList = Snippet
    toList   = unSnippet


--------------------------------------------------------------------------------
testSnippet :: Step -> Snippet -> Snippet
testSnippet s = Snippet . lines . testStep s . unlines . unSnippet


--------------------------------------------------------------------------------
assertSnippet :: Step -> Snippet -> Snippet -> Assertion
assertSnippet step input expected = expected @=? testSnippet step input


--------------------------------------------------------------------------------
-- | Create a temporary directory with a randomised name built from the template
-- provided
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


--------------------------------------------------------------------------------
-- | Perform an action inside a temporary directory tree and purge the tree
-- afterwards
withTestDirTree :: IO a -> IO a
withTestDirTree action = bracket
    ((,) <$> getCurrentDirectory <*> createTempDirectory "stylish_haskell")
    (\(current, temp) ->
        setCurrentDirectory current *>
        removeDirectoryRecursive temp)
    (\(_, temp) -> setCurrentDirectory temp *> action)
