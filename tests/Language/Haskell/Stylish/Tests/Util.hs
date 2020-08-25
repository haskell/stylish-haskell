{-# LANGUAGE BlockArguments #-}
module Language.Haskell.Stylish.Tests.Util
    ( testStep
    , testStep'
    , withTestDirTree
    , (@=??)
    ) where


--------------------------------------------------------------------------------
import           Control.Exception              (bracket, try)
import           Control.Monad.Writer           (execWriter, tell)
import           Data.List                      (intercalate)
import           System.Directory               (createDirectory,
                                                 getCurrentDirectory,
                                                 getTemporaryDirectory,
                                                 removeDirectoryRecursive,
                                                 setCurrentDirectory)
import           System.FilePath                ((</>))
import           System.IO.Error                (isAlreadyExistsError)
import           System.Random                  (randomIO)
import           Test.HUnit                     (Assertion, assertFailure)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep s str = case s of
  Step _ step ->
    case parseModule [] Nothing str of
      Left err      -> error err
      Right module' -> unlines $ step ls module'
  OldStep _ step ->
    case parseModuleHSE [] Nothing str of
      Left err      -> error err
      Right module' -> unlines $ step ls module'
  where
    ls = lines str

testStep' :: Step -> Lines -> Lines
testStep' s ls = lines $ testStep s (unlines ls)

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

(@=??) :: Lines -> Lines -> Assertion
expected @=?? actual =
  if expected == actual then pure ()
  else assertFailure $ intercalate "\n" $ execWriter do
    tell ["Expected:"]
    printLines expected
    tell ["Got:"]
    printLines actual
  where
    printLines =
      mapM_ \line -> tell ["  " <> line]
