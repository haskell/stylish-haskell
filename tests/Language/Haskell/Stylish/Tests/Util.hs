module Language.Haskell.Stylish.Tests.Util
    ( testStep
    , testStep'
    , withTestDirTree
    ) where


--------------------------------------------------------------------------------
import           Control.Exception              (bracket, try)
import           System.Directory               (createDirectory,
                                                 getCurrentDirectory,
                                                 getTemporaryDirectory,
                                                 removeDirectoryRecursive,
                                                 setCurrentDirectory)
import           System.FilePath                ((</>))
import           System.IO.Error                (isAlreadyExistsError)
import           System.Random                  (randomIO)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep step str = case parseModule [] Nothing str of
    Left err      -> error err
    Right module' -> unlines $ stepFilter step ls module'
  where
    ls = lines str

testStep' :: Step -> Lines -> Lines
testStep' step ls = case parseModule [] Nothing (unlines ls) of
  Left err ->
    error $ "parseAndFormat: Should've been able to parse input - " <> err
  Right parsedModule ->
    stepFilter step ls parsedModule


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
