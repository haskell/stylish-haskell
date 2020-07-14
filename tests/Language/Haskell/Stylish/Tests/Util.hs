module Language.Haskell.Stylish.Tests.Util
    ( testStep
    , withTestDirTree
    ) where


--------------------------------------------------------------------------------
import           Control.Exception                 (bracket, try)
import           System.Directory                  (createDirectory,
                                                    getCurrentDirectory,
                                                    getTemporaryDirectory,
                                                    removeDirectoryRecursive,
                                                    setCurrentDirectory)
import           System.FilePath                   ((</>))
import           System.IO.Error                   (isAlreadyExistsError)
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Random                     (randomIO)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse
import qualified Language.Haskell.Stylish.ParseGHC as ParseGHC
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
testStep :: Step -> String -> String
testStep step str = case stepFilter step of
    Left f -> case parseModule [] Nothing str of
        Left err      -> error err
        Right module' -> unlines $ f ls module'
    Right f -> case unsafePerformIO (ParseGHC.parseModule mempty Nothing str) of
        Left err      -> error err
        Right module' -> unlines $ f ls module'
  where
    ls = lines str


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
