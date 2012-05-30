--------------------------------------------------------------------------------
module StylishHaskell.Config
    ( configFilePath
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (msum)
import           System.Directory
import           System.FilePath     ((</>))


--------------------------------------------------------------------------------
configFileName :: String
configFileName = ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
configFilePath :: Maybe FilePath -> IO (Maybe FilePath)
configFilePath userSpecified = do
    current  <- (</> configFileName) <$> getCurrentDirectory
    currentE <- doesFileExist current
    home     <- (</> configFileName) <$> getHomeDirectory
    homeE    <- doesFileExist home
    return $ msum
        [ userSpecified
        , if currentE then Just current else Nothing
        , if homeE then Just home else Nothing
        ]
