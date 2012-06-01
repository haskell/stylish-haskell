--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module StylishHaskell.Config
    ( Config (..)
    , configFilePath
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                       ((<$>))
import           Control.Monad                             (forM, msum, mzero)
import           Data.Aeson                                (FromJSON(..))
import qualified Data.Aeson                                as A
import qualified Data.Aeson.Types                          as A
import qualified Data.ByteString                           as B
import Data.Map (Map)
import qualified Data.Map as M
import           Data.Yaml                                 (decodeEither)
import           System.Directory
import           System.FilePath                           ((</>))


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish
import qualified StylishHaskell.Stylish.Imports
import qualified StylishHaskell.Stylish.LanguagePragmas
import qualified StylishHaskell.Stylish.Tabs
import qualified StylishHaskell.Stylish.TrailingWhitespace
import qualified StylishHaskell.Stylish.UnicodeSyntax


--------------------------------------------------------------------------------
data Config = Config
    { configStylish :: [Stylish]
    }


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config $
    [ StylishHaskell.Stylish.Imports.stylish True
    , StylishHaskell.Stylish.LanguagePragmas.stylish
    , StylishHaskell.Stylish.TrailingWhitespace.stylish
    ]


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


--------------------------------------------------------------------------------
loadConfig :: Maybe FilePath -> IO Config
loadConfig mfp = do
    mfp' <- configFilePath mfp
    case mfp' of
        Nothing -> return defaultConfig
        Just fp -> do
            bs <- B.readFile fp
            case decodeEither bs of
                Left err     -> error $
                    "StylishHaskell.Config.loadConfig: " ++
                    "Could not load " ++ fp ++ ": " ++ err
                Right config -> return config


--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = Config
    <$> (o A..: "stylish" >>= fmap concat . mapM parseStylish)
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (A.Object -> A.Parser Stylish)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("languages_pragmas",   parseLanguagePragmas)
    , ("tabs",                parseTabs)
    , ("trailing_whitespace", parseTrailingWhitespace)
    , ("unicode_syntax",      parseUnicodeSyntax)
    ]


--------------------------------------------------------------------------------
parseStylish :: A.Value -> A.Parser [Stylish]
parseStylish val = do
    map' <- parseJSON val :: A.Parser (Map String A.Value)
    forM (M.toList map') $ \(k, v) -> case (M.lookup k catalog, v) of
        (Just parser, A.Object o) -> parser o
        _                         -> fail $ "Invalid declaration for " ++ k


--------------------------------------------------------------------------------
parseImports :: A.Object -> A.Parser Stylish
parseImports o = StylishHaskell.Stylish.Imports.stylish
    <$> o A..: "align"


--------------------------------------------------------------------------------
parseLanguagePragmas :: A.Object -> A.Parser Stylish
parseLanguagePragmas _ = return StylishHaskell.Stylish.LanguagePragmas.stylish


--------------------------------------------------------------------------------
parseTabs :: A.Object -> A.Parser Stylish
parseTabs o = StylishHaskell.Stylish.Tabs.stylish
    <$> o A..: "spaces"


--------------------------------------------------------------------------------
parseTrailingWhitespace :: A.Object -> A.Parser Stylish
parseTrailingWhitespace _ =
    return StylishHaskell.Stylish.TrailingWhitespace.stylish


--------------------------------------------------------------------------------
parseUnicodeSyntax :: A.Object -> A.Parser Stylish
parseUnicodeSyntax _ = return StylishHaskell.Stylish.UnicodeSyntax.stylish
