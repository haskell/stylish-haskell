--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module StylishHaskell.Config
    ( Config (..)
    , configFilePath
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                       ((<$>), (<*>))
import           Control.Monad                             (forM, msum, mzero)
import           Data.Aeson                                (FromJSON(..))
import qualified Data.Aeson                                as A
import qualified Data.Aeson.Types                          as A
import qualified Data.ByteString                           as B
import           Data.List                                 (intercalate)
import           Data.Map                                  (Map)
import qualified Data.Map                                  as M
import           Data.Yaml                                 (decodeEither)
import           System.Directory
import           System.FilePath                           ((</>))


--------------------------------------------------------------------------------
import           StylishHaskell.Step
import qualified StylishHaskell.Step.Imports            as Imports
import qualified StylishHaskell.Step.LanguagePragmas    as LanguagePragmas
import qualified StylishHaskell.Step.Tabs               as Tabs
import qualified StylishHaskell.Step.TrailingWhitespace as TrailingWhitespace
import qualified StylishHaskell.Step.UnicodeSyntax      as UnicodeSyntax
import           StylishHaskell.Verbose


--------------------------------------------------------------------------------
data Config = Config
    { configSteps :: [Step]
    }


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config $
    [ Imports.step Imports.Global
    , LanguagePragmas.step LanguagePragmas.Vertical True
    , TrailingWhitespace.step
    ]


--------------------------------------------------------------------------------
configFileName :: String
configFileName = ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
configFilePath :: Verbose -> Maybe FilePath -> IO (Maybe FilePath)
configFilePath verbose userSpecified = do
    current  <- (</> configFileName) <$> getCurrentDirectory
    currentE <- doesFileExist current
    report current currentE
    home     <- (</> configFileName) <$> getHomeDirectory
    homeE    <- doesFileExist home
    report home homeE
    return $ msum
        [ userSpecified
        , if currentE then Just current else Nothing
        , if homeE then Just home else Nothing
        ]
  where
    report fp e = verbose $ fp ++ if e then " exists" else " does not exist"


--------------------------------------------------------------------------------
loadConfig :: Verbose -> Maybe FilePath -> IO Config
loadConfig verbose mfp = do
    mfp' <- configFilePath verbose mfp
    case mfp' of
        Nothing -> do
            verbose $ "Using default configuration"
            return defaultConfig
        Just fp -> do
            verbose $ "Loading configuration at " ++ fp
            bs <- B.readFile fp
            case decodeEither bs of
                Left err     -> error $
                    "StylishHaskell.Config.loadConfig: " ++ err
                Right config -> return config


--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = Config
    <$> (o A..: "steps" >>= fmap concat . mapM parseSteps)
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (A.Object -> A.Parser Step)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("language_pragmas",    parseLanguagePragmas)
    , ("tabs",                parseTabs)
    , ("trailing_whitespace", parseTrailingWhitespace)
    , ("unicode_syntax",      parseUnicodeSyntax)
    ]


--------------------------------------------------------------------------------
parseSteps :: A.Value -> A.Parser [Step]
parseSteps val = do
    map' <- parseJSON val :: A.Parser (Map String A.Value)
    forM (M.toList map') $ \(k, v) -> case (M.lookup k catalog, v) of
        (Just parser, A.Object o) -> parser o
        _                         -> fail $ "Invalid declaration for " ++ k


--------------------------------------------------------------------------------
-- | Utility for enum-like options
parseEnum :: [(String, a)] -> a -> Maybe String -> A.Parser a
parseEnum _    def Nothing  = return def
parseEnum strs _   (Just k) = case lookup k strs of
    Just v  -> return v
    Nothing -> fail $ "Unknown option: " ++ k ++ ", should be one of: " ++
        intercalate ", " (map fst strs)


--------------------------------------------------------------------------------
parseImports :: A.Object -> A.Parser Step
parseImports o = Imports.step
    <$> (o A..:? "align" >>= parseEnum aligns Imports.Global)
  where
    aligns =
        [ ("global", Imports.Global)
        , ("group",  Imports.Group)
        , ("none",   Imports.None)
        ]


--------------------------------------------------------------------------------
parseLanguagePragmas :: A.Object -> A.Parser Step
parseLanguagePragmas o = LanguagePragmas.step
    <$> (o A..:? "style" >>= parseEnum styles LanguagePragmas.Vertical)
    <*> o A..:? "remove_redundant" A..!= True
  where
    styles =
        [ ("vertical", LanguagePragmas.Vertical)
        , ("compact",  LanguagePragmas.Compact)
        ]


--------------------------------------------------------------------------------
parseTabs :: A.Object -> A.Parser Step
parseTabs o = Tabs.step
    <$> o A..:? "spaces" A..!= 8


--------------------------------------------------------------------------------
parseTrailingWhitespace :: A.Object -> A.Parser Step
parseTrailingWhitespace _ = return TrailingWhitespace.step


--------------------------------------------------------------------------------
parseUnicodeSyntax :: A.Object -> A.Parser Step
parseUnicodeSyntax _ = return UnicodeSyntax.step
