--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module StylishHaskell.Config
    ( Extensions
    , Config (..)
    , defaultConfigFilePath
    , configFilePath
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                    (pure, (<$>), (<*>))
import           Control.Monad                          (forM, msum, mzero)
import           Data.Aeson                             (FromJSON(..))
import qualified Data.Aeson                             as A
import qualified Data.Aeson.Types                       as A
import qualified Data.ByteString                        as B
import           Data.List                              (intercalate)
import           Data.Map                               (Map)
import qualified Data.Map                               as M
import           Data.Yaml                              (decodeEither)
import           System.Directory
import           System.FilePath                        ((</>))


--------------------------------------------------------------------------------
import           Paths_stylish_haskell                  (getDataFileName)
import           StylishHaskell.Step
import qualified StylishHaskell.Step.Imports            as Imports
import qualified StylishHaskell.Step.LanguagePragmas    as LanguagePragmas
import qualified StylishHaskell.Step.Tabs               as Tabs
import qualified StylishHaskell.Step.TrailingWhitespace as TrailingWhitespace
import qualified StylishHaskell.Step.UnicodeSyntax      as UnicodeSyntax
import           StylishHaskell.Verbose


--------------------------------------------------------------------------------
type Extensions = [String]


--------------------------------------------------------------------------------
data Config = Config
    { configSteps              :: [Step]
    , configColumns            :: Int
    , configLanguageExtensions :: [String]
    }


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
emptyConfig :: Config
emptyConfig = Config [] 80 []


--------------------------------------------------------------------------------
configFileName :: String
configFileName = ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = getDataFileName ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
configFilePath :: Verbose -> Maybe FilePath -> IO (Maybe FilePath)
configFilePath verbose userSpecified = do
    (current, currentE) <- check $ (</> configFileName) <$> getCurrentDirectory
    (home, homeE)       <- check $ (</> configFileName) <$> getHomeDirectory
    (def, defE)         <- check defaultConfigFilePath
    return $ msum
        [ userSpecified
        , if currentE then Just current else Nothing
        , if homeE then Just home else Nothing
        , if defE then Just def else Nothing
        ]
  where
    check fp = do
        fp' <- fp
        ex  <- doesFileExist fp'
        verbose $ fp' ++ if ex then " exists" else " does not exist"
        return (fp', ex)


--------------------------------------------------------------------------------
loadConfig :: Verbose -> Maybe FilePath -> IO Config
loadConfig verbose mfp = do
    mfp' <- configFilePath verbose mfp
    case mfp' of
        Nothing -> do
            verbose $ "Using empty configuration"
            return emptyConfig
        Just fp -> do
            verbose $ "Loading configuration at " ++ fp
            bs <- B.readFile fp
            case decodeEither bs of
                Left err     -> error $
                    "StylishHaskell.Config.loadConfig: " ++ err
                Right config -> return config


--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = do
    -- First load the config without the actual steps
    config <- Config
        <$> pure []
        <*> (o A..:? "columns"             A..!= 80)
        <*> (o A..:? "language_extensions" A..!= [])

    -- Then fill in the steps based on the partial config we already have
    steps <- (o A..:  "steps" >>= fmap concat . mapM (parseSteps config))
    return config {configSteps = steps}
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (Config -> A.Object -> A.Parser Step)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("language_pragmas",    parseLanguagePragmas)
    , ("tabs",                parseTabs)
    , ("trailing_whitespace", parseTrailingWhitespace)
    , ("unicode_syntax",      parseUnicodeSyntax)
    ]


--------------------------------------------------------------------------------
parseSteps :: Config -> A.Value -> A.Parser [Step]
parseSteps config val = do
    map' <- parseJSON val :: A.Parser (Map String A.Value)
    forM (M.toList map') $ \(k, v) -> case (M.lookup k catalog, v) of
        (Just parser, A.Object o) -> parser config o
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
parseImports :: Config -> A.Object -> A.Parser Step
parseImports config o = Imports.step
    <$> pure (configColumns config)
    <*> (o A..:? "align" >>= parseEnum aligns Imports.Global)
  where
    aligns =
        [ ("global", Imports.Global)
        , ("group",  Imports.Group)
        , ("none",   Imports.None)
        ]


--------------------------------------------------------------------------------
parseLanguagePragmas :: Config -> A.Object -> A.Parser Step
parseLanguagePragmas _ o = LanguagePragmas.step
    <$> (o A..:? "style" >>= parseEnum styles LanguagePragmas.Vertical)
    <*> o A..:? "remove_redundant" A..!= True
  where
    styles =
        [ ("vertical", LanguagePragmas.Vertical)
        , ("compact",  LanguagePragmas.Compact)
        ]


--------------------------------------------------------------------------------
parseTabs :: Config -> A.Object -> A.Parser Step
parseTabs _ o = Tabs.step
    <$> o A..:? "spaces" A..!= 8


--------------------------------------------------------------------------------
parseTrailingWhitespace :: Config -> A.Object -> A.Parser Step
parseTrailingWhitespace _ _ = return TrailingWhitespace.step


--------------------------------------------------------------------------------
parseUnicodeSyntax :: Config -> A.Object -> A.Parser Step
parseUnicodeSyntax _ o = UnicodeSyntax.step
    <$> o A..:? "add_language_pragma" A..!= True
