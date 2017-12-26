--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Haskell.Stylish.Config
    ( Extensions
    , Config (..)
    , defaultConfigBytes
    , configFilePath
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (forM, mzero)
import           Data.Aeson                                       (FromJSON (..))
import qualified Data.Aeson                                       as A
import qualified Data.Aeson.Types                                 as A
import qualified Data.ByteString                                  as B
import qualified Data.FileEmbed                                   as FileEmbed
import           Data.List                                        (inits,
                                                                   intercalate)
import           Data.Map                                         (Map)
import qualified Data.Map                                         as M
import           Data.Maybe                                       (fromMaybe)
import           Data.Yaml                                        (decodeEither)
import           System.Directory
import           System.FilePath                                  (joinPath,
                                                                   splitPath,
                                                                   (</>))
import qualified System.IO                                        as IO (Newline (..),
                                                                         nativeNewline)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports            as Imports
import qualified Language.Haskell.Stylish.Step.LanguagePragmas    as LanguagePragmas
import qualified Language.Haskell.Stylish.Step.SimpleAlign        as SimpleAlign
import qualified Language.Haskell.Stylish.Step.Tabs               as Tabs
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace as TrailingWhitespace
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax      as UnicodeSyntax
import           Language.Haskell.Stylish.Verbose


--------------------------------------------------------------------------------
type Extensions = [String]


--------------------------------------------------------------------------------
data Config = Config
    { configSteps              :: [Step]
    , configColumns            :: Int
    , configLanguageExtensions :: [String]
    , configNewline            :: IO.Newline
    }


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
configFileName :: String
configFileName = ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
defaultConfigBytes :: B.ByteString
defaultConfigBytes = $(FileEmbed.embedFile "data/stylish-haskell.yaml")


--------------------------------------------------------------------------------
configFilePath :: Verbose -> Maybe FilePath -> IO (Maybe FilePath)
configFilePath _       (Just userSpecified) = return (Just userSpecified)
configFilePath verbose Nothing              = do
    current  <- getCurrentDirectory
    home     <- getHomeDirectory
    mbConfig <- search $
        [d </> configFileName | d <- ancestors current] ++
        [home </> configFileName]

    return mbConfig
  where
    -- All ancestors of a dir (including that dir)
    ancestors :: FilePath -> [FilePath]
    ancestors = init . map joinPath . reverse . inits . splitPath

    search :: [FilePath] -> IO (Maybe FilePath)
    search []       = return Nothing
    search (f : fs) = do
        -- TODO Maybe catch an error here, dir might be unreadable
        exists <- doesFileExist f
        verbose $ f ++ if exists then " exists" else " does not exist"
        if exists then return (Just f) else search fs


--------------------------------------------------------------------------------
loadConfig :: Verbose -> Maybe FilePath -> IO Config
loadConfig verbose userSpecified = do
    mbFp <- configFilePath verbose userSpecified
    verbose $ "Loading configuration at " ++ fromMaybe "<embedded>" mbFp
    bytes <- maybe (return defaultConfigBytes) B.readFile mbFp
    case decodeEither bytes of
        Left err     -> error $
            "Language.Haskell.Stylish.Config.loadConfig: " ++ err
        Right config -> return config


--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = do
    -- First load the config without the actual steps
    config <- Config
        <$> pure []
        <*> (o A..:? "columns"             A..!= 80)
        <*> (o A..:? "language_extensions" A..!= [])
        <*> (o A..:? "newline"             >>= parseEnum newlines IO.nativeNewline)

    -- Then fill in the steps based on the partial config we already have
    stepValues <- o A..: "steps" :: A.Parser [A.Value]
    steps      <- mapM (parseSteps config) stepValues
    return config {configSteps = concat steps}
  where
    newlines =
        [ ("native", IO.nativeNewline)
        , ("lf",     IO.LF)
        , ("crlf",   IO.CRLF)
        ]
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (Config -> A.Object -> A.Parser Step)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("language_pragmas",    parseLanguagePragmas)
    , ("simple_align",        parseSimpleAlign)
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
parseSimpleAlign :: Config -> A.Object -> A.Parser Step
parseSimpleAlign c o = SimpleAlign.step
    <$> pure (configColumns c)
    <*> (SimpleAlign.Config
        <$> withDef SimpleAlign.cCases            "cases"
        <*> withDef SimpleAlign.cTopLevelPatterns "top_level_patterns"
        <*> withDef SimpleAlign.cRecords          "records")
  where
    withDef f k = fromMaybe (f SimpleAlign.defaultConfig) <$> (o A..:? k)


--------------------------------------------------------------------------------
parseImports :: Config -> A.Object -> A.Parser Step
parseImports config o = Imports.step
    <$> pure (configColumns config)
    <*> (Imports.Options
        <$> (o A..:? "align" >>= parseEnum aligns (def Imports.importAlign))
        <*> (o A..:? "list_align" >>= parseEnum listAligns (def Imports.listAlign))
        <*> (o A..:? "pad_module_names" A..!= def Imports.padModuleNames)
        <*> (o A..:? "long_list_align"
            >>= parseEnum longListAligns (def Imports.longListAlign))
        -- Note that padding has to be at least 1. Default is 4.
        <*> (o A..:? "empty_list_align"
            >>= parseEnum emptyListAligns (def Imports.emptyListAlign))
        <*> o A..:? "list_padding" A..!= (def Imports.listPadding)
        <*> o A..:? "separate_lists" A..!= (def Imports.separateLists)
        <*> o A..:? "space_surround" A..!= (def Imports.spaceSurround))
  where
    def f = f Imports.defaultOptions

    aligns =
        [ ("global", Imports.Global)
        , ("file",   Imports.File)
        , ("group",  Imports.Group)
        , ("none",   Imports.None)
        ]

    listAligns =
        [ ("new_line",    Imports.NewLine)
        , ("with_alias",  Imports.WithAlias)
        , ("after_alias", Imports.AfterAlias)
        ]

    longListAligns =
        [ ("inline",             Imports.Inline)
        , ("new_line",           Imports.InlineWithBreak)
        , ("new_line_multiline", Imports.InlineToMultiline)
        , ("multiline",          Imports.Multiline)
        ]

    emptyListAligns =
        [ ("inherit", Imports.Inherit)
        , ("right_after", Imports.RightAfter)
        ]

--------------------------------------------------------------------------------
parseLanguagePragmas :: Config -> A.Object -> A.Parser Step
parseLanguagePragmas config o = LanguagePragmas.step
    <$> pure (configColumns config)
    <*> (o A..:? "style" >>= parseEnum styles LanguagePragmas.Vertical)
    <*> o A..:? "align" A..!= True
    <*> o A..:? "remove_redundant" A..!= True
  where
    styles =
        [ ("vertical",     LanguagePragmas.Vertical)
        , ("compact",      LanguagePragmas.Compact)
        , ("compact_line", LanguagePragmas.CompactLine)
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
