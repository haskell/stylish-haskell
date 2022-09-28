--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
  ( parseModule
  ) where


--------------------------------------------------------------------------------
import           Data.List                                          (foldl',
                                                                     stripPrefix)
import           Data.Maybe                                         (fromMaybe,
                                                                     listToMaybe,
                                                                     mapMaybe)
import qualified GHC.Data.StringBuffer                              as GHC
import           GHC.Driver.Ppr                                     as GHC
import qualified GHC.Driver.Session                                 as GHC
import qualified GHC.LanguageExtensions.Type                        as LangExt
import qualified GHC.Parser.Errors.Ppr                              as GHC
import qualified GHC.Parser.Header                                  as GHC
import qualified GHC.Parser.Lexer                                   as GHC
import qualified GHC.Types.SrcLoc                                   as GHC
import qualified GHC.Utils.Error                                    as GHC
import qualified GHC.Utils.Outputable                               as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GHCEx
import qualified Language.Haskell.GhclibParserEx.GHC.Parser         as GHCEx


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module


--------------------------------------------------------------------------------
type Extensions = [String]


--------------------------------------------------------------------------------
parseExtension :: String -> Either String (LangExt.Extension, Bool)
parseExtension str = case GHCEx.readExtension str of
    Just e  -> Right (e, True)
    Nothing -> case str of
        'N' : 'o' : str' -> fmap not <$> parseExtension str'
        _                -> Left $ "Unknown extension: " ++ show str


--------------------------------------------------------------------------------
-- | Filter out lines which use CPP macros
unCpp :: String -> String
unCpp = unlines . go False . lines
  where
    go _           []       = []
    go isMultiline (x : xs) =
        let isCpp         = isMultiline || listToMaybe x == Just '#'
            nextMultiline = isCpp && not (null x) && last x == '\\'
        in (if isCpp then "" else x) : go nextMultiline xs


--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str


--------------------------------------------------------------------------------
-- | Abstraction over GHC lib's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> Either String Module
parseModule externalExts0 fp string = do
    -- Parse extensions.
    externalExts1 <- traverse parseExtension externalExts0

    -- Build first dynflags.
    let dynFlags0 = foldl' toggleExt baseDynFlags externalExts1

    -- Parse options from file
    let fileOptions = fmap GHC.unLoc $ GHC.getOptions dynFlags0
            (GHC.stringToStringBuffer string)
            (fromMaybe "-" fp)
        fileExtensions = mapMaybe (\str -> do
            str' <- stripPrefix "-X" str
            case parseExtension str' of
                Left _  -> Nothing
                Right x -> pure x)
            fileOptions

    -- Set further dynflags.
    let dynFlags1 = foldl' toggleExt dynFlags0 fileExtensions
            `GHC.gopt_set` GHC.Opt_KeepRawTokenStream

    -- Possibly strip CPP.
    let removeCpp s = if GHC.xopt LangExt.Cpp dynFlags1 then unCpp s else s
        input = removeCpp $ dropBom string

    -- Actual parse.
    case GHCEx.parseModule input dynFlags1 of
        GHC.POk _ m -> Right m
        GHC.PFailed ps -> Left . withFileName . GHC.showSDoc dynFlags1 .
            GHC.vcat . GHC.pprMsgEnvelopeBagWithLoc . fmap GHC.pprError . snd $
            GHC.getMessages ps
  where
    withFileName x = maybe "" (<> ": ") fp <> x

    toggleExt dynFlags (ext, onOff) = foldl'
        toggleExt
        ((if onOff then GHC.xopt_set else GHC.xopt_unset) dynFlags ext)
        [(rhs, onOff') | (lhs, onOff', rhs) <- GHC.impliedXFlags, lhs == ext]
