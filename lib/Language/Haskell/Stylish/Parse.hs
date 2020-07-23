{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
  ( parseModule
  , parseModuleHSE
  ) where


--------------------------------------------------------------------------------
import           Data.Function                   ((&))
import           Data.List                       (isPrefixOf, nub)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           System.IO.Unsafe                (unsafePerformIO)

--------------------------------------------------------------------------------
import           Bag                             (bagToList)
import qualified DynFlags                        as GHC
import           FastString                      (mkFastString)
import qualified GHC.Hs                          as GHC
import qualified GHC.LanguageExtensions          as GHC
import qualified ErrUtils                        as GHC
import qualified HeaderInfo                      as GHC
import qualified HscTypes                        as GHC
import           Lexer                           (ParseResult(..))
import           Lexer                           (mkPState, unP)
import qualified Lexer                           as GHC
import qualified Panic                           as GHC
import qualified Parser                          as GHC
import           SrcLoc                          (mkRealSrcLoc)
import qualified SrcLoc                          as GHC
import           StringBuffer                    (stringToStringBuffer)
import qualified StringBuffer                    as GHC

--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts           as H

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC (baseDynFlags)
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step (OldModule)

type Extensions = [String]

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
parseModule exts fp string =
  parsePragmasIntoDynFlags baseDynFlags userExtensions filePath string >>= \dynFlags ->
    dropBom string
      & removeCpp dynFlags
      & runParser dynFlags
      & toModule dynFlags
  where
    toModule :: GHC.DynFlags -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs)) -> Either String Module
    toModule dynFlags res = case res of
      POk ps m ->
        Right (makeModule ps m)
      PFailed failureState ->
        let
          withFileName x = maybe "" (<> ": ") fp <> x
        in
        Left . withFileName . unlines . getParserStateErrors dynFlags $ failureState

    removeCpp dynFlags s =
      if GHC.xopt GHC.Cpp dynFlags then unCpp s
      else s

    userExtensions =
      fmap toLocatedExtensionFlag ("Haskell2010" : exts) -- FIXME: do we need `Haskell2010` here?

    toLocatedExtensionFlag flag
      = "-X" <> flag
      & GHC.L GHC.noSrcSpan

    getParserStateErrors dynFlags state
      = GHC.getErrorMessages state dynFlags
      & bagToList
      & fmap (\errMsg -> show (GHC.errMsgSpan errMsg) <> ": " <> show errMsg)

    filePath =
      fromMaybe "<interactive>" fp

    runParser :: GHC.DynFlags -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs))
    runParser flags str =
      let
        filename = mkFastString filePath
        parseState = mkPState flags (stringToStringBuffer str) (mkRealSrcLoc filename 1 1)
      in
        unP GHC.parseModule parseState

-- | Parse 'DynFlags' from the extra options
--
--   /Note:/ this function would be IO, but we're not using any of the internal
--   features that constitute side effectful computation. So I think it's fine
--   if we run this to avoid changing the interface too much.
parsePragmasIntoDynFlags ::
     GHC.DynFlags
  -> [GHC.Located String]
  -> FilePath
  -> String
  -> Either String GHC.DynFlags
{-# NOINLINE parsePragmasIntoDynFlags #-}
parsePragmasIntoDynFlags originalFlags extraOpts filepath str = unsafePerformIO $ catchErrors $ do
  let opts = GHC.getOptions originalFlags (GHC.stringToStringBuffer str) filepath
  (parsedFlags, _invalidFlags, _warnings) <- GHC.parseDynamicFilePragma originalFlags (opts <> extraOpts)
  -- FIXME: have a look at 'leftovers' since it should be empty
  return $ Right $ parsedFlags `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  where
    catchErrors act = GHC.handleGhcException reportErr (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)

--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModuleHSE :: Extensions -> Maybe FilePath -> String -> Either String OldModule
parseModuleHSE extraExts mfp string = do
    -- Determine the extensions: those specified in the file and the extra ones
    let noPrefixes       = unShebang . dropBom $ string
        extraExts'       = map H.classifyExtension extraExts
        (lang, fileExts) = fromMaybe (Nothing, []) $ H.readExtensions noPrefixes
        exts             = nub $ fileExts ++ extraExts' ++ defaultExtensions

        -- Parsing options...
        fp       = fromMaybe "<unknown>" mfp
        mode     = H.defaultParseMode
            { H.extensions   = exts
            , H.fixities     = Nothing
            , H.baseLanguage = case lang of
                Nothing -> H.baseLanguage H.defaultParseMode
                Just l  -> l
            }

        -- Preprocessing
        processed = if H.EnableExtension H.CPP `elem` exts
                       then unCpp noPrefixes
                       else noPrefixes

    case H.parseModuleWithComments mode processed of
        H.ParseOk md -> return md
        err          -> Left $
            "Language.Haskell.Stylish.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
  where
    -- | Remove shebang lines
    unShebang :: String -> String
    unShebang str =
        let (shebangs, other) = break (not . ("#!" `isPrefixOf`)) (lines str) in
        unlines $ map (const "") shebangs ++ other

    -- | Syntax-related language extensions are always enabled for parsing. Since we
    -- can't authoritatively know which extensions are enabled at compile-time, we
    -- should try not to throw errors when parsing any GHC-accepted code.
    defaultExtensions :: [H.Extension]
    defaultExtensions = map H.EnableExtension
      [ H.GADTs
      , H.HereDocuments
      , H.KindSignatures
      , H.NewQualifiedOperators
      , H.PatternGuards
      , H.StandaloneDeriving
      , H.UnicodeSyntax
      ]
