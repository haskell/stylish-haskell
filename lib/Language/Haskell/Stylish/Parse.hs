{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
  ( parseModule
  ) where


--------------------------------------------------------------------------------
import           Data.Function                   ((&))
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           System.IO.Unsafe                (unsafePerformIO)

--------------------------------------------------------------------------------
import           Bag                             (bagToList)
import qualified DynFlags                        as GHC
import qualified ErrUtils                        as GHC
import           FastString                      (mkFastString)
import qualified GHC.Hs                          as GHC
import qualified GHC.LanguageExtensions          as GHC
import qualified HeaderInfo                      as GHC
import qualified HscTypes                        as GHC
import           Lexer                           (ParseResult (..))
import           Lexer                           (mkPState, unP)
import qualified Lexer                           as GHC
import qualified Panic                           as GHC
import qualified Parser                          as GHC
import           SrcLoc                          (mkRealSrcLoc)
import qualified SrcLoc                          as GHC
import           StringBuffer                    (stringToStringBuffer)
import qualified StringBuffer                    as GHC

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC    (baseDynFlags)
import           Language.Haskell.Stylish.Module

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
