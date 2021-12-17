{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
  ( parseModule
  ) where


--------------------------------------------------------------------------------
import           Data.Maybe                                 (listToMaybe)


--------------------------------------------------------------------------------
import           GHC.Driver.Ppr                             as GHC
import qualified GHC.Driver.Session                         as GHC
import qualified GHC.LanguageExtensions.Type                as LangExt
import qualified GHC.Parser.Errors.Ppr                      as GHC
import qualified GHC.Parser.Lexer                           as GHC
import qualified GHC.Utils.Error                            as GHC
import qualified GHC.Utils.Outputable                       as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHCEx
import           Language.Haskell.Stylish.GHC               (baseDynFlags)
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
parseModule _exts fp string =
    let input = removeCpp $ dropBom string in
    case GHCEx.parseModule input dynFlags of
      GHC.POk _ m -> Right m
      GHC.PFailed ps -> Left . withFileName . GHC.showSDoc dynFlags . GHC.vcat .
          GHC.pprMsgEnvelopeBagWithLoc . fmap GHC.pprError . snd $
          GHC.getMessages ps
  where
    -- TODO: Add extensions again.
    dynFlags = baseDynFlags

    removeCpp s = if GHC.xopt LangExt.Cpp dynFlags then unCpp s else s

    withFileName x = maybe "" (<> ": ") fp <> x
