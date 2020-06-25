--------------------------------------------------------------------------------
module Language.Haskell.Stylish.ParseGHC
    ( parseModule
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe, listToMaybe, mapMaybe)
import           Data.Foldable                   (foldMap)

import qualified SrcLoc                          as S
import qualified Lexer                           as L
import qualified GHC.Hs                          as G
import qualified GHC.Hs.Extension                as GE
import qualified DynFlags                        as D

import qualified Language.Haskell.TH.LanguageExtensions as Ext

import qualified Language.Haskell.GhclibParserEx.GHC.Parser as Parser

import Language.Haskell.GhclibParserEx.GHC.Settings.Config
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (readExtension, parsePragmasIntoDynFlags)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
-- | Syntax-related language extensions are always enabled for parsing. Since we
-- can't authoritatively know which extensions are enabled at compile-time, we
-- should try not to throw errors when parsing any GHC-accepted code.
defaultExtensions :: [Ext.Extension] -- uncompleted list of extensions
defaultExtensions  = [Ext.GADTs,
                      Ext.KindSignatures,
                      Ext.UnicodeSyntax,
                      Ext.PatternGuards,
                      Ext.StandaloneDeriving]


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
-- | Remove shebang lines
unShebang :: String -> String
unShebang str =
    let (shebangs, other) = break (not . ("#!" `isPrefixOf`)) (lines str) in
    unlines $ map (const "") shebangs ++ other


--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str

--------------------------------------------------------------------------------
-- | List of DynFlags to obtain before parsing
baseDynFlags :: D.DynFlags
baseDynFlags = D.defaultDynFlags fakeSettings fakeLlvmConfig

--------------------------------------------------------------------------------
-- | Abstraction over GHC's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> IO (Either String GHCModule)
parseModule extraExts mfp string = do
  -- Determine the extensions: those specified in the file and the extra ones
    let noPrefixes        = unShebang . dropBom $ string
        extraExts'        = mapMaybe readExtension extraExts
        enableDisableExts = (extraExts' ++ defaultExtensions ,[])   

        fp       = fromMaybe "<unknown>" mfp

        processed = if Ext.Cpp `elem` fst enableDisableExts
                    then unCpp noPrefixes
                    else noPrefixes

    dynFlags <- parsePragmasIntoDynFlags baseDynFlags enableDisableExts fp processed
    return $ case dynFlags of
      Right ghcFlags -> case Parser.parseModule processed ghcFlags of
        L.POk _st md -> Right md
        L.PFailed st -> let err = L.getErrorMessages st ghcFlags in
           Left $ "Language.Haskell.Stylish.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ foldMap (\x -> show x ++ "\n") err
      Left msg -> Left $ "Language.Haskell.Stylish.Parse.parseModule: could not parse pragmas into dynamic flags: " ++ show msg

