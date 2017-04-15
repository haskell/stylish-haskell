--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
    ( parseModule
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (isPrefixOf, nub)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import qualified Language.Haskell.Exts           as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
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
-- | Remove shebang from the first line
unShebang :: String -> String
unShebang str
    | "#!" `isPrefixOf` str = unlines $ ("" :) $ drop 1 $ lines str
    | otherwise             = str


--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str


--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> Either String Module
parseModule extraExts mfp string = do
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
