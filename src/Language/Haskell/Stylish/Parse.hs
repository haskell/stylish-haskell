--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
    ( parseModule
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error             (throwError)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Step


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
-- | If
--
-- > {-# LANGUAGE Haskell2010 #-}
--
-- is specified, we need to add a number of extra extensions
addHaskell2010Extensions :: [H.Extension] -> [H.Extension]
addHaskell2010Extensions exts
    -- For some reason Haskell2010 is not supported yet in haskell-src-exts
    | H.UnknownExtension "Haskell2010" `elem` exts = exts ++ extra
    | otherwise                                    = exts
  where
    extra =
        [ H.PatternGuards
        , H.RelaxedPolyRec
        , H.EmptyDataDecls
        , H.ForeignFunctionInterface
        , H.UnknownExtension "NoNPlusKPatterns"  -- Not supported yet in HSE
        ]


--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> Either String Module
parseModule extraExts mfp string = do
    -- Determine the extensions: those specified in the file and the extra ones
    let noBom      = dropBom string
        extraExts' = map H.classifyExtension extraExts
        fileExts   = fromMaybe [] $ H.readExtensions noBom
        exts       = addHaskell2010Extensions $ fileExts ++ extraExts'

        -- Parsing options...
        fp       = fromMaybe "<unknown>" mfp
        mode     = H.defaultParseMode
            {H.extensions = exts, H.fixities = Nothing}

        -- Preprocessing
        noCpp    = if H.CPP `elem` exts then unCpp noBom else noBom

    case H.parseModuleWithComments mode noCpp of
        H.ParseOk md -> return md
        err          -> throwError $
            "Language.Haskell.Stylish.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
