--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse
    ( parseModule
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error             (throwError)
import           Data.Maybe                      (fromMaybe)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
-- | Filter out lines which use CPP macros
unCpp :: String -> String
unCpp = unlines . map unCpp' . lines
  where
    unCpp' ('#' : _) = ""
    unCpp' xs        = xs


--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str


--------------------------------------------------------------------------------
-- | Read an extension name from a string
parseExtension :: String -> Either String H.Extension
parseExtension str = case reads str of
    [(x, "")] -> return x
    _         -> throwError $ "Unknown extension: " ++ str


--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModule :: Extensions -> Maybe FilePath -> String -> Either String Module
parseModule extraExts mfp string = do
    -- Determine the extensions: those specified in the file and the extra ones 
    extraExts' <- mapM parseExtension extraExts
    let fileExts = fromMaybe [] $ H.readExtensions string
        exts     = fileExts ++ extraExts'

        -- Parsing options...
        fp       = fromMaybe "<unknown>" mfp
        mode     = H.defaultParseMode
            {H.extensions = exts, H.fixities = Nothing}

        -- Preprocessing
        string'  = dropBom $ (if H.CPP `elem` exts then unCpp else id) $ string

    case H.parseModuleWithComments mode string' of
        H.ParseOk md -> return md
        err          -> throwError $
            "Language.Haskell.Stylish.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
