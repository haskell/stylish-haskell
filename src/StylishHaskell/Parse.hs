--------------------------------------------------------------------------------
module StylishHaskell.Parse
    ( parseModule
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (fromMaybe)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
-- | Filter out lines which use CPP macros
unCpp :: String -> String
unCpp = unlines . map unCpp' . lines
  where
    unCpp' ('#' : _) = ""
    unCpp' xs        = xs


--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModule :: Maybe FilePath -> String -> Either String Module
parseModule mfp string =
    let fp       = fromMaybe "<unknown>" mfp
        -- Determine the extensions used in the file, and update the parsing
        -- mode based upon those
        exts     = fromMaybe [] $ H.readExtensions string
        mode     = H.defaultParseMode
            {H.extensions = exts, H.fixities = Nothing}
        -- Special handling for CPP, haskell-src-exts can't deal with it
        string'  = if H.CPP `elem` exts then unCpp string else string
    in case H.parseModuleWithComments mode string' of
        H.ParseOk md -> Right md
        err          -> Left $
            "StylishHaskell.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
