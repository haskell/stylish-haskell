--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
module Language.Haskell.Stylish.Util
    ( indent
    , padRight
    , everything
    , trimLeft
    , trimRight
    , wrap
    , wrapRest
    , wrapMaybe
    , wrapRestMaybe

    -- * Extra list functions
    , withHead
    , withInit
    , withTail
    , withLast
    , flagEnds

    , traceOutputable
    , traceOutputableM

    , unguardedRhsBody
    , rhsBody

    , getGuards
    ) where


--------------------------------------------------------------------------------
import           Data.Char                     (isSpace)
import           Data.Data                     (Data)
import qualified Data.Generics                 as G
import           Data.Maybe                    (maybeToList)
import           Data.Typeable                 (cast)
import           Debug.Trace                   (trace)
import qualified GHC.Hs                        as Hs
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Utils.Outputable                                as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.GHC (showOutputable)


--------------------------------------------------------------------------------
indent :: Int -> String -> String
indent len = (indentPrefix len ++)


--------------------------------------------------------------------------------
indentPrefix :: Int -> String
indentPrefix = (`replicate` ' ')


--------------------------------------------------------------------------------
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '


--------------------------------------------------------------------------------
everything :: (Data a, Data b) => a -> [b]
everything = G.everything (++) (maybeToList . cast)


--------------------------------------------------------------------------------
{-
infoPoints :: [S.Located pass] -> [((Int, Int), (Int, Int))]
infoPoints = fmap (helper . S.getLoc)
  where
    helper :: S.SrcSpan -> ((Int, Int), (Int, Int))
    helper (S.RealSrcSpan s) = do
               let
                start = S.realSrcSpanStart s
                end = S.realSrcSpanEnd s
               ((S.srcLocLine start, S.srcLocCol start), (S.srcLocLine end, S.srcLocCol end))
    helper _                   = ((-1,-1), (-1,-1))
-}

--------------------------------------------------------------------------------
trimLeft :: String -> String
trimLeft  = dropWhile isSpace


--------------------------------------------------------------------------------
trimRight :: String -> String
trimRight = reverse . trimLeft . reverse


--------------------------------------------------------------------------------
wrap :: Int       -- ^ Maximum line width
     -> String    -- ^ Leading string
     -> Int       -- ^ Indentation
     -> [String]  -- ^ Strings to add/wrap
     -> Lines     -- ^ Resulting lines
wrap maxWidth leading ind = wrap' leading
  where
    wrap' ss [] = [ss]
    wrap' ss (str:strs)
        | overflows ss str =
            ss : wrapRest maxWidth ind (str:strs)
        | otherwise = wrap' (ss ++ " " ++ str) strs

    overflows ss str = length ss > maxWidth ||
        ((length ss + length str) >= maxWidth && ind + length str  <= maxWidth)


--------------------------------------------------------------------------------
wrapMaybe :: Maybe Int -- ^ Maximum line width (maybe)
          -> String    -- ^ Leading string
          -> Int       -- ^ Indentation
          -> [String]  -- ^ Strings to add/wrap
          -> Lines     -- ^ Resulting lines
wrapMaybe (Just maxWidth) = wrap maxWidth
wrapMaybe Nothing         = noWrap


--------------------------------------------------------------------------------
noWrap :: String    -- ^ Leading string
       -> Int       -- ^ Indentation
       -> [String]  -- ^ Strings to add
       -> Lines     -- ^ Resulting lines
noWrap leading _ind = noWrap' leading
  where
    noWrap' ss []         = [ss]
    noWrap' ss (str:strs) = noWrap' (ss ++ " " ++ str) strs


--------------------------------------------------------------------------------
wrapRest :: Int
         -> Int
         -> [String]
         -> Lines
wrapRest maxWidth ind = reverse . wrapRest' [] ""
  where
    wrapRest' ls ss []
        | null ss = ls
        | otherwise = ss:ls
    wrapRest' ls ss (str:strs)
        | null ss = wrapRest' ls (indent ind str) strs
        | overflows ss str = wrapRest' (ss:ls) "" (str:strs)
        | otherwise = wrapRest' ls (ss ++ " " ++ str) strs

    overflows ss str = (length ss + length str + 1) >= maxWidth


--------------------------------------------------------------------------------
wrapRestMaybe :: Maybe Int
              -> Int
              -> [String]
              -> Lines
wrapRestMaybe (Just maxWidth) = wrapRest maxWidth
wrapRestMaybe Nothing         = noWrapRest


--------------------------------------------------------------------------------
noWrapRest :: Int
           -> [String]
           -> Lines
noWrapRest ind = reverse . noWrapRest' [] ""
  where
    noWrapRest' ls ss []
        | null ss = ls
        | otherwise = ss:ls
    noWrapRest' ls ss (str:strs)
        | null ss = noWrapRest' ls (indent ind str) strs
        | otherwise = noWrapRest' ls (ss ++ " " ++ str) strs


--------------------------------------------------------------------------------
withHead :: (a -> a) -> [a] -> [a]
withHead _ []       = []
withHead f (x : xs) = f x : xs


--------------------------------------------------------------------------------
withLast :: (a -> a) -> [a] -> [a]
withLast _ []       = []
withLast f [x]      = [f x]
withLast f (x : xs) = x : withLast f xs


--------------------------------------------------------------------------------
withInit :: (a -> a) -> [a] -> [a]
withInit _ []       = []
withInit _ [x]      = [x]
withInit f (x : xs) = f x : withInit f xs


--------------------------------------------------------------------------------
withTail :: (a -> a) -> [a] -> [a]
withTail _ []       = []
withTail f (x : xs) = x : map f xs



--------------------------------------------------------------------------------
-- | Utility for traversing through a list and knowing when you're at the
-- first and last element.
flagEnds :: [a] -> [(a, Bool, Bool)]
flagEnds = \case
    [] -> []
    [x] -> [(x, True, True)]
    x : y : zs -> (x, True, False) : go (y : zs)
  where
    go (x : y : zs) = (x, False, False) : go (y : zs)
    go [x]          = [(x, False, True)]
    go []           = []


--------------------------------------------------------------------------------
traceOutputable :: GHC.Outputable a => String -> a -> b -> b
traceOutputable title x =
    trace (title ++ ": " ++ (showOutputable x))


--------------------------------------------------------------------------------
traceOutputableM :: (GHC.Outputable a, Monad m) => String -> a -> m ()
traceOutputableM title x = traceOutputable title x $ pure ()


--------------------------------------------------------------------------------
-- Utility: grab the body out of guarded RHSs if it's a single unguarded one.
unguardedRhsBody :: Hs.GRHSs Hs.GhcPs a -> Maybe a
unguardedRhsBody (Hs.GRHSs _ [grhs] _)
    | Hs.GRHS _ [] body <- GHC.unLoc grhs = Just body
unguardedRhsBody _ = Nothing


-- Utility: grab the body out of guarded RHSs
rhsBody :: Hs.GRHSs Hs.GhcPs a -> Maybe a
rhsBody (Hs.GRHSs _ [grhs] _)
    | Hs.GRHS _ _ body <- GHC.unLoc grhs = Just body
rhsBody _ = Nothing


--------------------------------------------------------------------------------
-- get guards in a guarded rhs of a Match
getGuards :: Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> [Hs.GuardLStmt Hs.GhcPs]
getGuards (Hs.Match _ _ _ grhss) =
  let
    lgrhs = getLocGRHS grhss -- []
    grhs  = map GHC.unLoc lgrhs
  in
    concatMap getGuardLStmts grhs


getLocGRHS :: Hs.GRHSs Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> [Hs.LGRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)]
getLocGRHS (Hs.GRHSs _ guardeds _) = guardeds


getGuardLStmts :: Hs.GRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> [Hs.GuardLStmt Hs.GhcPs]
getGuardLStmts (Hs.GRHS _ guards _) = guards
