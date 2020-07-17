{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Util
    ( nameToString
    , isOperator
    , indent
    , padRight
    , everything
    , infoPoints
    , trimLeft
    , trimRight
    , wrap
    , wrapRest
    , wrapMaybe
    , wrapRestMaybe

    , getStartLineUnsafe
    , getEndLineUnsafe

    , withHead
    , withInit
    , withTail
    , withLast
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                 ((&&&), (>>>))
import           Data.Char                     (isAlpha, isSpace)
import           Data.Data                     (Data)
import qualified Data.Generics                 as G
import           Data.Maybe                    (fromMaybe, listToMaybe,
                                                maybeToList)
import           Data.Typeable                 (cast)
import qualified Language.Haskell.Exts         as H
import           SrcLoc                        (GenLocated(..), Located, SrcSpan(..))
import           SrcLoc                        (srcSpanStartLine, srcSpanEndLine)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str


--------------------------------------------------------------------------------
isOperator :: H.Name l -> Bool
isOperator = fromMaybe False
    . (fmap (not . isAlpha) . listToMaybe)
    . nameToString

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
infoPoints :: H.SrcSpanInfo -> [((Int, Int), (Int, Int))]
infoPoints = H.srcInfoPoints >>> map (H.srcSpanStart &&& H.srcSpanEnd)


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
    noWrap' ss [] = [ss]
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

getStartLineUnsafe :: Located a -> Int
getStartLineUnsafe = \case
  (L (RealSrcSpan s) _) -> srcSpanStartLine s
  _ -> error "could not get start line of block"

getEndLineUnsafe :: Located a -> Int
getEndLineUnsafe = \case
  (L (RealSrcSpan s) _) -> srcSpanEndLine s
  _ -> error "could not get end line of block"

