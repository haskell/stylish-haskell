--------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Stylish.Step.LanguagePragmas
    ( Style (..)
    , step
      -- * Utilities
    , addLanguagePragma
    ) where


--------------------------------------------------------------------------------
import           Data.List.NonEmpty              (NonEmpty, fromList, toList)
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T


--------------------------------------------------------------------------------
import qualified GHC.Hs                          as Hs
import           SrcLoc                          (RealSrcSpan, realSrcSpanStart,
                                                  srcLocLine, srcSpanEndLine,
                                                  srcSpanStartLine)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
    | CompactLine
    deriving (Eq, Show)


--------------------------------------------------------------------------------
verticalPragmas :: String -> Int -> Bool -> [String] -> Lines
verticalPragmas lg longest align pragmas' =
    [ "{-# " ++ lg ++ " " ++ pad pragma ++ " #-}"
    | pragma <- pragmas'
    ]
  where
    pad
      | align = padRight longest
      | otherwise = id


--------------------------------------------------------------------------------
compactPragmas :: String -> Maybe Int -> [String] -> Lines
compactPragmas lg columns pragmas' = wrapMaybe columns ("{-# " ++ lg) 13 $
    map (++ ",") (init pragmas') ++ [last pragmas' ++ " #-}"]


--------------------------------------------------------------------------------
compactLinePragmas :: String -> Maybe Int -> Bool -> [String] -> Lines
compactLinePragmas _  _ _ [] = []
compactLinePragmas lg columns align pragmas' = map (wrapLanguage . pad) prags
  where
    wrapLanguage ps = "{-# " ++ lg ++ ps ++  " #-}"
    maxWidth = fmap (\c -> c - 16) columns
    longest  = maximum $ map length prags
    pad
      | align = padRight longest
      | otherwise = id
    prags = map truncateComma $ wrapMaybe maxWidth "" 1 $
      map (++ ",") (init pragmas') ++ [last pragmas']


--------------------------------------------------------------------------------
truncateComma :: String -> String
truncateComma ""     = ""
truncateComma xs
    | last xs == ',' = init xs
    | otherwise      = xs


--------------------------------------------------------------------------------
prettyPragmas :: String -> Maybe Int -> Int -> Bool -> Style -> [String] -> Lines
prettyPragmas lp _    longest align Vertical    = verticalPragmas lp longest align
prettyPragmas lp cols _       _     Compact     = compactPragmas lp cols
prettyPragmas lp cols _       align CompactLine = compactLinePragmas lp cols align


--------------------------------------------------------------------------------
-- | Filter redundant (and duplicate) pragmas out of the groups. As a side
-- effect, we also sort the pragmas in their group...
filterRedundant :: (Text -> Bool)
                -> [(l, NonEmpty Text)]
                -> [(l, [Text])]
filterRedundant isRedundant' = snd . foldr filterRedundant' (S.empty, []) . fmap (fmap toList)
  where
    filterRedundant' (l, xs) (known, zs)
        | S.null xs' = (known', zs)
        | otherwise  = (known', (l, S.toAscList xs') : zs)
      where
        fxs    = filter (not . isRedundant') xs
        xs'    = S.fromList fxs `S.difference` known
        known' = xs' `S.union` known

--------------------------------------------------------------------------------
step :: Maybe Int -> Style -> Bool -> Bool -> String -> Step
step = ((((makeStep "LanguagePragmas" .) .) .) .) . step'


--------------------------------------------------------------------------------
step' :: Maybe Int -> Style -> Bool -> Bool -> String -> Lines -> Module -> Lines
step' columns style align removeRedundant lngPrefix ls m
  | null languagePragmas = ls
  | otherwise = applyChanges changes ls
  where
    isRedundant'
        | removeRedundant = isRedundant m
        | otherwise       = const False

    languagePragmas = moduleLanguagePragmas m

    convertFstToBlock :: [(RealSrcSpan, a)] -> [(Block String, a)]
    convertFstToBlock = fmap \(rspan, a) ->
      (Block (srcSpanStartLine rspan) (srcSpanEndLine rspan), a)

    groupAdjacent' =
      fmap turnSndBackToNel . groupAdjacent . fmap (fmap toList)
      where
        turnSndBackToNel (a, bss) = (a, fromList . concat $ bss)

    longest :: Int
    longest  = maximum $ map T.length $ toList . snd =<< languagePragmas

    groups :: [(Block String, NonEmpty Text)]
    groups = [(b, pgs) | (b, pgs) <- groupAdjacent' (convertFstToBlock languagePragmas)]

    changes =
      [ change b (const $ prettyPragmas lngPrefix columns longest align style (fmap T.unpack pg))
      | (b, pg) <- filterRedundant isRedundant' groups
      ]

--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: String -> String -> Module -> [Change String]
addLanguagePragma lg prag modu
    | prag `elem` present = []
    | otherwise           = [insert line ["{-# " ++ lg ++ " " ++ prag ++ " #-}"]]
  where
    pragmas'      = moduleLanguagePragmas modu
    present       = concatMap ((fmap T.unpack) . toList . snd) pragmas'
    line          = if null pragmas' then 1 else firstLocation pragmas'
    firstLocation :: [(RealSrcSpan, NonEmpty Text)] -> Int
    firstLocation = minimum . fmap (srcLocLine . realSrcSpanStart . fst)


--------------------------------------------------------------------------------
-- | Check if a language pragma is redundant. We can't do this for all pragmas,
-- but we do a best effort.
isRedundant :: Module -> Text -> Bool
isRedundant m "ViewPatterns" = isRedundantViewPatterns m
isRedundant m "BangPatterns" = isRedundantBangPatterns m
isRedundant _ _              = False


--------------------------------------------------------------------------------
-- | Check if the ViewPatterns language pragma is redundant.
isRedundantViewPatterns :: Module -> Bool
isRedundantViewPatterns = null . queryModule getViewPat
  where
    getViewPat :: Hs.Pat Hs.GhcPs -> [()]
    getViewPat = \case
      Hs.ViewPat{} -> [()]
      _            -> []


--------------------------------------------------------------------------------
-- | Check if the BangPatterns language pragma is redundant.
isRedundantBangPatterns :: Module -> Bool
isRedundantBangPatterns modul =
    (null $ queryModule getBangPat modul) &&
    (null $ queryModule getMatchStrict modul)
  where
    getBangPat :: Hs.Pat Hs.GhcPs -> [()]
    getBangPat = \case
      Hs.BangPat{} -> [()]
      _            -> []

    getMatchStrict :: Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> [()]
    getMatchStrict (Hs.XMatch m) = Hs.noExtCon m
    getMatchStrict (Hs.Match _ ctx _ _) = case ctx of
      Hs.FunRhs _ _ Hs.SrcStrict -> [()]
      _                          -> []
