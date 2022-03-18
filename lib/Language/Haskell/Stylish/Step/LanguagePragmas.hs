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


--------------------------------------------------------------------------------
import qualified GHC.Hs                          as GHC
import qualified GHC.Types.SrcLoc                as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import qualified Language.Haskell.Stylish.Editor as Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
    | CompactLine
    | VerticalCompact
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
verticalCompactPragmas :: String -> [String] -> Lines
verticalCompactPragmas lg pragmas' =
  [ "{-# " <> lg
  , "    " <> head pragmas'
  ]
  <> [ "  , "  <> pragma | pragma <- tail pragmas']
  <> [ "  #-}"]


--------------------------------------------------------------------------------
truncateComma :: String -> String
truncateComma ""     = ""
truncateComma xs
    | last xs == ',' = init xs
    | otherwise      = xs


--------------------------------------------------------------------------------
prettyPragmas :: String -> Maybe Int -> Int -> Bool -> Style -> [String] -> Lines
prettyPragmas lp _    longest align Vertical        = verticalPragmas lp longest align
prettyPragmas lp cols _       _     Compact         = compactPragmas lp cols
prettyPragmas lp cols _       align CompactLine     = compactLinePragmas lp cols align
prettyPragmas lp _    _       _     VerticalCompact = verticalCompactPragmas lp


--------------------------------------------------------------------------------
-- | Filter redundant (and duplicate) pragmas out of the groups. As a side
-- effect, we also sort the pragmas in their group...
filterRedundant :: (String -> Bool)
                -> [(l, NonEmpty String)]
                -> [(l, [String])]
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
  | otherwise = Editor.apply changes ls
  where
    isRedundant'
        | removeRedundant = isRedundant m
        | otherwise       = const False

    languagePragmas = moduleLanguagePragmas m

    convertFstToBlock :: [(GHC.RealSrcSpan, a)] -> [(Block String, a)]
    convertFstToBlock = fmap \(rspan, a) ->
      (Block (GHC.srcSpanStartLine rspan) (GHC.srcSpanEndLine rspan), a)

    groupAdjacent' =
      fmap turnSndBackToNel . groupAdjacent . fmap (fmap toList)
      where
        turnSndBackToNel (a, bss) = (a, fromList . concat $ bss)

    longest :: Int
    longest  = maximum $ map length $ toList . snd =<< languagePragmas

    groups :: [(Block String, NonEmpty String)]
    groups = [(b, pgs) | (b, pgs) <- groupAdjacent' (convertFstToBlock languagePragmas)]

    changes = mconcat
      [ Editor.changeLines b (const $ prettyPragmas lngPrefix columns longest align style pg)
      | (b, pg) <- filterRedundant isRedundant' groups
      ]


--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: String -> String -> Module -> Editor.Edits
addLanguagePragma lg prag modu
    | prag `elem` present = mempty
    | otherwise           = Editor.insertLines line ["{-# " ++ lg ++ " " ++ prag ++ " #-}"]
  where
    pragmas'      = moduleLanguagePragmas modu
    present       = concatMap (toList . snd) pragmas'
    line          = if null pragmas' then 1 else firstLocation pragmas'
    firstLocation :: [(GHC.RealSrcSpan, NonEmpty String)] -> Int
    firstLocation = minimum . fmap (GHC.srcLocLine . GHC.realSrcSpanStart . fst)


--------------------------------------------------------------------------------
-- | Check if a language pragma is redundant. We can't do this for all pragmas,
-- but we do a best effort.
isRedundant :: Module -> String -> Bool
isRedundant m "ViewPatterns" = isRedundantViewPatterns m
isRedundant m "BangPatterns" = isRedundantBangPatterns m
isRedundant _ _              = False


--------------------------------------------------------------------------------
-- | Check if the ViewPatterns language pragma is redundant.
isRedundantViewPatterns :: Module -> Bool
isRedundantViewPatterns = null . queryModule getViewPat
  where
    getViewPat :: GHC.Pat GHC.GhcPs -> [()]
    getViewPat = \case
      GHC.ViewPat{} -> [()]
      _             -> []


--------------------------------------------------------------------------------
-- | Check if the BangPatterns language pragma is redundant.
isRedundantBangPatterns :: Module -> Bool
isRedundantBangPatterns modul =
    (null $ queryModule getBangPat modul) &&
    (null $ queryModule getMatchStrict modul)
  where
    getBangPat :: GHC.Pat GHC.GhcPs -> [()]
    getBangPat = \case
      GHC.BangPat{} -> [()]
      _             -> []

    getMatchStrict :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [()]
    getMatchStrict (GHC.Match _ ctx _ _) = case ctx of
      GHC.FunRhs _ _ GHC.SrcStrict -> [()]
      _                            -> []
