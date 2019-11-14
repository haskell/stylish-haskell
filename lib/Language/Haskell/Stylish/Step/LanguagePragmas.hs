--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.LanguagePragmas
    ( Style (..)
    , step
      -- * Utilities
    , addLanguagePragma
    ) where


--------------------------------------------------------------------------------
import qualified Data.Set                        as S
import qualified Language.Haskell.Exts           as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
    | CompactLine
    deriving (Eq, Show)


--------------------------------------------------------------------------------
pragmas :: H.Module l -> [(l, [String])]
pragmas (H.Module _ _ ps _ _) =
    [(l, map nameToString names) | H.LanguagePragma l names <- ps]
pragmas _                     = []


--------------------------------------------------------------------------------
-- | The start of the first block
firstLocation :: [(Block a, [String])] -> Int
firstLocation = minimum . map (blockStart . fst)


--------------------------------------------------------------------------------
verticalPragmas :: Bool -> Int -> Bool -> [String] -> Lines
verticalPragmas lw longest align pragmas' =
    [ "{-# " ++ lowerCaseLang lw ++ " " ++ pad pragma ++ " #-}"
    | pragma <- pragmas'
    ]
  where
    pad
      | align = padRight longest
      | otherwise = id


--------------------------------------------------------------------------------
compactPragmas :: Bool -> Int -> [String] -> Lines
compactPragmas lw columns pragmas' = wrap columns ("{-# " ++ lowerCaseLang lw) 13 $
    map (++ ",") (init pragmas') ++ [last pragmas' ++ " #-}"]


--------------------------------------------------------------------------------
compactLinePragmas :: Bool -> Int -> Bool -> [String] -> Lines
compactLinePragmas _  _ _ [] = []
compactLinePragmas lw columns align pragmas' = map (wrapLanguage . pad) prags
  where
    wrapLanguage ps = "{-# " ++ lowerCaseLang lw ++ ps ++  " #-}"
    maxWidth = columns - 16
    longest  = maximum $ map length prags
    pad
      | align = padRight longest
      | otherwise = id
    prags = map truncateComma $ wrap maxWidth "" 1 $
      map (++ ",") (init pragmas') ++ [last pragmas']


--------------------------------------------------------------------------------
truncateComma :: String -> String
truncateComma ""     = ""
truncateComma xs
    | last xs == ',' = init xs
    | otherwise      = xs


--------------------------------------------------------------------------------
prettyPragmas :: Bool -> Int -> Int -> Bool -> Style -> [String] -> Lines
prettyPragmas lw _    longest align Vertical    = verticalPragmas lw longest align
prettyPragmas lw cols _       _     Compact     = compactPragmas lw cols
prettyPragmas lw cols _       align CompactLine = compactLinePragmas lw cols align

--------------------------------------------------------------------------------
lowerCaseLang :: Bool -> String
lowerCaseLang True  = "language"
lowerCaseLang False = "LANGUAGE"


--------------------------------------------------------------------------------
-- | Filter redundant (and duplicate) pragmas out of the groups. As a side
-- effect, we also sort the pragmas in their group...
filterRedundant :: (String -> Bool)
                -> [(l, [String])]
                -> [(l, [String])]
filterRedundant isRedundant' = snd . foldr filterRedundant' (S.empty, [])
  where
    filterRedundant' (l, xs) (known, zs)
        | S.null xs' = (known', zs)
        | otherwise  = (known', (l, S.toAscList xs') : zs)
      where
        fxs    = filter (not . isRedundant') xs
        xs'    = S.fromList fxs `S.difference` known
        known' = xs' `S.union` known

--------------------------------------------------------------------------------
step :: Int -> Style -> Bool -> Bool -> Bool -> Step
step = ((((makeStep "LanguagePragmas" .) .) .) .) . step'


--------------------------------------------------------------------------------
step' :: Int -> Style -> Bool -> Bool -> Bool -> Lines -> Module -> Lines
step' columns style align removeRedundant allowLower ls (module', _)
    | null pragmas' = ls
    | otherwise     = applyChanges changes ls
  where
    isRedundant'
        | removeRedundant = isRedundant module'
        | otherwise       = const False
    pragmas' = pragmas $ fmap linesFromSrcSpan module'
    longest  = maximum $ map length $ snd =<< pragmas'
    groups   = [(b, concat pgs) | (b, pgs) <- groupAdjacent pragmas']
    changes  =
        [ change b (const $ prettyPragmas allowLower columns longest align style pg)
        | (b, pg) <- filterRedundant isRedundant' groups
        ]


--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: Bool -> String -> H.Module H.SrcSpanInfo -> [Change String]
addLanguagePragma lw prag modu
    | prag `elem` present = []
    | otherwise           = [insert line ["{-# " ++ lowerCaseLang lw ++ " " ++ prag ++ " #-}"]]
  where
    pragmas' = pragmas (fmap linesFromSrcSpan modu)
    present  = concatMap snd pragmas'
    line     = if null pragmas' then 1 else firstLocation pragmas'


--------------------------------------------------------------------------------
-- | Check if a language pragma is redundant. We can't do this for all pragmas,
-- but we do a best effort.
isRedundant :: H.Module H.SrcSpanInfo -> String -> Bool
isRedundant m "ViewPatterns" = isRedundantViewPatterns m
isRedundant m "BangPatterns" = isRedundantBangPatterns m
isRedundant _ _              = False


--------------------------------------------------------------------------------
-- | Check if the ViewPatterns language pragma is redundant.
isRedundantViewPatterns :: H.Module H.SrcSpanInfo -> Bool
isRedundantViewPatterns m = null
    [() | H.PViewPat {} <- everything m :: [H.Pat H.SrcSpanInfo]]


--------------------------------------------------------------------------------
-- | Check if the BangPatterns language pragma is redundant.
isRedundantBangPatterns :: H.Module H.SrcSpanInfo -> Bool
isRedundantBangPatterns m = null
    [() | H.PBangPat _ _ <- everything m :: [H.Pat H.SrcSpanInfo]]
