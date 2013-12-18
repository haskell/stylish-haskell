--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.LanguagePragmas
    ( Style (..)
    , step

      -- * Utilities
    , addLanguagePragma
    ) where


--------------------------------------------------------------------------------
import qualified Data.Set                        as S
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import Language.Haskell.Stylish.Block
import Language.Haskell.Stylish.Editor
import Language.Haskell.Stylish.Step
import Language.Haskell.Stylish.Util


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
verticalPragmas :: Int -> [String] -> Lines
verticalPragmas longest pragmas' =
    [ "{-# LANGUAGE " ++ padRight longest pragma ++ " #-}"
    | pragma <- pragmas'
    ]


--------------------------------------------------------------------------------
compactPragmas :: Int -> [String] -> Lines
compactPragmas columns pragmas' = wrap columns "{-# LANGUAGE" 13 $
    map (++ ",") (init pragmas') ++ [last pragmas', "#-}"]


--------------------------------------------------------------------------------
compactLinePragmas :: Int -> [String] -> Lines
compactLinePragmas _ [] = []
compactLinePragmas columns pragmas' =
  let maxWidth = columns - 16
      longest  = maximum $ map length prags
      prags    = map truncateComma $ wrap maxWidth "" 1 $
                    map (++ ",") (init pragmas') ++ [last pragmas']
  in map (wrapLanguage . padRight longest) prags
  where
    wrapLanguage ps = "{-# LANGUAGE" ++ ps ++  " #-}"


--------------------------------------------------------------------------------
truncateComma :: String -> String
truncateComma ""     = ""
truncateComma xs
    | last xs == ',' = init xs
    | otherwise      = xs


--------------------------------------------------------------------------------
prettyPragmas :: Int -> Int -> Style -> [String] -> Lines
prettyPragmas _       longest Vertical    = verticalPragmas longest
prettyPragmas columns _       Compact     = compactPragmas columns
prettyPragmas columns _       CompactLine = compactLinePragmas columns


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
step :: Int -> Style -> Bool -> Step
step columns style = makeStep "LanguagePragmas" . step' columns style


--------------------------------------------------------------------------------
step' :: Int -> Style -> Bool -> Lines -> Module -> Lines
step' columns style removeRedundant ls (module', _)
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
        [ change b (const $ prettyPragmas columns longest style pg)
        | (b, pg) <- filterRedundant isRedundant' groups
        ]


--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: String -> H.Module H.SrcSpanInfo -> [Change String]
addLanguagePragma prag modu
    | prag `elem` present = []
    | otherwise           = [insert line ["{-# LANGUAGE " ++ prag ++ " #-}"]]
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
    [() | H.PViewPat _ _ _ <- everything m :: [H.Pat H.SrcSpanInfo]]


--------------------------------------------------------------------------------
-- | Check if the BangPatterns language pragma is redundant.
isRedundantBangPatterns :: H.Module H.SrcSpanInfo -> Bool
isRedundantBangPatterns m = null
    [() | H.PBangPat _ _ <- everything m :: [H.Pat H.SrcSpanInfo]]
