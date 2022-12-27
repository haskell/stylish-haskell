--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                              (Test, testGroup)
import           Test.Framework.Providers.HUnit              (testCase)
import           Test.HUnit                                  (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.UnicodeSyntax
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.UnicodeSyntax.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04 (GADTs)" case04
    , testCase "case 05 (Linear types)" case05
    , testCase "case 06 (Forall)" case06
    , testCase "case 07 (do notation)" case07
    , testCase "case 08 (arrow syntax)" case08
    , testCase "case 09 (TH quotes)" case09
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet (step True "LANGUAGE")
    [ "sort :: Ord a => [a] -> [a]"
    , "sort _ = []"
    ]
    [ "{-# LANGUAGE UnicodeSyntax #-}"
    , "sort ∷ Ord a ⇒ [a] → [a]"
    , "sort _ = []"
    ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = assertSnippet (step True "LaNgUaGe")
    [ "sort :: Ord a => [a] -> [a]"
    , "sort _ = []"
    ]
    [ "{-# LaNgUaGe UnicodeSyntax #-}"
    , "sort ∷ Ord a ⇒ [a] → [a]"
    , "sort _ = []"
    ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = assertSnippet (step False "LANGUAGE")
    [ "x :: Int -> Int -> Int"
    , "x = undefined"
    ]
    [ "x ∷ Int → Int → Int"
    , "x = undefined"
    ]

case04 :: Assertion
case04 = assertSnippet (step False "LANGUAGE")
    [ "data Foo where"
    , "  Foo0 :: Foo"
    , "  Foo1 :: Int -> Foo"
    , "  Foo2 :: Int -> Bool -> Foo"
    , "  FooC :: Show a => a -> Foo"
    ]
    [ "data Foo where"
    , "  Foo0 ∷ Foo"
    , "  Foo1 ∷ Int → Foo"
    , "  Foo2 ∷ Int → Bool → Foo"
    , "  FooC ∷ Show a ⇒ a → Foo"
    ]

case05 :: Assertion
case05 = assertSnippet (step False "LANGUAGE")
    [ "{-# LANGUAGE LinearTypes #-}"
    , ""
    , "construct :: Int -> a %1 -> T1 a"
    , "construct _ x = MkT1 x"
    , ""
    , "data T3 a m where"
    , "  MkT3 :: a %m -> T3 a m"
    ]
    [ "{-# LANGUAGE LinearTypes #-}"
    , ""
    , "construct ∷ Int → a %1 → T1 a"
    , "construct _ x = MkT1 x"
    , ""
    , "data T3 a m where"
    , "  MkT3 ∷ a %m → T3 a m"
    ]

case06 :: Assertion
case06 = assertSnippet (step False "LANGUAGE")
    [ "{-# LANGUAGE ScopedTypeVariables #-}"
    , ""
    , "err :: forall a. a"
    , "err = undefined"
    , ""
    , "foo :: forall a. Int -> (forall b. Show b => b -> a) -> Bool"
    , "foo = undefined"
    , ""
    , "data Foo where"
    , "  Foo :: forall a. Show a => a -> Foo"
    ]
    [ "{-# LANGUAGE ScopedTypeVariables #-}"
    , ""
    , "err ∷ ∀ a. a"
    , "err = undefined"
    , ""
    , "foo ∷ ∀ a. Int → (∀ b. Show b ⇒ b → a) → Bool"
    , "foo = undefined"
    , ""
    , "data Foo where"
    , "  Foo ∷ ∀ a. Show a ⇒ a → Foo"
    ]

case07 :: Assertion
case07 = assertSnippet (step False "LANGUAGE")
    [ "main :: IO ()"
    , "  main = do"
    , "  s <- getLine"
    , "  putStrLn s"
    ]
    [ "main ∷ IO ()"
    , "  main = do"
    , "  s ← getLine"
    , "  putStrLn s"
    ]

case08 :: Assertion
case08 = assertSnippet (step False "LANGUAGE")
    [ "{-# LANGUAGE Arrows #-}"
    , ""
    , "a = proc x -> do"
    , "  y <- f -< x+1"
    , "  (|untilA (increment -< x+y) (within 0.5 -< x)|)"
    , ""
    , "b = proc x -> f x -<< x+1"
    ]
    [ "{-# LANGUAGE Arrows #-}"
    , ""
    , "a = proc x → do"
    , "  y ← f ⤙ x+1"
    , "  ⦇untilA (increment ⤙ x+y) (within 0.5 ⤙ x)⦈"
    , ""
    , "b = proc x → f x ⤛ x+1"
    ]

case09 :: Assertion
case09 = assertSnippet (step False "LANGUAGE")
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "exp = [| 2 + 2 |]"
    ]
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "exp = ⟦ 2 + 2 ⟧"
    ]
