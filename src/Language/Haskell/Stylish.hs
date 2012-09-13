module Language.Haskell.Stylish
    ( -- * Run
      runSteps
      -- * Steps
    , imports
    , languagePragmas
    , records
    , tabs
    , trailingWhitespace
    , unicodeSyntax
      -- ** Data types
    , Imports.Align (..)
    , LanguagePragmas.Style (..)
      -- ** Helpers
    , stepName
      -- * Config
    , module StylishHaskell.Config
      -- * Misc
    , module StylishHaskell.Verbose
    , version
    , Lines
    , Step
    ) where

import StylishHaskell
import StylishHaskell.Config
import StylishHaskell.Step
import StylishHaskell.Verbose
import Paths_stylish_haskell  (version)

import qualified StylishHaskell.Step.Imports as Imports
import qualified StylishHaskell.Step.LanguagePragmas as LanguagePragmas
import qualified StylishHaskell.Step.Records as Records
import qualified StylishHaskell.Step.Tabs as Tabs
import qualified StylishHaskell.Step.TrailingWhitespace as TrailingWhitespace
import qualified StylishHaskell.Step.UnicodeSyntax as UnicodeSyntax

imports :: Int -- ^ columns
        -> Imports.Align
        -> Step
imports = Imports.step

languagePragmas :: Int -- ^ columns
                -> LanguagePragmas.Style
                -> Bool -- ^ remove redundant?
                -> Step
languagePragmas = LanguagePragmas.step

records :: Step
records = Records.step

tabs :: Int -- ^ number of spaces
     -> Step
tabs = Tabs.step

trailingWhitespace :: Step
trailingWhitespace = TrailingWhitespace.step

unicodeSyntax :: Bool -- ^ add language pragma?
              -> Step
unicodeSyntax = UnicodeSyntax.step
