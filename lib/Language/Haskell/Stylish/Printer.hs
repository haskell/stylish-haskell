{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Language.Haskell.Stylish.Printer
  ( Printer(..)
  , PrinterState(..)

    -- * Alias
  , P

    -- * Functions to use the printer
  , runPrinter

    -- ** Combinators
  , comma
  , dot
  , newline
  , parenthesize
  , prefix
  , putText
  , sep
  , space
  , suffix
  , indent
  , indented
    -- ** Outputable helpers
  , showOutputable
  , compareOutputable
  ) where

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config (Config')
import           Language.Haskell.Stylish.Parse (baseDynFlags)

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, replicateM)
import           Control.Monad.Reader            (MonadReader, ReaderT(..))
import           Control.Monad.State             (MonadState, State)
import           Control.Monad.State             (execState, gets, modify)
import           GHC.Generics                    (Generic)
import qualified Outputable                      as GHC
import           Prelude                         hiding (lines)

--------------------------------------------------------------------------------

type P = Printer
type Lines = [String]

newtype Printer a = Printer (ReaderT Config' (State PrinterState) a)
  deriving (Applicative, Functor, Monad, MonadReader Config', MonadState PrinterState)

data PrinterState = PrinterState
  { lines :: Lines
  , linePos :: !Int
  , currentLine :: String
  }
  deriving stock (Generic)

runPrinter :: Config' -> Printer a -> Lines
runPrinter cfg (Printer printer) =
  let
    PrinterState parsedLines _ startedLine = runReaderT printer cfg `execState` PrinterState [] 0 ""
  in
    parsedLines <> if startedLine == [] then [] else [startedLine]

putText :: String -> P ()
putText txt = do
  l <- gets currentLine
  modify (\s -> s { currentLine = l <> txt })

newline :: P ()
newline = do
  l <- gets currentLine
  modify (\s -> s { currentLine = "", linePos = 0, lines = lines s <> [l] })

space :: P ()
space = putText " "

dot :: P ()
dot = putText "."

comma :: P ()
comma = putText ","

parenthesize :: P a -> P a
parenthesize action = putText "(" *> action <* putText ")"

sep :: P a -> [P a] -> P ()
sep _ [] = pure ()
sep s (first : rest) = do
  first >> forM_ rest ((>>) s)

prefix :: P a -> P b -> P b
prefix pa pb = pa >> pb

suffix :: P a -> P b -> P a
suffix pa pb = pb >> pa

indented :: Int -> [P a] -> [P a]
indented i = fmap \x -> replicateM i space >> x

indent :: Int -> P a -> P a
indent i = (>>) (replicateM i space)

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

compareOutputable :: GHC.Outputable a => a -> a -> Ordering
compareOutputable i0 i1 = compare (showOutputable i0) (showOutputable i1)
