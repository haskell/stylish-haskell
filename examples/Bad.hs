{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
            ViewPatterns,
    ScopedTypeVariables #-}

module Bad where

import Control.Applicative ((<$>))
import System.Directory (doesFileExist)

import qualified Data.Map as M
import      Data.Map    ((!), keys, Map)   

data Point = Point
    { pointX, pointY :: Double
    , pointName :: String
    } deriving (Show)
