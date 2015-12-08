{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
    ( someFunc
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

someFunc :: IO ()
someFunc = mainWith (circle 1 :: Diagram B)
