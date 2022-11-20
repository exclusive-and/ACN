
{-# LANGUAGE DeriveAnyClass #-}

module Netlist.AcnPrimitives
    ( HDL (..)
    , BlackBox (..)
    , BlackBoxTemplate
    , Element
    )
  where

import Control.DeepSeq
import GHC.Generics
  

data HDL = Verilog | VHDL
    deriving (Show, Generic, NFData)
    
type BlackBoxTemplate = [Element]

data Element = Element
    deriving Show

data BlackBox = BlackBox
    deriving Show


