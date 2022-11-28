
{-# LANGUAGE DeriveAnyClass #-}

module Acn.Primitives
    ( HDL (..)
    , BlackBox (..)
    , BlackBoxTemplate
    , Element
    , Domain (..)
    , DomainName
    , ActiveEdge (..)
    , ResetKind (..)
    , InitBehaviour (..)
    , ResetPolarity (..)
    )
  where

import Control.DeepSeq
import Data.Text (Text)
import GHC.Generics
  

data HDL = Verilog | VHDL
    deriving (Show, Generic, NFData)
    

type BlackBoxTemplate = [Element]

data Element = Element
    deriving Show

data BlackBox = BlackBox
    deriving Show


data Domain
    = Domain
        { domainName        :: !DomainName
        , period            :: !Int
        , activeEdge        :: !ActiveEdge
        , resetKind         :: !ResetKind
        , initBehaviour     :: !InitBehaviour
        , polarity          :: !ResetPolarity
        }
    deriving (Show, Generic, NFData)

type DomainName = Text

data ActiveEdge
    = Rising
    -- ^ Elements are sensitive to the rising edge of the domain.
    | Falling
    -- ^ Elements are sensitive to the falling edge of this domain.
    deriving (Show, Generic, NFData)

data ResetKind
    = Asynchronous
    -- ^ Elements disregard the state of the clock when responding to
    -- reset signals.
    | Synchronous
    -- ^ Elements wait until the next clock activation before responding
    -- to a reset signal.
    deriving (Show, Generic, NFData)

data ResetPolarity
    = ActiveHigh
    -- ^ Reset when the underlying reset value is high.
    | ActiveLow
    -- ^ Reset when the underlying reset value is low.
    deriving (Show, Generic, NFData)
    
data InitBehaviour
    = Unknown
    -- ^ We don't know what value to give the element on powerup.
    | Defined
    deriving (Show, Generic, NFData)


