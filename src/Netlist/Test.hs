
{-# LANGUAGE MagicHash #-}

module Netlist.Test where

import Netlist.AcnSyntax
import Netlist.AcnIds
import Netlist.AcnToVerilog

import Control.Monad.State
import GHC.Stack


newtype NetlistMonad a
    = NetlistMonad { runNetlistM :: State AcnIdSet a }
    deriving (Functor, Applicative, Monad)

instance AcnIdSetMonad NetlistMonad where
    acnIdSetM = NetlistMonad . acnIdSetM

instance AcnNameMonad NetlistMonad where
    acnNameNormalizerM =
        NetlistMonad . pure $ \nm -> AcnName nm nm [] Basic emptyCallStack

acnTest :: Doc
acnTest = flip evalState (VerilogState emptyAcnSet) $ acnToVerilogComponent =<< do
    inputId <- newAcnName "in"

    let
      inputNet = NetDeclarator Nothing inputId (BitVector 32) Nothing

    inputId2 <- newAcnName "in"
    inputId3 <- newAcnName "in"
    inputId4 <- newAcnName "in"
    inputId5 <- newAcnName "in"
    
    let
      inputNet2 = NetDeclarator Nothing inputId2 (BitVector 12) Nothing
      inputNet3 = NetDeclarator Nothing inputId3 (BitVector 14) Nothing
      inputNet4 = NetDeclarator Nothing inputId4 (BitVector 16) Nothing
      inputNet5 = NetDeclarator Nothing inputId5 (BitVector 18) Nothing
      
    logic1  <- newAcnName "logic"
    logic2  <- newAcnName "logic"

    let
      constr1 = NetConstr (verbatimId# "cons1") [1]
      constr2 = NetConstr (verbatimId# "cons2") [0]

      fields = [NetField 14 23 (Signed 10), NetField 0 13 (Signed 14)]

      ty = CartesianType (verbatimId# "MyType")
             [constr1, constr2] fields

      logicNet1 = NetDeclarator Nothing logic1 (BitVector 24) Nothing
      logicNet2 = NetDeclarator Nothing logic2 (Cartesian ty) Nothing

    let subComp = verbatimId# "SubComponent"
    subCompName <- newAcnName "sub_component"

    let
      ports = IndexedPortMap [ (In, BitVector 32, Identifier inputId)
                             , (Out, BitVector 24, Identifier logic1)
                             ]

      logicA1 = InstDecl [logicNet1] [] subComp subCompName [] ports
      logicA2 = Assignment logicNet2
                  $ DataCon (Cartesian ty) 1 [Literal (Just $ Signed 10) $ NumLit 5]

    resId <- newAcnName "res"

    let
      resNet = NetDeclarator Nothing resId (BitVector 24) Nothing

      resA = CondAssignment resNet (Identifier inputId) (BitVector 32)
               [ (Just $ NumLit 123, Identifier logic1)
               , (Just $ NumLit 456, Slice (Identifier logic2) 23 14)
               , (Nothing, Literal Nothing $ NumLit 69)
               ]

    let
      compName = verbatimId# "Component"
      component = AcnComponent compName
                    [inputNet, inputNet2, inputNet3, inputNet4, inputNet5] [logicA1, logicA2] [resA]

    return component


main :: IO ()
main = do
    print acnTest
