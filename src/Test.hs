
{-# LANGUAGE MagicHash #-}

module Test where

import Acn.Ids
import Acn.Syntax
--import Compilers.AcnToVerilog

import Control.Monad.State
import GHC.Stack


instance NameMonad (State IdSet) where
    nameNormalizerM = pure $ \nm -> Name nm nm [] Basic emptyCallStack

acnComponent :: State IdSet Component
acnComponent = do
    inputId <- newName "in"

    let
      inputNet = NetDeclarator Nothing inputId (BitVector 32) Nothing

    inputId2 <- newName "in"
    inputId3 <- newName "in"
    inputId4 <- newName "in"
    inputId5 <- newName "in"
    
    let
      inputNet2 = NetDeclarator Nothing inputId2 (BitVector 12) Nothing
      inputNet3 = NetDeclarator Nothing inputId3 (BitVector 14) Nothing
      inputNet4 = NetDeclarator Nothing inputId4 (BitVector 16) Nothing
      inputNet5 = NetDeclarator Nothing inputId5 (BitVector 18) Nothing
      
    logic1  <- newName "logic"
    logic2  <- newName "logic"

    let
      constr1 = NetConstructor (verbatimId# "cons1") [1]
      constr2 = NetConstructor (verbatimId# "cons2") [0]

      fields = [NetField 14 23 (Signed 10), NetField 0 13 (Signed 14)]

      ty = CartesianType (verbatimId# "MyType")
             [constr1, constr2] fields

      logicNet1 = NetDeclarator Nothing logic1 (BitVector 24) Nothing
      logicNet2 = NetDeclarator Nothing logic2 (Cartesian ty) Nothing

    let subComp = verbatimId# "SubComponent"
    subCompName <- newName "sub_component"

    let
      ports = IndexedPortMap [ (In, BitVector 32, Identifier inputId)
                             , (Out, BitVector 24, Identifier logic1)
                             ]

      logicA1 = InstAssignment [logicNet1] [] subComp subCompName [] ports
      logicA2 = Assignment logicNet2
                  $ DataCon (Cartesian ty) 1 [Literal (Just $ Signed 10) $ NumLit 5]

    resId <- newName "res"

    let
      resNet = NetDeclarator Nothing resId (BitVector 24) Nothing

      resA = CondAssignment resNet (Identifier inputId) (BitVector 32)
               [ Dependent (NumLit 123) (Identifier logic1)
               , Dependent (NumLit 456) (Slice (Identifier logic2) 23 14)
               , Default (Literal Nothing $ NumLit 69)
               ]

    let
      inputs = [inputNet, inputNet2, inputNet3, inputNet4, inputNet5]

      compName = verbatimId# "Component"
      component = Component compName inputs [logicA1, logicA2] [resA]

    return component


main :: IO ()
main = do
    print $ evalState acnComponent emptyIdSet

