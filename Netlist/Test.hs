
{-# LANGUAGE OverloadedStrings #-}

module Netlist.Test where

import Netlist.Acn
import Netlist.AcnToVerilog

import Control.Monad.State
import GHC.Stack


main :: IO ()
main = do
    let inputId  = RawIdentifier "in" Nothing callStack
        inputNet = NetDeclaration (Just "input") inputId (BitVector 32) Nothing

        logicId1  = RawIdentifier "logic1" Nothing callStack
        logicNet1 = NetDeclaration (Just "logic 1") logicId1 (BitVector 24) Nothing
        logicId2  = RawIdentifier "logic2" Nothing callStack
        logicNet2 = NetDeclaration (Just "logic 2") logicId2 (BitVector 16) Nothing

        {-
        subComponentId = RawIdentifier "SubComponent" Nothing callStack
        instanceName = RawIdentifier "sub_comp" Nothing callStack
        pm = IndexedPortMap [ (In, BitVector 32, Identifier inputId)
                            , (Out, BitVector 24, Identifier logicId1)
                            , (Out, BitVector 16, Identifier logicId2)
                            ]
        logicA = InstDecl [logicNet1, logicNet2] [] subComponentId instanceName [] pm
        -}

        constr = NetConstr (RawIdentifier "cons" Nothing callStack) [1]
        fields = [Signed 10, Signed 14]
        ty     = CartesianType (RawIdentifier "MyType" Nothing callStack) [constr] fields
        logicA = Assignment logicNet1 (DataCon (Cartesian ty) 0 [Literal Nothing $ NumLit 5])

        resId  = RawIdentifier "res" Nothing callStack
        resNet = NetDeclaration (Just "result") resId (BitVector 24) Nothing
        resA = CondAssignment resNet (Identifier inputId) (BitVector 32)
                    [ (Just $ NumLit 123, Identifier logicId1)
                    , (Just $ NumLit 456, Identifier logicId2)
                    , (Nothing, Literal Nothing $ NumLit 45)
                    ]

        cName = RawIdentifier "component" Nothing callStack
        component = AcnComponent cName [inputNet] [logicA] [resA]

        doc = flip evalState (VerilogState True) $ acnToVerilogComponent component

    putStrLn $ show doc
