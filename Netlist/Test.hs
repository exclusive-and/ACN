
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
        logicNet2 = NetDeclaration (Just "logic 2") logicId2 (BitVector 24) Nothing

        
        subComponentId = RawIdentifier "SubComponent" Nothing callStack
        instanceName = RawIdentifier "sub_comp" Nothing callStack
        pm = IndexedPortMap [ (In, BitVector 32, Identifier inputId)
                            , (Out, BitVector 24, Identifier logicId1)
                            ]
        logicA1 = InstDecl [logicNet1] [] subComponentId instanceName [] pm
        

        constr1 = NetConstr (RawIdentifier "cons1" Nothing callStack) [1]
        constr2 = NetConstr (RawIdentifier "cons2" Nothing callStack) [0]
        fields = [Signed 10, Signed 14]
        ty     = CartesianType (RawIdentifier "MyType" Nothing callStack) [constr1, constr2] fields
        logicA2 = Assignment logicNet2
                    $ DataCon (Cartesian ty) 1
                        [ Literal (Just (Signed 10, 10)) $ NumLit 5 ]

        resId  = RawIdentifier "res" Nothing callStack
        resNet = NetDeclaration (Just "result") resId (BitVector 24) Nothing
        resA = CondAssignment resNet (Identifier inputId) (BitVector 32)
                    [ (Just $ NumLit 123, Identifier logicId1)
                    , (Just $ NumLit 456, Identifier logicId2)
                    , (Nothing, Literal Nothing $ NumLit 45)
                    ]

        cName = RawIdentifier "component" Nothing callStack
        component = AcnComponent cName [inputNet] [logicA1, logicA2] [resA]

        doc = flip evalState (VerilogState True) $ acnToVerilogComponent component

    putStrLn $ show doc
