
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Netlist.Test where

import Netlist.Acn
import Netlist.AcnIds
import Netlist.AcnToVerilog

import Control.Monad.State
import GHC.Stack


main :: IO ()
main = do
    let inputId  = verbatimId# "in"
        inputNet = NetDeclarator (Just "input") inputId (BitVector 32) Nothing

        logicId1  = verbatimId# "logic1"
        logicNet1 = NetDeclarator (Just "logic 1") logicId1 (BitVector 24) Nothing
        logicId2  = verbatimId# "logic2"
        logicNet2 = NetDeclarator (Just "logic 2") logicId2 (Cartesian ty) Nothing

        
        subComponentId = verbatimId# "SubComponent"
        instanceName = verbatimId# "sub_comp"
        pm = IndexedPortMap [ (In, BitVector 32, Identifier inputId)
                            , (Out, BitVector 24, Identifier logicId1)
                            ]
        logicA1 = InstDecl [logicNet1] [] subComponentId instanceName [] pm
        

        constr1 = NetConstr (verbatimId# "cons1") [1]
        constr2 = NetConstr (verbatimId# "cons2") [0]
        constr3 = NetConstr (verbatimId# "cons3") [0, 1]
        fields = [NetField 14 23 (Signed 10), NetField 0 13 (Signed 14)]
        ty     = CartesianType (verbatimId# "MyType")
                    [constr1, constr2] fields
        logicA2 = Assignment logicNet2
                    $ DataCon (Cartesian ty) 1
                        [ Literal (Just $ Signed 10) $ NumLit 5 ]

        resId  = verbatimId# "res"
        resNet = NetDeclarator (Just "result") resId (BitVector 24) Nothing
        resA = CondAssignment resNet (Identifier inputId) (BitVector 32)
                    [ (Just $ NumLit 123, Identifier logicId1)
                    , (Just $ NumLit 456, Slice (Identifier logicId2) 23 14)
                    , (Nothing, Literal Nothing $ NumLit 45)
                    ]

        cName = verbatimId# "component"
        component = AcnComponent cName [inputNet] [logicA1, logicA2] [resA]

        doc = flip evalState (VerilogState True) $ acnToVerilogComponent component

    putStrLn $ show doc
