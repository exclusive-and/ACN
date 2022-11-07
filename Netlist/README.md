
# ACN: A Dataflow Language for Intermediate Representation of HDLs

## TL;DR

An IR language that models CMOS logic in a convenient way for both
high-level functional IR's (Core, TT, etc) and low-level HDL's (Verilog,
SystemVerilog, VHDL).

Example 1 (ACN):

```haskell
let
  -- Inputs
  aNet = NetDeclaration Nothing aId (Signed 32) Nothing
  bNet = NetDeclaration Nothing bId (Signed 32) Nothing

  -- Result
  cNet = NetDeclaration Nothing cId (Signed 32) Nothing

  -- Adder subcomponent instance
  addDecl = InstDecl [cNet] [] adderId adderInstanceId []
                $ IndexedPortMap
                    [ (In , Signed 32, aId)
                    , (In , Signed 32, bId)
                    , (Out, Signed 32, cId)
                    ]

  -- Component declaration
in
  AcnComponent componentId [aNet, bNet] [] [addDecl]
```

Example 1 (Verilog):

```verilog
module example
  ( input  [31:0] a
  , input  [31:0] b
  , output [31:0] c
  );

  adderId adderInstanceId
    ( a
    , b
    , c
    )

endmodule
```

Example 1 (Haskell):

```haskell
example = \a b -> let c = a + b in c
```



