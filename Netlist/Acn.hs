
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE MagicHash #-}

-- |
-- = Assignment-Creates-Net (ACN) Hardware Description Language
--
-- ACN is a single static assignment language for algorithmically
-- representing CMOS-style digital logic. Its main application is as an
-- intermediate stage in Haskell-to-HDL compilers: it converts readily
-- to both graph form for Haskell interoperability, and to Verilog/VHDL
-- for FPGA programming.
--
-- The core idea of the language is that each net should have exactly
-- one driver. This idea corresponds well with Haskell, where names are
-- immutably bound to expressions. And conveniently for us, it also
-- corresponds to the notion in CMOS logic that a wire should only be
-- powered and grounded through exactly one CMOS cell. Driving a wire
-- by more than one CMOS cell should be considered an error, as it will
-- result in short circuits if one cell grounds the wire while another
-- cell powers it.
--
-- = Visualizing an Example
--
-- To properly understand ACN, let's return to Haskell-land for a moment
-- and consider a familiar closure:
--
-- @
-- example = \\a b -> let c = a + b in c
-- @
--
-- We can decompose this closure into three parts. First, it receives
-- data from the rest of the program through the input binders. Second,
-- it computes its own result. Finally, it transmits that result through
-- its output. The decomposition is comparable to a circuit, with input
-- wires, internal logic, and CMOS-controlled outputs. In fact, this is
-- exactly the shape of an ACN declaration:
--
-- @
-- 'InstDecl' [cNet] [] addId exampleId []
--     $ 'IndexedPortMap'
--         [ ('In' , 'Signed' 32, aId)
--         , ('In' , 'Signed' 32, bId)
--         , ('Out', 'Signed' 32, cId)
--         ]
-- @
--
-- We have a component instance in place of a let-binding, which takes
-- inputs from other parts of the circuit, computes a result, and then
-- transmits the result on the output net. Both forms describe a node in
-- a graph, with edges pointing to the origins of the inputs.
--
module Netlist.Acn where

import              Control.DeepSeq
import              Data.Bool
import              Data.Eq
import              Data.IntMap (IntMap (..))
import qualified    Data.IntMap as IntMap
import qualified    Data.Kind as Kind
import              Data.List
import              Data.Maybe
import              Data.Text (Text (..))
import qualified    Data.Text as Text
import              Data.Typeable (Typeable)
import              GHC.Generics
import              GHC.Int
import              GHC.Stack
import              Text.Show (Show (..))

import              Language.Haskell.TH.Syntax (Lift)

-- TO REMOVE
import              GHC.Base (Int (..), isTrue#, (+#), (==#))
import              GHC.Integer.Logarithms (integerLogBase#)


-- |
-- ACN top-level component.
--
-- @
-- 'AcnComponent' name
--     [inNet]
--     ['Assignment' logic expr1]
--     ['Assignment' out expr2]
-- @
--
-- corresponds to the STG closure
--
-- > name = \inNet ->
-- >     let { logic = expr1
-- >         ; out   = expr2
-- >         } in out
--
data AcnComponent
    = AcnComponent
        { componentName :: !Identifier      -- ^ Name of the component.
        , inputs        :: [NetDeclaration] -- ^ Input ports.
        , logic         :: [AcnDeclaration] -- ^ Internal logic.
        , outputs       :: [AcnDeclaration] -- ^ Output ports\/logic.
        }
    deriving (Show, Generic, NFData)

-- |
-- ACN language declarations.
--
-- The /Assignment/ part of /Assignment/-Creates-Net refers to any
-- kind of net value judgment. So, an ACN declaration has two basic
-- responsibilities:
--
--  * describe the structure of some hardware process; and
--  * specify all the nets driven by that process.
--
-- See Note [ACN to HDL Net Usage] for more information on how on-site
-- net declarations translate into HDL net declarations.
--
-- See Note [Multiplexing Assignments] for details regarding
-- conditional assignments and three-valued logic.
--
data AcnDeclaration
    = Assignment
        !NetDeclaration             -- ^ Result net.
        !Expr                       -- ^ Expression to assign.
    | CondAssignment
        !NetDeclaration             -- ^ Result net.
        !Expr                       -- ^ Scrutinee.
        !NetType                    -- ^ Scrutinee type.
        [(Maybe Literal, Expr)]     -- ^ Alternatives.
    | InstDecl
        [NetDeclaration]            -- ^ Result nets.
        [Attr']                     -- ^ Instance attributes.
        !Identifier                 -- ^ Component name.
        !Identifier                 -- ^ Instance name.
        [()]                        -- ^ Compile-time parameters.
        PortMap                     -- ^ I\/O port configuration.
    | BlackBoxDecl
        !ClosureFun                 -- ^ Primitive to defer.
        ClosureContext              -- ^ Calling context.
    | TickDecl
        !CommentOrDirective
        AcnDeclaration              -- ^ Declaration to be annotated.
    | ConditionalDecl
        !Text                       -- ^ Condition text.
        [AcnDeclaration]            -- ^ Body to add on condition.
    deriving Show

instance NFData AcnDeclaration where
    rnf x = x `seq` ()

-- Note [Multiplexing Assignments]
--
-- Because nets can only be assigned to once, there are some tristate
-- logic tricks we can't do. For example, no ACN declaration can result
-- in
--
-- @
-- wire x;
-- assign x = p1 ? v1 : 1'bz;
-- assign x = p2 ? v2 : 1'bz;
-- @
--
-- Fortunately, even though we can't do this tristate logic, neither
-- can the FPGAs we're targetting. FPGAs use CMOS logic, which is
-- only two-valued; we don't lose generality by forcing choices to use
-- multiplexers.

data CommentOrDirective
    = Comment   Text
    | Directive Text
    deriving Show

-- |
-- A net declarartion.
--
-- The net declaration @NetDeclaration comment name ty (Just init)@
-- generates either
--
-- > wire <ty> <name> = <init>; // <comment>
--
-- for combinational assignments, or
--
-- > reg <ty> <name> = <init>; // <comment>
--
-- for procedural assignments.
--
-- See Note [ACN to HDL Net Usage] for more information on generating
-- appropriate HDL net declarations from ACN declarations.
--
data NetDeclaration
    = NetDeclaration
        { netComment    :: !(Maybe Text)
        , netName       :: !Identifier
        , netType       :: !NetType
        , initVal       :: Maybe Expr
        }
    deriving (Show, Generic, NFData)

-- Note [ACN to HDL Net Usage]
--
-- Unlike Verilog and VHDL, ACN doesn't care about the usage of its nets.
-- Since a net is declared on-site, there is no need to track whether a
-- net is continuous or procedural in its assignments.
--
-- To target Verilog and VHDL, the codegen net declaration pass examines
-- the assignment sites. If we have the ACN assignment site
--
-- @
-- 'CondAssignment' res scrut scrutTy alts
-- @
--
-- then we can determine that we should declare @res@ in Verilog as
--
-- > reg <res>;
--
-- In VHDL, the same conditional assignment declaration is rendered
-- continuously, so the VHDL net declaration pass determines to declare @res@
--
-- > signal <res>;
--
-- What matters is that all the information about the assignment site needed
-- to determine net usage is also present in the structure of the site's
-- declaration.

-- | Net names and references.
--
data Identifier
    = RawIdentifier
        { givenName         :: !Text
        , rawParsed         :: Maybe Identifier
        , provenance        :: !CallStack
        }
    | UniqueIdentifier
        { baseName          :: !Text
        , baseNameNoCase    :: !Text
        , exteionsions      :: [Word]
        , idType            :: !IdentifierType
        , hdl               :: !HDL
        , provenance        :: !CallStack
        }
    deriving (Show, Generic, NFData)

data IdentifierType
    = Basic
    | Extended
    deriving (Show, Generic, NFData, Eq)


-- | Map expressions to input or output nets of an ACN component.
--
data PortMap
    = IndexedPortMap
        [ (PortDirection, NetType, Expr) ]
    -- ^ Association in-order: the @n@-th port mapping corresponds with the
    -- @n@-th input of the component.
    | NamedPortMap
        [ (Identifier, PortDirection, NetType, Expr) ]
    -- ^ Association by name: port mapping @(id, _, ty, _)@ corresponds to
    -- net @NetDeclaration _ id ty _@ in the component.
    deriving Show

data PortDirection = In | Out
    deriving (Show, Generic, NFData)

-- |
-- Context to instantiate a deferred closure into.
-- 
-- Some closure instantiations may be deferred, notably in the case of
-- primitives. When deferring instantiation, we must provide all the
-- necessary context to properly instantiate the closure later.
--
-- Necessary context for ACN instantiation includes arguments to the
-- closure, and result declarations.
--
data ClosureContext
    = ClosureContext
        { closTargets       :: [NetDeclaration]
        -- ^ Result declarations.
        , closInputs        :: [ClosureArg]
        -- ^ Closure arguments.
        , closFunctions     :: IntMap [(ClosureFun, ClosureContext)]
        -- ^ Deferred function inputs.
        }
    deriving Show

-- | Closures are usually blackboxes, but they /may/ be deferred functions.
--
data ClosureFun
    = PrimClosure
        { closName          :: Text
        , closLibraries     :: [BlackBoxTemplate]
        , closImports       :: [BlackBoxTemplate]
        , closQsys          :: [((Text, Text), BlackBox)]
        , closTokens        :: BlackBox
        }
    | FunClosure
        { closName          :: Text
        , closIdentifier    :: Identifier
        , closDeclarations  :: [AcnDeclaration]
        }
    deriving Show

-- | Hardware closures may accept expressions or type-level arguments.
--
type ClosureArg = Either (Expr, NetType) NetTyCon

        
-- |
-- Net type-like constructors.
--
-- The internal netlist language has several /type-like/ things:
--
--  * actual hardware types;
--  * proxied tycons (tycons artificially made non-representable);
--  * domains;
--  * constant parameters.
--
-- Only 'HWTyCon's may be used in codegen, but the other tycons can be passed
-- to the blackbox instantiator as metadata to guide primitive generation.
--
data NetTyCon
    = HWTyCon
        { netTyConName  :: !Text
        -- ^ The generated name of this type. To be emitted by the VHDL
        -- backend.
        , hwTypeRhs     :: !NetType
        -- ^ The representable netlist type itself.
        }
    | Proxy         (Maybe NetTyCon)
    | KnownDomain   !Domain
    | Integer       !Int
    | String        !String
    deriving (Show, Generic, NFData)
    
-- | Types with known representations in hardware.
--
data NetType
    = Annotated [Attr'] !NetType
    -- N.B. that non-HW 'NetTyCon's are only created in the builtin type
    -- translator. Since the builtin type translator will never produce
    -- an annotated 'NetTyCon', we can safely move annotated types into the
    -- realm of 'NetType'.
    | Clock     !DomainName
    | Reset     !DomainName
    | Enable    !DomainName
    | Boolean
    -- ^ Strictly 2-valued Booleans.
    | Bit
    -- ^ Potentially IEEE-1164 9-valued logic types.
    | Index     !Integer
    -- ^ Integer value with a fixed upper bound @n@ of @clogBase 2 n@ bits.
    | BitVector !Int
    -- ^ Wire bundle of @n@ bits.
    | Signed    !Int
    | Unsigned  !Int
    | Cartesian !CartesianType
    -- ^ User-defined types. See 'CartesianType'.
    | MemBlob   !Int !Int
    -- ^ "Vector-of-bitvector" types. The first parameter is the number
    -- of elements. The second parameter is the size in bits of each element.
    | Vector    !Int !NetType
    | RTree     !Int !NetType
    | BiDirectional !PortDirection !NetType
    | File
    deriving (Show, Generic, NFData)

data Attr'
    = BoolAttr'     String Bool
    | IntAttr'      String Int
    | StringAttr'   String String
    | Attr'         String
    deriving (Eq, Show, Generic, NFData)

attrName :: Attr' -> String
attrName = \case
    BoolAttr' nm _      -> nm
    IntAttr' nm _       -> nm
    StringAttr' nm _    -> nm
    Attr' nm            -> nm
    
-- |
-- Determine the size of a representable net type.
--
-- N.B. any zero-width types should have been filtered by Core to netlist
-- type conversion.
--
netTypeSize :: NetType -> Size
netTypeSize = \case
    Annotated _ ty      -> netTypeSize ty
    Clock _             -> 1
    Reset _             -> 1
    Enable _            -> 1
    Boolean             -> 1
    Bit                 -> 1
    Index 0             -> 0
    Index 1             -> 1
    Index n             -> fromMaybe 0 . clogBase 2 $ n
    BitVector n         -> n
    Signed n            -> n
    Unsigned n          -> n
    Cartesian cty       -> cartesianSize cty
    Vector n ty         -> n * netTypeSize ty
    RTree d ty          -> (2 ^ d) * netTypeSize ty
    BiDirectional In ty -> netTypeSize ty
    BiDirectional Out _ -> 0
    File                -> 32

type Size = Int
    
-- |
-- Programmer-defined algebraic data types.
--
-- N.B. that the 'mkADT' function should prevent any tycons other than
-- 'HWTyCon' from becoming fields of a cartesian type.
--
-- Unlike base Clash, we don't tie fields to constructors. We prefer
-- to optimistically flatten SP types to generate fewer multiplexers.
--
-- Given the source
--
-- @
-- data Example = Example1 { w1 :: Bit }
--              | Example2 { w2 :: Bit }
--  
-- example :: Bool -> Example
-- example p = if p then Example2 b else Example1 a
-- @
--
-- the base Clash netlist generator produces the HDL result
--
-- @
-- wire [1:0] exampleWire;
--  
-- assign exampleWire[1] = p;
-- assign exampleWire[0] = p ? b : a;
-- @
--
-- From the same source, the new netlist generator will deduce the
-- intermediate result
--
-- @
-- data Example =
-- Example { switch :: ExampleSwitch
--         , w1     :: Bit
--         , w2     :: Bit
--         }
--  
-- data ExampleSwitch = Example1 | Example2
--  
-- example :: Bool -> Example
-- example p = if p then Example Example2 a b
--                  else Example Example1 a b
-- @
--
-- The new intermediate result's HDL output is
--
-- @
-- wire [2:0] exampleWire;
--  
-- assign exampleWire[2] = p;
-- assign exampleWire[1:0] = {a, b};
-- @
--
-- Optimistic flattening is advantageous over multiplexers because
-- wires are no-cost in HDL synthesis.
--
data CartesianType
    = CartesianType
        { typeName      :: !Identifier
        , constructors  :: [NetConstr]
        , fields        :: [NetType]
        }
    deriving (Show, Generic, NFData)

data NetConstr
    = NetConstr
        { consName      :: !Identifier
        , fieldIndices  :: [Int]
        }
    deriving (Show, Generic, NFData)
        
-- |
-- Compute size needed to represent a cartesian type.
--
-- The final size should be the sum of the number of bits for the
-- constructor with the number of bits needed for all fields.
--
cartesianSize :: CartesianType -> Size
cartesianSize cty = constrSize + fieldsSize where
    constrSize = fromMaybe 0 . clogBase 2 $ numConstrs
    numConstrs = toInteger . length $ constructors cty

    fieldsSize = sum . map netTypeSize $ fields cty


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


-- | Well-typed representable expressions.
--
data Expr
    = Literal
        !(Maybe (NetType, Size))    -- ^ Literal size and type.
        !Literal                    -- ^ Literal contents.
    | Identifier    !Identifier     -- ^ Reference to a net.
    | DataCon
        !NetType                    -- ^ Type to be constructed.
        !Int                        -- ^ Index of constructor to use.
        [Expr]                      -- ^ Constructor arguments.
    | SuperDataCon
        !CartesianType              -- ^ Type to be constructed.
        !Expr                       -- ^ Expression determining constructor.
        [Maybe Expr]                -- ^ All fields for this type.
    | Projection
        !Expr                       -- ^ Source expression.
        !NetType                    -- ^ Type of source expression.
        !Int                        -- ^ Constructor to project from.
        !Int                        -- ^ Field to project.
    | Slice
        !Expr                       -- ^ Source expression.
        !Int                        -- ^ High bit index of range.
        !Int                        -- ^ Low bit index of range.
    | BlackBoxE
        !ClosureFun                 -- ^ Primitive to defer.
        ClosureContext              -- ^ Calling context.
        !Bool                       -- ^ Maybe enclose in parentheses.
    deriving Show

instance NFData Expr where
    rnf x = x `seq` ()

data Literal
    = NumLit    !Integer            -- ^ Number literal
    | BoolLit   !Bool               -- ^ Boolean literal
    | BitLit    !Bit                -- ^ Bit literal
    | BitVecLit !Integer !Integer   -- ^ BitVector literal
    | VecLit    [Literal]           -- ^ Vector literal
    | BlobLit   !String !String     -- ^ Blob literal
    | StringLit !String             -- ^ String literal
    deriving (Eq, Show)

-- | IEEE 1364 four-valued logic literals.
--
data Bit
    = H -- ^ High
    | L -- ^ Low
    | U -- ^ Undefined
    | Z -- ^ High-impedance
    deriving (Eq, Show, Typeable, Lift)


-- NEED TO IMPLEMENT


type BlackBoxTemplate = [Element]

data Element = Element
    deriving Show

data BlackBox = BlackBox
    deriving Show

data HDL = Verilog | VHDL
    deriving (Show, Generic, NFData)



-- | \x y -> ceiling (logBase x y), x > 1 && y > 0
clogBase :: Integer -> Integer -> Maybe Int
clogBase x y | x > 1 && y > 0 =
  case y of
    1 -> Just 0
    _ -> let z1 = integerLogBase# x y
             z2 = integerLogBase# x (y-1)
         in  if isTrue# (z1 ==# z2)
                then Just (I# (z1 +# 1#))
                else Just (I# z1)
clogBase _ _ = Nothing

