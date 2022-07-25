
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Acn
-- Description  : ACN Hardware Description Language
-- Maintainer   : xandgate@gmail.com
-- Stability    : experimental
-- Language     : Haskell2010
-- 
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
-- = Worked Example
--
-- To properly understand ACN, let's return to Haskell-land for a moment
-- and consider a simple closure:
--
-- @
-- example = \\a b -> let c = a + b in c
-- @
--
-- How would the same closure be represented in ACN? First of all, every
-- closure in normal form corresponds to an ACN component. Components
-- take the same approach for inputs as GHC's STG language does: each
-- input is a special binder that names a slot where we may insert some
-- argument at the closure's call site:
--
-- @
-- let aNet = 'NetDeclaration' Nothing aId ('Signed' 32) Nothing
--     bNet = 'NetDeclaration' Nothing bId ('Signed' 32) Nothing
-- @
--
-- Next, we must represent addition. There are a few ways such a
-- function may be compiled: it could have a primitive or black box form
-- substituted in its place by the HDL codegen. But for the sake of
-- demonstration, we will assume that it's also a closure represented by
-- a component whose top-level binder is @addId@. To use the component
-- in this circuit, we must instantiate it with:
--
-- @
-- let decl = 'InstDecl' [cNet] [] addId addInstanceId []
--                $ 'IndexedPortMap'
--                    [ ('In' , 'Signed' 32, aId)
--                    , ('In' , 'Signed' 32, bId)
--                    , ('Out', 'Signed' 32, cId)
--                    ]
-- @
--
-- Finally, we can populate the contents of the component. Note that the
-- extra list corresponds to any internal logic that we don't wish to
-- output from the component, like binders in a let-expression that don't
-- appear in the body. The fully specified component is:
--
-- @
-- 'AcnComponent' exampleId [aNet, bNet] [] [decl]
-- @
--
-- On the other side, we can generate a Verilog description of this logic.
-- ACN components correspond directly to Verilog modules, so we start
-- immediately with that:
--
-- @
-- module example
-- @
--
-- Where things get interesting is the generation of net declarations.
-- Verilog expects nets to be declared in advance, and also expects them to
-- be annotated with how they're meant to be assigned. ACN doesn't
-- distinguish between continuous and latching assignments the way Verilog
-- does, so there's a trick to this step. What ACN does have is a lot of
-- knowledge about how nets actually get used: since an ACN declaration is
-- responsible for any nets that it assigns to, we can always determine
-- their behaviour by examining the declarations that create them.
-- Specifically, @cNet@ occurs in an instance declaration. Instances will
-- always drive their nets continuously, so we know that it should be
-- declared as:
--
-- @
--     ( input [31:0] a
--     , input [31:0] b
--     , output wire [31:0] c
--     );
-- @
--
-- Normally we'd then use the same trick to declare internal logic nets,
-- and then generate the internal logic itself. This component has no
-- internal logic, so we can skip those steps. After that, we generate
-- the code that assigns to outputs; in this case our instantiation:
--
-- @
--   add addInstance
--     (a, b, c);
--
-- endmodule
-- @
-- 
-- = Some Peculiarities and Black Boxes
-- 
-- The ACN approach also applies some restrictions to what we may do.
-- For example, some circuits use tristate logic rather than multiplexers
-- for decisions. In Verilog, we might have something like:
-- 
-- @
-- wire x;
-- assign x = p1 ? v1 : 1'bz;
-- assign x = p2 ? v2 : 1'bz;
-- @
-- 
-- Naively, we might try to implement this with two @'Assignment'@
-- declarations. But we run into a problem right away: since each
-- declaration must create its own result net, there's no way for both
-- assignments to assign to the same net, as in the Verilog code. The only
-- way around the restriction in this case is a particular conditional
-- assignment that does all the possible tri-state assignments within the
-- same block so that they may all use the same result net.
-- 
-- These edge cases are where @'AcnBlackBox'@ comes in handy. If ACN
-- doesn't represent the exact output we want, we can write a primitive
-- in the target HDL. The primitive is treated as a black box, and can
-- be substituted by codegen with the appropriate context. As long as we
-- still know the declarations of nets exposed by the black box, they
-- can still be used in a circuit as we would any other declaration.
-- 
-- Going back to the example circuit, we could imagine addition not as a
-- component to be instantiated, but as a primitive. That would change
-- the ACN declaration to look something like:
-- 
-- @
-- 'Assignment' cNet
--     $ 'BlackBoxE'
--         (addBB :: 'AcnBlackBox')
--         ('BlackBoxContext' []
--             [ Left ('Identifier' aId, 'Signed' 32)
--             , Left ('Identifier' bId, 'Signed' 32)
--             ])
-- @
-- 
-- From an appropriately written primitive for @addBB@, the resultant
-- Verilog could look like:
-- 
-- @
-- assign c = a + b;
-- @
-- 
-- Of course, the reverse process of black box insertion is complicated.
-- It puts ACN as a language in an interesting position not far off from
-- Haskell itself. Every C program has an equivalent in Haskell. But it's
-- not always possible to directly compile those C programs to their
-- Haskell equivalents. Similarly, every Verilog or VHDL description can
-- be written in ACN. But while writing a Haskell-to-ACN-to-Verilog
-- compiler is tractable, writing a Verilog-to-ACN-to-Haskell compiler
-- is likely very hard. So while ACN is useful algorithmically as an HDL,
-- using it in one direction might require some extra steps (perhaps a
-- Verilog to LLHD to ACN approach would work for this purpose).
-- 
module Netlist.Acn
    ( -- * ACN Declarations
      AcnComponent (..)
    , AcnDeclaration (..)
    , CommentOrDirective (..)
      -- ** Net Declarations
    , NetDeclaration (..)
    , Identifier (..)
    , IdentifierType (..)
      -- ** Port Maps
    , PortMap (..)
    , PortDirection (..)
      -- ** ACN Black Boxes
    , AcnBlackBox (..)
    , BlackBoxContext (..)
    , BlackBoxArg
      -- * Netlist Types
    , NetTyCon (..)
      -- ** Representable Netlist Types
    , NetType (..)
    , Attr' (..)
    , netTypeSize
    , Size
    , CartesianType (..)
    , NetConstr (..)
    , cartesianSize
      -- ** Clock Domains
    , Domain (..)
    , DomainName
    , ActiveEdge (..)
    , ResetKind (..)
    , ResetPolarity (..)
    , InitBehaviour (..)
      -- * Representable Expressions
    , Expr (..)
    , Literal (..)
    , Bit (..)
    )
  where

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

import              CeilingLog


-- * ACN Declarations
-- 
-- $acnDeclarations

-- |
-- ACN top-level component.
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
-- ACN declarations have two basic responsibilities:
--
--  (1) describe the structure of some hardware process; and
--  (2) create all the nets assigned to by that process.
--
-- Satisfying responsibility (2) has some interesting ramifications for
-- the representation of logic designs. As we've seen, this allows ACN to
-- know a lot about the use of nets. We can easily determine target HDL
-- annotations for net codegen. We may even be able to make more nuanced
-- decisions about the annotations: assessing the advantages of many
-- different implementations on a problem-specific basis.
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
        !AcnBlackBox                -- ^ Primitive to defer.
        BlackBoxContext             -- ^ Calling context.
    | TickDecl
        !CommentOrDirective
        AcnDeclaration              -- ^ Declaration to be annotated.
    | ConditionalDecl
        !Text                       -- ^ Condition text.
        [AcnDeclaration]            -- ^ Body to add on condition.
    deriving Show

instance NFData AcnDeclaration where
    rnf x = x `seq` ()

data CommentOrDirective
    = Comment   Text
    | Directive Text
    deriving Show

-- |
-- An ACN net declaration contains the name and type information of a net.
-- Usage annotations should be decided by examining the declarations that
-- create the nets.
--
data NetDeclaration
    = NetDeclaration
        { netComment    :: !(Maybe Text)    -- ^ Comment describing the net.
        , netName       :: !Identifier      -- ^ Name of the net.
        , netType       :: !NetType         -- ^ Net's representable type.
        , initVal       :: Maybe Expr       -- ^ Optional initial value.
        }
    deriving (Show, Generic, NFData)

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


-- ** Black Boxes
-- 
-- $acnBlackBoxes

-- |
-- Some circuit parts are deeply HDL-dependent and aren't easily
-- represented by ACN itself. To work around this, ACN has circuit
-- primitives in the target language which that language's codegen
-- can substitute in the appropriate context.
--
data AcnBlackBox
    = PrimBlackBox
        { boxName       :: Text
        , boxLibraries  :: [BlackBoxTemplate]
        , boxImports    :: [BlackBoxTemplate]
        , boxQsys       :: [((Text, Text), BlackBox)]
        , boxTokens     :: BlackBox
        }
    deriving Show

-- |
-- The substitution context needed to properly instantiate a black box
-- into a circuit. The necessary context includes the input nets from
-- the surrounding circuit, as well as the declaration for any result nets
-- we wish to expose.
--
data BlackBoxContext
    = BlackBoxContext
        { boxTargets    :: [NetDeclaration]
        -- ^ Result declarations.
        , boxInputs     :: [BlackBoxArg]
        -- ^ Black box arguments.
        }
    deriving Show

-- | Hardware black boxes may accept expressions or type-level arguments.
--
type BlackBoxArg = Either (Expr, NetType) NetTyCon


-- * Representable Types
--
-- $acnTypes

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
-- Compute size needed to represent a cartesian type. The final
-- size should be the sum of the number of bits for the constructor
-- with the number of bits needed for all fields.
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


-- * Netlist Expressions
--
-- $acnExpressions
    
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
        !AcnBlackBox                -- ^ Primitive to defer.
        BlackBoxContext             -- ^ Calling context.
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


