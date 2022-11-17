
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Netlist.AcnSyntax
-- Description  : ACN Language Abstract Syntax
-- Copyright    : (c) Simon Lovell Bart, 2022
-- License      : BSD2
-- Maintainer   : xandgate@gmail.com
-- Stability    : experimental
-- 
-- = Assignment-Creates-Net (ACN) Language Abstract Syntax
--
-- This module defines the compiler's internal abstract syntax for
-- representing ACN terms. The terms can be divided into three
-- categories, where a term in each category is built out of terms
-- from the one immediately below:
--
--  (1) Top-level components;
--
--  (2) Judgement-level assignment declarations;
--
--  (3) Value-level continuous expressions.
--
-- By declaring and assigning nets in a single judgement, the
-- compiler can automatically determine the properties of every
-- net (chiefly usage annotations).
-- 
module Netlist.AcnSyntax
    ( -- * Netlist Syntax
      -- ** Declarations
      AcnComponent (..)
    , AcnDeclaration (..)
    , AcnBindings
    , SortedDecl (..)
    , sortedDeclToDecl
    , AcnAlternative
    , CommentOrDirective (..)
    , PortMap (..)
    , PortDirection (..)

      -- ** Expressions
    , AcnExpression (..)
    , Literal (..)
    , VerilogBit (..)

      -- ** Net Declarators
    , NetDeclarator (..) 

      -- * Black Boxes
    , AcnBlackBox (..)
    , BlackBoxContext (..)
    , BlackBoxArg

      -- * Netlist Types
    , NetTyCon (..)

      -- ** Representable Netlist Types
    , NetType (..)
    , Attr' (..)
    , netTypeSize

      -- ** Cartesian Types
    , CartesianType (..)
    , NetConstr (..)
    , NetField (..)
    , cartesianSize
    , constructorSize

      -- ** Clock Domains
    , Domain (..)
    , DomainName
    , ActiveEdge (..)
    , ResetKind (..)
    , ResetPolarity (..)
    , InitBehaviour (..)  
    )
  where

import              Netlist.AcnIds
import              Netlist.AcnPrimitives

import              Control.DeepSeq
import              Data.Bool
import              Data.Eq
import              Data.Map (Map (..))
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


-- |
-- ACN top-level component.
--
data AcnComponent
    = AcnComponent
        { componentName :: !AcnId           -- ^ Name of the component.
        , inputs        :: [NetDeclarator]  -- ^ Input ports.
        , logic         :: [AcnDeclaration] -- ^ Internal logic.
        , outputs       :: [AcnDeclaration] -- ^ Output ports\/logic.
        }
    deriving (Show, Generic, NFData)

-- |
-- Judgement-level declarations which create and drive nets.
--
data AcnDeclaration
    -- |
    -- Prototypical assignment: creates a net driven by an expression.
    = Assignment
        !NetDeclarator          -- ^ Created result net.
        !AcnExpression          -- ^ Expression to assign.

    -- |
    -- Conditional assignment. Creates a net driven by one of many
    -- possible alternate expressions.
    | CondAssignment
        !NetDeclarator          -- ^ Created result net.
        !AcnExpression          -- ^ Expression to scrutinize.
        !NetType                -- ^ Scrutinee type.
        [AcnAlternative]        -- ^ Alternatives to choose from.

    -- |
    -- Subcomponent instantiation. Creates an arbitrary number of nets,
    -- each driven by one of the outputs of the instantiated component.
    | InstDecl
        [NetDeclarator]         -- ^ Created result nets.
        [Attr']                 -- ^ Instance attributes.
        !AcnId                  -- ^ Component name.
        !AcnId                  -- ^ Instance name.
        [()]                    -- ^ Compile-time parameters.
        PortMap                 -- ^ I\/O port configuration.

    -- |
    -- Black box instantiation. Creates an arbitrary number of nets,
    -- each driven by one of the outputs of some magical primitive.
    | BlackBoxDecl
        !AcnBlackBox            -- ^ Primitive to defer.
        BlackBoxContext         -- ^ Instantiation context.

    -- |
    -- Annotated declaration.
    | TickDecl
        !CommentOrDirective     -- ^ Annotation.
        AcnDeclaration          -- ^ Declaration to be annotated.

    -- |
    -- Declaration wrapped in a preprocessor condition.
    | ConditionalDecl
        !Text                   -- ^ Condition text.
        [AcnDeclaration]        -- ^ Body to add on condition.
    deriving Show

instance NFData AcnDeclaration where
    rnf x = x `seq` ()

-- |
-- An ACN net declarator contains the name and type information of a net.
-- Usage annotations should be decided by examining the declarations that
-- create the nets.
--
data NetDeclarator
    = NetDeclarator
        { netComment    :: !(Maybe Text)        -- ^ Optional comment.
        , netName       :: !AcnId               -- ^ Name of the net.
        , netType       :: !NetType             -- ^ Net's representable type.
        , initVal       :: Maybe AcnExpression  -- ^ Optional initial value.
        }
    deriving (Show, Generic, NFData)

-- |
-- Map of ACN identifiers to the declarations that create them.
--
-- N.B. that it's possible, but rare, for multiple IDs to map to the same
-- declaration (e.g. in the case of instance declarations).
--
type AcnBindings = Map AcnId SortedDecl

-- |
-- Region-annotated declarations and declarators. Helpful for optimizing,
-- as some optimizations are only applicable to certain regions.
--
data SortedDecl
    = Input  NetDeclarator
    | Logic  AcnDeclaration
    | Output AcnDeclaration

-- |
-- Get an ACN declaration from a region-sorted declaration/declarator.
--
sortedDeclToDecl :: SortedDecl -> Maybe AcnDeclaration
sortedDeclToDecl = \case
    Input _  -> Nothing
    Logic d  -> Just d
    Output d -> Just d


type AcnAlternative = (Maybe Literal, AcnExpression)

data CommentOrDirective
    = Comment   Text
    | Directive Text
    deriving Show
    
-- |
-- Map expressions to input or output nets of an ACN component.
--
data PortMap
    = IndexedPortMap
        [ (PortDirection, NetType, AcnExpression) ]
    -- ^ Association in-order: the @n@-th port mapping corresponds with the
    -- @n@-th input of the component.
    | NamedPortMap
        [ (AcnId, PortDirection, NetType, AcnExpression) ]
    -- ^ Association by name: port mapping @(id, _, ty, _)@ corresponds to
    -- net @NetDeclaration _ id ty _@ in the component.
    deriving Show

data PortDirection = In | Out
    deriving (Show, Generic, NFData)


-- |
-- Typed, continuous, value-level terms.
--
data AcnExpression
    -- |
    -- Fixed or dynamic-sized, typed literals.
    = Literal
        !(Maybe NetType)        -- ^ Literal size and type.
        !Literal                -- ^ Literal contents.
    
    -- |
    -- Variable reference.
    | Identifier !AcnId
    
    -- |
    -- Construct a datatype from a single fixed constructor index.
    | DataCon
        !NetType                -- ^ Type to be constructed.
        !Int                    -- ^ Index of constructor to use.
        [AcnExpression]         -- ^ Constructor arguments.
    
    -- |
    -- Construct a cartesian datatype, with the constructor selected
    -- dynamically by an expression.
    | SuperDataCon
        !CartesianType          -- ^ Type to be constructed.
        !AcnExpression          -- ^ Constructor encoding.
        [Maybe AcnExpression]   -- ^ All fields for this type.
    
    -- |
    -- Project a field of a Cartesian datatype (primitive types
    -- don't have well-defined projections in ACN).
    | Projection
        !AcnExpression          -- ^ Source expression.
        !CartesianType          -- ^ Type of source expression.
        !Int                    -- ^ Constructor to project from.
        !Int                    -- ^ Field to project.
    
    -- |
    -- Slice raw bit representation of a source expression.
    | Slice
        !AcnExpression          -- ^ Source expression.
        !Int                    -- ^ High bit index of range.
        !Int                    -- ^ Low bit index of range.
    
    -- |
    --
    | BlackBoxE
        !AcnBlackBox            -- ^ Primitive to defer.
        BlackBoxContext         -- ^ Calling context.
        !Bool                   -- ^ Should enclose in parentheses?
    deriving Show

instance NFData AcnExpression where
    rnf x = x `seq` ()

-- |
-- Expression-level constants and literals.
--
data Literal
    = NumLit    !Integer            -- ^ Number literal
    | BoolLit   !Bool               -- ^ Boolean literal
    | BitLit    !VerilogBit         -- ^ Bit literal
    | BitVecLit !Integer !Integer   -- ^ BitVector literal
    | VecLit    [Literal]           -- ^ Vector literal
    | BlobLit   !String !String     -- ^ Blob literal
    | StringLit !String             -- ^ String literal
    deriving (Eq, Show)

-- |
-- IEEE 1364 four-valued logic literals.
--
data VerilogBit
    = H -- ^ High
    | L -- ^ Low
    | U -- ^ Undefined
    | Z -- ^ High-impedance
    deriving (Eq, Show, Typeable, Lift)
    

-- |
-- The contents of a primitive black box in an ACN declaration.
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
        { boxTargets    :: [NetDeclarator]
        -- ^ Result declarations.
        , boxInputs     :: [BlackBoxArg]
        -- ^ Black box arguments.
        }
    deriving Show

-- |
-- Hardware black boxes may accept expressions or type-level arguments.
--
type BlackBoxArg = Either (AcnExpression, NetType) NetTyCon


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
    
-- |
-- Types with known representations in hardware.
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
netTypeSize :: NetType -> Int
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

    
-- |
-- Programmer-defined algebraic data types. Sum-of-product types get
-- flattened to product types with a switch:
--
-- @
-- -- ORIGINAL TYPE
-- --
-- data Example = Example1 { w1 :: Bit }
--              | Example2 { w2 :: Bit }
--  
-- example :: Bool -> Example
-- example p = if p then Example2 b else Example1 a
--
--
-- -- FLATTENED EQUIVALENT
-- --
-- data Example' =
--     Example' { switch :: ExampleSwitch
--              , w1     :: Bit
--              , w2     :: Bit
--              }
--
-- data ExampleSwitch = Example1' | Example2'
--
-- example' :: Bool -> Example'
-- example' p = if p then Example' Example2' a b
--                   else Example' Example1' a b
-- @
--
-- The flattening is to ensure that we generate extra wires rather than
-- multiplexing logic. The preference for more wires is advantageous
-- over multiplexers because wires are no-cost in HDL synthesis. The
-- above definition of @example\'@ generates the Verilog:
--
-- @
-- wire [2:0] example;
--
-- assign example = {p, a, b};
-- @
--
-- For contrast, the original implementation of @example@ generates the
-- multiplexing logic:
--
-- @
-- reg [1:0] example;
--
-- always @(*) begin
--   case (p)
--     1'b0: example = {1'b0, a};
--     1'b1: example = {1'b1, b};
--   endcase
-- end
-- @
--
data CartesianType
    = CartesianType
        { typeName      :: !AcnId
        , constructors  :: [NetConstr]
        , fields        :: [NetField]
        }
    deriving (Show, Generic, NFData)

data NetConstr
    = NetConstr
        { consName      :: !AcnId
        , fieldIndices  :: [Int]
        }
    deriving (Show, Generic, NFData)
        
data NetField
    = NetField
        { fieldStart    :: Int
        , fieldEnd      :: Int
        , fieldType     :: NetType
        }
    deriving (Show, Generic, NFData)
        
-- |
-- Compute size needed to represent a cartesian type. The final
-- size should be the sum of the number of bits for the constructor
-- with the number of bits needed for all fields.
--
cartesianSize :: CartesianType -> Int
cartesianSize cty = constructorSize cty + fieldsSize where
    fieldsSize = sum . map (netTypeSize . fieldType) $ fields cty

-- |
-- Compute the size of the constructor for a Cartesian type.
--
constructorSize :: CartesianType -> Int
constructorSize =
    fromMaybe 0 . clogBase 2 . toInteger . length . constructors


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

    
