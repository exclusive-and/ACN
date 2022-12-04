
{-# LANGUAGE DeriveAnyClass #-}

-----------------------------------------------------------
-- |
-- Module       : Acn.Syntax
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
-----------------------------------------------------------
-- 
module Acn.Syntax
    ( -- * Netlist Term Syntax
      -- ** Declarations
      Component (..), Assignment (..)
    , Annotation (..), CaseAlt (..)
    , PortMap (..), PortDirection (..)
      -- ** Expressions
    , Expression (..)
    , Literal (..), VerilogBit (..)
      -- ** Net Declarators
    , Declarator (..)

      -- * Black Boxes
    , BlackBox (..)
    , BlackBoxContext (..), BlackBoxArg

      -- * Netlist Types
    , NetTypeLike (..)
      -- ** Representable Netlist Types
    , NetType (..), Attr' (..)
    , netTypeSize
      -- ** Cartesian Types
    , CartesianType (..)
    , NetConstructor (..), NetField (..)
    , cartesianSize, constructorSize
    
      -- * Declaration Namespace
    , AcnBindings, SortedDecl (..)
    , sortedDeclToDecl
    )
  where

import              Acn.CompilerPass
import qualified    Acn.Ids as Acn
import qualified    Acn.Primitives as Acn

import              Control.DeepSeq
import              Data.Map (Map)
import              Data.Maybe
import              Data.Text (Text)
import              Data.Typeable (Typeable)
import              GHC.Generics
import              GHC.Stack

import              Language.Haskell.TH.Syntax (Lift)

import              CeilingLog


-- |
-- ACN top-level component.
--
data Component pass
    = Component
        { componentName :: !Acn.Id              -- ^ Name of the component.
        , inputs        :: [Declarator pass]    -- ^ Input ports.
        , logic         :: [Assignment pass]    -- ^ Internal logic.
        , outputs       :: [Assignment pass]    -- ^ Output ports\/logic.
        }
    deriving (Show, Generic, NFData)

-- |
-- Assignment declarations which create and drive nets.
--
data Assignment pass
    -- |
    -- Prototypical assignment: creates a net driven by an expression.
    = Assignment
        !Declarator pass    -- ^ Created result net.
        !Expression pass    -- ^ Expression to assign.

    -- |
    -- Conditional assignment. Creates a net driven by one of many
    -- possible alternate expressions.
    | CondAssignment
        !Declarator pass    -- ^ Created result net.
        !Expression pass    -- ^ Expression to scrutinize.
        !SynType    pass    -- ^ Scrutinee type.
        [CaseAlt pass]      -- ^ Alternatives to choose from.

    -- |
    -- Subcomponent instantiation. Creates an arbitrary number of nets,
    -- each driven by one of the outputs of the instantiated component.
    | InstDecl
        [Declarator pass]   -- ^ Created result nets.
        [Attr']         -- ^ Instance attributes.
        !Acn.Id         -- ^ Component name.
        !Acn.Id         -- ^ Instance name.
        [()]            -- ^ Compile-time parameters.
        PortMap pass    -- ^ I\/O port configuration.

    -- |
    -- Black box instantiation. Creates an arbitrary number of nets,
    -- each driven by one of the outputs of some magical primitive.
    | BlackBoxDecl
        !BlackBox               -- ^ Primitive to defer.
        BlackBoxContext pass    -- ^ Instantiation context.

    -- |
    -- Annotated declaration(s).
    | AnnotatedDecl
        !Annotation         -- ^ Annotation.
        [Assignment pass]   -- ^ Declaration(s) to be annotated.
    deriving Show

instance NFData (Assignment pass) where
    rnf x = x `seq` ()

-- |
-- Annotations that can be inserted in or around declarations to supplement
-- them in synthesis.
-- 
data Annotation
    = Comment   Text    -- ^ Comment.
    | Directive Text    -- ^ Synthesizer directive.
    | Condition Text    -- ^ Synthesizer preprocessor condition.
    deriving Show
 
-- |
-- An ACN net declarator contains the name and type information of a net.
-- Usage annotations should be decided by examining the declarations that
-- create the nets.
--
data Declarator pass
    = Declarator
        !(Maybe Text)               -- ^ Optional comment.
        !Acn.Id                     -- ^ Name of the net.
        !(SynType pass)             -- ^ Net's representable type.
        (Maybe (Expression pass))   -- ^ Optional initial value.
    deriving (Show, Generic, NFData)

-- |
-- One branch of a conditional assignment declaration.
--
data CaseAlt pass
    -- |
    -- Default branch.
    = Default
        Expression pass -- ^ Value to assign by default.

    -- |
    -- Branch dependent on a condition.
    | Dependent
        Literal pass    -- ^ Condition.
        Expression pass -- ^ Value to assign.
    deriving Show
   
-- |
-- Map expressions to input or output nets of an ACN component.
--
data PortMap pass
    = IndexedPortMap
        [ (PortDirection, SynType pass, Expression pass) ]
    -- ^ Association in-order: the @n@-th port mapping corresponds with the
    -- @n@-th input of the component.
    | NamedPortMap
        [ (Acn.Id, PortDirection, NetType pass, Expression pass) ]
    -- ^ Association by name: port mapping @(id, _, ty, _)@ corresponds to
    -- net @NetDeclaration _ id ty _@ in the component.
    deriving Show

data PortDirection = In | Out
    deriving (Show, Generic, NFData)


-- |
-- Typed, continuous, value-level terms.
--
data Expression pass
    -- |
    -- Fixed or dynamic-sized, typed literals.
    = Literal
        !(Maybe (SynType pass)) -- ^ Literal size and type.
        !Literal                -- ^ Literal contents.
    
    -- |
    -- Variable reference.
    | Identifier !Acn.Id
    
    -- |
    -- Construct a datatype from a single fixed constructor index.
    | DataCon
        !(SynType pass)         -- ^ Type to be constructed.
        !Int                    -- ^ Index of constructor to use.
        [Expression pass]       -- ^ Constructor arguments.
    
    -- |
    -- Construct a cartesian datatype, with the constructor selected
    -- dynamically by an expression.
    | SuperDataCon
        !CartesianType              -- ^ Type to be constructed.
        !Expression pass            -- ^ Constructor encoding.
        [Maybe (Expression pass)]   -- ^ All fields for this type.
    
    -- |
    -- Project a field of a Cartesian datatype (primitive types
    -- don't have well-defined projections in ACN).
    | Projection
        !(Expression pass)  -- ^ Source expression.
        !CartesianType      -- ^ Type of source expression.
        !Int                -- ^ Constructor to project from.
        !Int                -- ^ Field to project.
    
    -- |
    -- Slice raw bit representation of a source expression.
    | Slice
        !(Expression pass)  -- ^ Source expression.
        !Int                -- ^ High bit index of range.
        !Int                -- ^ Low bit index of range.
    
    -- |
    --
    | BlackBoxE
        !BlackBox               -- ^ Primitive to defer.
        (BlackBoxContext pass)  -- ^ Calling context.
        !Bool                   -- ^ Should enclose in parentheses?
    deriving Show

instance NFData Expression where
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
data BlackBox
    = PrimBlackBox
        { boxName       :: Text
        , boxLibraries  :: [Acn.BlackBoxTemplate]
        , boxImports    :: [Acn.BlackBoxTemplate]
        , boxQsys       :: [((Text, Text), Acn.BlackBox)]
        , boxTokens     :: Acn.BlackBox
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
        { boxTargets    :: [Declarator]
        -- ^ Result declarations.
        , boxInputs     :: [BlackBoxArg]
        -- ^ Black box arguments.
        }
    deriving Show

-- |
-- Hardware black boxes may accept expressions or type-level arguments.
--
type BlackBoxArg = Either (Expression, NetType) NetTyCon


-- |
-- Things that can be synthesis-checked.
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
data SynTyThing
    = ARepresentableType    !NetType
    | AProxyThing           (Maybe NetTyThing)
    | AKnownDomain          !Acn.Domain
    | ALiftedExpression     !Expression !NetType
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
    | Clock     !Acn.DomainName
    | Reset     !Acn.DomainName
    | Enable    !Acn.DomainName
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
    MemBlob m n         -> m * n
    Vector n ty         -> n * netTypeSize ty
    RTree d ty          -> (2 ^ d) * netTypeSize ty
    BiDirectional In ty -> netTypeSize ty
    BiDirectional Out _ -> 0
    File                -> 32

-- |
-- Types that must pass synthesis-checking.
--
type family SynType pass

type instance SynType AcnConv = NetTypeLike
type instance SynType AcnSynC = NetType
type instance SynType AcnNorm = NetType
type instance SynType AcnDone = NetType

-- |
-- Type-level elements which may not pass synthesis-checking, but must
-- obey some type laws (e.g. no domains, no proxies) to be compatible
-- with HDL generation phases.
-- 
-- These are generally VHDL generics and Verilog parameters respectively.
--
type family GenType pass

type instance GenType AcnConv = NetTypeLike
type instance GenType AcnSynC = (Expression AcnSynC, NetType)
type instance GenType AcnNorm = (Expression AcnSynC, NetType)
type instance GenType AcnDone = (Expression AcnSynC, NetType)
    
    
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
        { typeName      :: !Acn.Id
        , constructors  :: [NetConstructor]
        , fields        :: [NetField]
        }
    deriving (Show, Generic, NFData)

data NetConstructor
    = NetConstructor
        { consName      :: !Acn.Id
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


-- |
-- Map of ACN identifiers to the declarations that create them.
--
-- N.B. that it's possible, but rare, for multiple IDs to map to the same
-- declaration (e.g. in the case of instance declarations).
--
type AcnBindings = Map Acn.Id SortedDecl

-- |
-- Region-annotated declarations and declarators. Helpful for optimizing,
-- as some optimizations are only applicable to certain regions.
--
data SortedDecl
    = Input  Declarator
    | Logic  Assignment
    | Output Assignment

-- |
-- Get an ACN declaration from a region-sorted declaration/declarator.
--
sortedDeclToDecl :: SortedDecl -> Maybe Assignment
sortedDeclToDecl = \case
    Input _  -> Nothing
    Logic d  -> Just d
    Output d -> Just d
    

