
-----------------------------------------------------------
-- |
-- Module       : Acn.CompilerPass
-- Description  : Type-indexed ACN Compile Stages.
-----------------------------------------------------------
--
module Acn.CompilerPass
    ( AcnWorkingOn (..), AcnPass
    , AcnConv, AcnSynCk, AcnOpts, AcnNorm, AcnDone
    )
  where

-- |
-- Which conversions/checks ACN is working on in the current pass.
-- 
data AcnWorkingOn
    -- |
    -- Source language representation is being converted to ACN.
    = Converting
    
    -- |
    -- ACN is verifying that all types and terms are synthesizable
    -- in the target backend.
    | SynthesisChecking
    
    -- |
    -- ACN is normalizing HDL-specific syntax with the backend-provided
    -- normalizer functions.
    | Normalizing
    
    -- |
    -- ACN is done everything it had to do, and we can finally pass
    -- control to the backend.
    | Finished

-- |
-- Singleton type-index for compiler passes.
-- 
-- N.B. that some backends might want to override the normal ACN
-- compilation pipeline. If they want to do that, then they can
-- define their own type-index for their own transformation stages.
-- 
-- N.B. that when building ACN, GHC can't figure out enough about
-- our type-indexed families to derive instances directly. Instead,
-- we'll need to help it out with standalone derivations like:
--
-- @
-- deriving instance Show ('SynType' pass) => Show ('Component' pass)
-- @
--
data AcnPass (p :: AcnWorkingOn)
    
type AcnConv  = AcnPass 'Converting
type AcnSynCk = AcnPass 'SynthesisChecking
type AcnNorm  = AcnPass 'Normalizing
type AcnDone  = AcnPass 'Finished

-- |
-- Optional stage: after synthesis-checking, ACN can optimize its
-- netlist representation. Can be skipped if netlist rewriting isn't
-- necessary.
-- 
-- Invariant: optimizing always uses the same types as
-- synthesis-checking.
type AcnOpts = AcnSynCk


