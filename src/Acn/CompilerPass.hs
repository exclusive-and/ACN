
-----------------------------------------------------------
-- |
-- Module       : Acn.CompilerPass
-- Description  : Type-indexed ACN Compile Stages.
-----------------------------------------------------------
--
module Acn.CompilerPass
    ( AcnWorkingOn (..), AcnPass (..)
    , AcnConv, AcnSynC, AcnOpts, AcnNorm, AcnDone
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
    | SynthesizeChecking
    
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
data AcnPass (p :: AcnWorkingOn)
    
type AcnConv = AcnPass 'Converting
type AcnSynC = AcnPass 'SynthesizeChecking
type AcnNorm = AcnPass 'Normalizing
type AcnDone = AcnPass 'Finished

-- |
-- Optional stage: after synthesis-checking, ACN can optimize its
-- netlist representation. Can be skipped if netlist rewriting isn't
-- necessary.
-- 
-- Invariant: optimizing always uses the same types as
-- synthesis-checking.
type AcnOpts = AcnSynC
