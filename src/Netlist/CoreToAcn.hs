
{-# LANGUAGE TemplateHaskell #-}

module Netlist.CoreToAcn where

import              Netlist.AcnIds
import              Netlist.AcnPrimitives
import              Netlist.AcnSyntax

import              Control.Lens (makeLenses)
import              Control.Monad.State.Lazy
import              GHC.Stack

import              Clash.Core.Name
import              Clash.Core.Subst
import              Clash.Core.Term
import              Clash.Core.Type
import              Clash.Core.Var
import              Clash.Core.VarEnv
import qualified    Clash.Unique as Unique


data NetlistState = NetlistState
    { _acnIdSet :: AcnIdSet
    -- ^ Database of names available to the ACN stage.
    }

makeLenses ''NetlistState

instance HasAcnIdSet NetlistState where
    acnIdentifierSet = acnIdSet

instance AcnNameMonad (State NetlistState) where
    acnNameNormalizerM = pure $ \nm ->
        AcnName nm nm [] Basic emptyCallStack

type NetlistMonad = State NetlistState


-- |
-- Map the Clash middle-end's internal Uniques to ACN's own IDs.
--
type IdMap = Unique.UniqMap AcnId

{-
convFunctionBinders
    :: HasCallStack
    => InScopeSet
    -> ( [Id], [LetBinding], Id )
    -> NetlistMonad
        ( [(AcnId, NetType)]
        , [AcnDeclaration]
        , [(AcnId, NetType)]
        , [AcnDeclaration]
        , [LetBinding]
        , Id
        )
convFunctionBinders inScope0 (args, binds, res) = do
    let (bndrs, exprs) = unzip binds
        inScope1 = inScope0 `extendInScopeSetList` (args ++ bndrs)

    -- Convert argument binders.
    (args', subst1, idMap1) <-
        convIds emptyUniqMap (mkSubst inScope1) args

    -- Convert result binder.
    ([res'], subst2, idMap2) <-
        convIds idMap1 subst1 [res]

    return undefined
-}

-- |
-- Create ACN IDs for each Core ID supplied. Returns a map of ACN IDs
-- indexed by the corresponding Core ID so that they may be looked up.
--
convIds
    :: IdMap    -- ^ Original map of Core IDs to ACN IDs.
    -> Subst    -- ^ Original substitution context.
    -> [Id]     -- ^ Core IDs to generate ACN IDs for.
    -> NetlistMonad ([Id], Subst, IdMap)
convIds = go [] where
    go
        :: [Id]     -- Processed IDs
        -> IdMap    -- Map to recover ACN IDs from Core ones
        -> Subst    -- Substitutions for IDs in Clash Core
        -> [Id]     -- IDs still to be processed
        -> NetlistMonad ([Id], Subst, IdMap)

    -- Finished processing all IDs; exit.
    go done idMap subst [] = return (reverse done, subst, idMap)

    -- Create a new unique for a Core ID, create a corresponding ACN ID,
    -- and associate them in a map.
    go done idMap subst0@(Subst inScope _ _ _) (i:is) = do
        newAcnId <- newAcnName $ nameOcc $ varName i
        let newCoreId = uniqAway inScope i
            subst1 = extendIdSubst subst0 i (Var newCoreId)
            subst2 = extendInScopeId subst1 newCoreId
            idMap' = Unique.extendUniqMap newCoreId newAcnId idMap
        go (newCoreId:done) idMap' subst2 is


-- |
-- Convert a Core type to an ACN type.
--
--convType
--    :: 

