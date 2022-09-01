
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Netlist.AcnIds
    ( AcnId (..)
    , verbatimId#
    , AcnIdTable (nameMap, idMap)
    , emptyIdTable
    , lookupAcnId
    , arithmeticId#
    )
  where

import              Control.DeepSeq
import              Data.HashMap.Strict (HashMap)
import qualified    Data.HashMap.Strict as HashMap
import              Data.IntMap (IntMap)
import qualified    Data.IntMap.Strict as IntMap
import              Data.Text (Text (..))
import              GHC.Generics (Generic)
import              GHC.Stack


-- |
-- Identifiers in ACN.
--
-- All generated identifiers are arithmetic to avoid worrying about
-- target language dependent concerns in the ACN representation itself.
-- Instead, backends can apply appropriate sanitization to the name
-- database independently.
--
data AcnId
    = ArithmeticId
        Int             -- ^ Key of name in name database.
        CallStack       -- ^ Origin of the identifier.
    | VerbatimId
        Text            -- ^ Exact name of the identifier.
        (Maybe AcnId)   -- ^ Optional parsed version of this id.
        CallStack       -- ^ Origin.
    deriving (Show, Generic, NFData)

-- |
-- Create an ACN identifier verbatim from some text. Doesn't update the
-- name database, and doesn't sanitize; just use the name exactly as given.
--
verbatimId# :: HasCallStack => Text -> AcnId
verbatimId# name = VerbatimId name Nothing originInfo


-- |
-- Name database for assisting ACN identifier generation.
--
data AcnIdTable
    = AcnIdTable
        { nameMap  :: IntMap Text
        -- ^ Searchable map of names.
        , idMap    :: HashMap Text AcnId
        -- ^ Match names to identifiers for id lookup.
        , idSupply :: Int
        -- ^ Arithmetic id supply.
        }
    deriving Show

-- |
-- An initial name database.
--
emptyIdTable = AcnIdTable IntMap.empty HashMap.empty 0

-- |
-- Lookup an ACN identifier in a name database.
--
lookupAcnId :: AcnId -> IntMap Text -> Maybe Text
lookupAcnId ident names =
    case ident of
        ArithmeticId n _ -> IntMap.lookup n names
        VerbatimId {}    -> Nothing

-- |
-- Generate a new identifier, and add its associated name to a name
-- database.
--
arithmeticId#
    :: HasCallStack
    => Text
    -> AcnIdTable
    -> (AcnIdTable, AcnId)
arithmeticId# name (AcnIdTable nameMap idMap supply) =
    (table', newId)
  where
    -- Create a new name.
    newId = ArithmeticId supply originInfo
    
    -- Update the table maps.
    nameMap' = IntMap.insert supply name nameMap
    idMap'   = HashMap.insert name newId idMap
      
    table' = AcnIdTable nameMap' idMap' (supply + 1)

    
-- TODO:
--  * [ ] makeBasic
--  * [ ] suffix
--  * [ ] toText
--  * [ ] next
--  * [x] unsafeFromCoreId
    

-- FUNNY DEBUG THINGS

-- |
-- Get the origin information of an identifier from GHC.
-- 
originInfo
    | originIsOn = callStack 
    | otherwise  = emptyCallStack

{-# INLINE originInfo #-}

-- |
-- Should we trace the origins of identifiers?
-- 
originIsOn = True
