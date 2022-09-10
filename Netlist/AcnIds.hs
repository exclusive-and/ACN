
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Netlist.AcnIds
    ( newAcnId#
    , AcnId (..)
    , verbatimId#
    , AcnIdSet (nameMap, seenIds)
    , emptyAcnSet
    , lookupAcnId
    )
  where

import              Control.DeepSeq
import              Data.Function (($), (&))
import              Data.Hashable (Hashable (..))
import              Data.HashMap.Strict (HashMap)
import qualified    Data.HashMap.Strict as HashMap
import              Data.HashSet (HashSet)
import qualified    Data.HashSet as HashSet
import              Data.IntMap (IntMap)
import qualified    Data.IntMap.Strict as IntMap
import qualified    Data.List as List
import qualified    Data.Maybe as Maybe
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
    | VerbatimId
        Text            -- ^ Exact name of the identifier.
        (Maybe AcnName) -- ^ Optional parsed version of this id.
        CallStack       -- ^ Origin.
    deriving (Show, Generic, NFData)

-- |
-- Create an ACN identifier verbatim from some text. Doesn't update the
-- name database, and doesn't sanitize; just use the name exactly as given.
--
verbatimId# :: HasCallStack => Text -> AcnId
verbatimId# name =
  let
    originInfo
        | originIsOn = callStack
        | otherwise  = emptyCallStack
  in
    VerbatimId name Nothing originInfo


-- |
--
--
data AcnName
    = AcnName
        { originalName      :: Text
        -- ^ Name as it appears in source with extensions removed.
        , nameNormalized    :: Text
        -- ^ Version of 'originalName' converted depending on desired lookup
        -- behaviour. Avoids name collisions in generated HDL.
        , extensions        :: [Word]
        , nameOrigin        :: CallStack
        -- ^ The origin of this name in the compiler.
        }
    deriving (Show, Generic, NFData)

-- |
-- Get the normalized name and extensions of an ACN name as search keys.
--
acnKey# :: AcnName -> (Text, [Word])
acnKey# (AcnName _ nm exts _) = (nm, exts)

instance Eq AcnName where
    id1 == id2 = acnKey# id1 == acnKey# id2
    id1 /= id2 = acnKey# id1 /= acnKey# id2

instance Hashable AcnName where
    hashWithSalt salt = hashWithSalt salt . hash

    hash =
      let
        fuzz factor ext = factor * factor * ext
        hash# nm exts = hash (nm, List.foldl' fuzz 2 exts)
      in
        uncurry hash# . acnKey#


-- |
-- Name database for assisting ACN identifier generation.
--
data AcnIdSet
    = AcnIdSet
        { nameMap    :: IntMap AcnName
        -- ^ Searchable map of names.
        , idSupply   :: Int
        -- ^ Arithmetic id supply.
        , seenIds    :: HashSet AcnName
        , freshCache :: FreshCache
        }
    deriving Show

-- |
-- An initial name database.
--
emptyAcnSet = AcnIdSet
    { nameMap    = IntMap.empty
    , idSupply   = 0
    , seenIds    = HashSet.empty
    , freshCache = HashMap.empty
    }

-- |
-- Lookup an ACN identifier in a name database.
--
lookupAcnId :: AcnId -> IntMap AcnName -> Maybe AcnName
lookupAcnId ident names =
    case ident of
        ArithmeticId n -> IntMap.lookup n names
        VerbatimId {}  -> Nothing


-- |
-- Generate a new identifier, and add its associated name to a name database.
--
newAcnId#
    :: HasCallStack
    => AcnName      -- ^ 
    -> AcnIdSet     -- ^ Working ID database.
    -> (AcnId, AcnIdSet)
newAcnId# nm idSet =
  let
    -- Record the origin of the new name.
    nmCopy = nm {nameOrigin = originInfo} where
        originInfo
            | originIsOn = callStack
            | otherwise  = emptyCallStack

    -- Check for this name in the set of already seen IDs. If it's already
    -- been seen, extend it and try again. Otherwise, return as-is.
    newNm
        | nm `HashSet.member` seenIds idSet
        = nmCopy {extensions = 0 : extensions nm}

        | otherwise
        = nmCopy

    -- Check the most recent top extension for this name in the fresh cache.
    fresh = freshCache idSet
    
    newNm' = case lookupFreshCache fresh newNm of
        Just currentMax ->
          let
            oldExts = extensions newNm
            newExts = currentMax + 1 : tail oldExts
          in
            newNm {extensions = newExts}

        Nothing -> newNm

    fresh' = updateFreshCache fresh newNm'
        
    -- Get the next key, and update the name database.
    newKey = idSupply idSet

    names' = nameMap idSet & IntMap.insert newKey newNm'
    seen'  = seenIds idSet & HashSet.insert newNm'

    idSet' = idSet
        { nameMap    = names'
        , idSupply   = idSupply idSet + 1
        , seenIds    = seen'
        , freshCache = fresh'
        }
  in
    (ArithmeticId newKey, idSet')


-- |
-- Cache of most recent extensions indexed by base names and length.
-- 
type FreshCache = HashMap Text (IntMap Word)

lookupFreshCache :: FreshCache -> AcnName -> Maybe Word
lookupFreshCache cache nm = do
    section <- HashMap.lookup (nameNormalized nm) cache
    IntMap.lookup (length $ extensions nm) section

updateFreshCache :: FreshCache -> AcnName -> FreshCache
updateFreshCache cache nm =
  let
    nm'  = nameNormalized nm
    exts = extensions nm

    topExt = Maybe.fromMaybe 0 $ Maybe.listToMaybe exts

    alter f e = Just . f . Maybe.fromMaybe e
    
    go0 f = HashMap.alter (alter f mempty) nm' cache
    go1 f = IntMap.alter (alter f 0) (length exts)
  in
    go0 (go1 (max topExt))


-- TODO:
--  * [x] makeBasic
--        newAcnName#
--
--  * [x] suffix
--        suffix#
--
--  * [ ] toText
--
--
--  * [x] next
--
--
--  * [x] unsafeFromCoreId
--        verbatimId#
--

-- |
-- Create a new entry in the name database from a string.
-- 
newAcnName#
    :: HasCallStack
    => (Text -> AcnName)
    -> Text
    -> AcnIdSet
    -> (AcnId, AcnIdSet)
newAcnName# normalizer nm = newAcnId# (normalizer nm)

{-# INLINE newAcnName# #-}

-- |
-- Create a new entry in the name database that combines the base
-- name and suffix into a new ID.
-- 
suffix#
    :: HasCallStack
    => (Text -> AcnName)
    -> AcnName
    -> Text
    -> AcnIdSet
    -> (AcnId, AcnIdSet)
suffix# normalizer nm0 affix idSet =
  let
    nm1 = normalizer $ originalName nm0 <> "_" <> affix
    nm2 = nm1 { extensions = extensions nm1 <> extensions nm0 }
  in
    newAcnId# nm2 idSet
    

-- FUNNY DEBUG THINGS

-- |
-- Should we trace the origins of identifiers?
-- 
originIsOn = True
