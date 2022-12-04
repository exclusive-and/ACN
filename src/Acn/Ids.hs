
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------
-- |
-- Module       : Acn.Ids
-- Description  : Language Agnostic Implementation of ACN Identifiers.
-----------------------------------------------------------
-- 
module Acn.Ids
    ( -- * Introduction and elimination.
      newIdFromCache#, idToText#

      -- * ACN identifier type.
    , Id (..)
    , verbatimId#

      -- * Parsed names.
    , Name (..), NameType (..)
    , nameToText#

      -- * Identifier databases.
    , HasIdSet (..), IdSet (nameMap, seenIds)
    , emptyIdSet

      -- * API and monadic functions.
    , IdMonad (..), NameMonad (..)
    , newName#, newName
    , suffix#, suffix
    )
  where

import              Acn.CompilerPass
  
import              Control.DeepSeq
import              Control.Lens (Lens', (.=))
import qualified    Control.Lens as Lens
import qualified    Control.Monad.State.Lazy as Lazy
import qualified    Control.Monad.State.Strict as Strict
import              Data.Function ((&), on)
import              Data.Hashable (Hashable (..))
import              Data.HashMap.Strict (HashMap)
import qualified    Data.HashMap.Strict as HashMap
import              Data.HashSet (HashSet)
import qualified    Data.HashSet as HashSet
import              Data.IntMap (IntMap)
import qualified    Data.IntMap.Strict as IntMap
import qualified    Data.List as List
import qualified    Data.Maybe as Maybe
import              Data.Text (Text)
import qualified    Data.Text as Text
import              GHC.Generics (Generic)
import              GHC.Stack


-- CRITICAL TYPES

-- |
-- Identifiers in ACN.
--
-- All generated identifiers are arithmetic to avoid worrying about
-- target language dependent concerns in the ACN representation itself.
-- Instead, backends can apply appropriate sanitization to the name
-- database independently.
--
data Id
    = ArithmeticId
        Int             -- ^ Key of name in name database.
    | VerbatimId
        Text            -- ^ Exact name of the identifier.
        (Maybe Name)    -- ^ Optional parsed version of this id.
        CallStack       -- ^ Origin.
    deriving (Show, Generic, NFData)

-- |
-- Get an version of an ACN identifier that can be compared.
--
acnKey# :: Id -> Either Int Text
acnKey# = \case
    ArithmeticId n    -> Left n
    VerbatimId nm _ _ -> Right nm

instance Eq Id where
    (==) = (==) `on` acnKey#
    (/=) = (/=) `on` acnKey#

instance Ord Id where
    compare = compare `on` acnKey#

-- |
-- Create an ACN identifier verbatim from some text. Doesn't update the
-- name database, and doesn't sanitize; just use the name exactly as given.
--
verbatimId# :: HasCallStack => Text -> Id
verbatimId# name =
  let
    originInfo
        | originIsOn = callStack
        | otherwise  = emptyCallStack
  in
    VerbatimId name Nothing originInfo


-- |
-- Normalized names stored in a database indexed by arithmetic identifiers.
--
data Name
    = Name
        { originalName      :: Text
        -- ^ Name as it appears in source with extensions removed.
        , nameNormalized    :: Text
        -- ^ Version of 'originalName' converted depending on desired lookup
        -- behaviour. Avoids name collisions in generated HDL.
        , extensions        :: [Word]
        , nameType          :: NameType
        , nameOrigin        :: CallStack
        -- ^ The origin of this name in the compiler.
        }
    deriving (Show, Generic, NFData)

-- |
-- Convert an ACN name to text.
-- 
nameToText# :: Name -> Text
nameToText# nm =
  let
    exts = map (Text.pack . show) $ reverse $ extensions nm
  in
    Text.intercalate "_" (originalName nm : exts)
    
-- |
-- Get the normalized name and extensions of an ACN name as search keys.
--
nameKey# :: Name -> (Text, [Word])
nameKey# (Name _ nm exts _ _) = (nm, exts)

instance Eq Name where
    (==) = (==) `on` nameKey#
    (/=) = (/=) `on` nameKey#

instance Hashable Name where
    hashWithSalt salt = hashWithSalt salt . hash

    hash =
      let
        fuzz factor ext = factor * factor * ext
        hash# nm exts = hash (nm, List.foldl' fuzz 2 exts)
      in
        uncurry hash# . nameKey#

data NameType = Basic | Extended
    deriving (Show, Generic, NFData)


class HasIdSet s where
    identifierSet :: Lens' s IdSet

instance HasIdSet IdSet where
    identifierSet = ($)

-- |
-- Name database for assisting ACN identifier generation.
--
data IdSet pass
    = IdSet
        { nameMap    :: IntMap Name
        -- ^ Searchable map of names.
        , idSupply   :: Int
        -- ^ Arithmetic id supply.
        , seenIds    :: HashSet Name
        , freshCache :: FreshCache
        }
    deriving Show

-- |
-- An initial name database.
--
emptyIdSet = IdSet
    { nameMap    = IntMap.empty
    , idSupply   = 0
    , seenIds    = HashSet.empty
    , freshCache = HashMap.empty
    }


-- INTRODUCTION AND ELMINATION FUNCTIONS

-- |
-- Convert an ACN ID to text, either by looking it up in a database
-- or emitting it verbatim.
-- 
idToText# :: Id -> IdSet -> Maybe Text
idToText# ident idSet = case ident of
    ArithmeticId n ->
      let
        nm = IntMap.lookup n $ nameMap idSet
      in
        nameToText# <$> nm
    
    VerbatimId t _ _ -> Just t

-- |
-- Create and cache a new identifier.
--
newIdFromCache#
    :: HasCallStack
    => Name         -- ^ Normalized identifier name.
    -> IdSet        -- ^ Working ID database.
    -> (Id, IdSet)
newIdFromCache# nm idSet =
  let
    -- Record the origin of the new name.
    nmCopy = nm { nameOrigin = originInfo } where
        originInfo
            | originIsOn = callStack
            | otherwise  = emptyCallStack

    -- Check the fresh cache for the most recent top-level extension for
    -- this name.
    fresh = freshCache idSet

    newNm = case lookupFreshCache fresh nmCopy of
        Just currentMax ->
          let
            oldExts = drop 1 $ extensions nmCopy
            newExts = currentMax + 1 : oldExts
          in
            nmCopy { extensions = newExts }

        Nothing -> nmCopy

    fresh' = updateFreshCache fresh newNm

    -- Get the next key, and update the name database.
    newKey = idSupply idSet

    names' = nameMap idSet & IntMap.insert newKey newNm
    seen'  = seenIds idSet & HashSet.insert newNm

    idSet' = idSet
        { nameMap    = names'
        , idSupply   = idSupply idSet + 1
        , seenIds    = seen'
        , freshCache = fresh'
        }
  in
    (ArithmeticId newKey, idSet')

-- |
--
newId#
    :: HasCallStack
    => Name
    -> IdSet
    -> (Id, IdSet)
newId# nm idSet =
  let
    -- Check for this name in the set of already seen IDs. If it's already
    -- been seen, extend it and try again. Otherwise, return as-is.
    newNm
        | nm `HashSet.member` seenIds idSet
        = nm { extensions = 0 : extensions nm }

        | otherwise
        = nm
  in
    newIdFromCache# newNm idSet

-- |
-- Cache of most recent extensions indexed by base names and length.
-- 
type FreshCache = HashMap Text (IntMap Word)

lookupFreshCache :: FreshCache -> Name -> Maybe Word
lookupFreshCache cache nm = do
    section <- HashMap.lookup (nameNormalized nm) cache
    IntMap.lookup (length $ extensions nm) section

updateFreshCache :: FreshCache -> Name -> FreshCache
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


-- EXTERNAL API AND CONVENIENCE FUNCTIONS

-- |
-- Lookup an ACN identifier in a name database.
--
lookupId# :: Id -> IntMap Name -> Maybe Name
lookupId# ident names =
    case ident of
        ArithmeticId n -> IntMap.lookup n names
        VerbatimId {}  -> Nothing


-- |
-- Types with an ID database that can be used and updated monadically.
--
class Monad m => IdMonad m where
    idSetM :: (IdSet -> IdSet) -> m IdSet

-- |
-- Monadically apply a function on an ambient ID database.
--
withIdSetM
    :: IdMonad m
    => (IdSet -> (b, IdSet))
    -> m b
withIdSetM f = do
    idSet <- idSetM id
    let (b, idSet') = f idSet
    _ <- idSetM (const idSet')
    return b

instance HasIdSet s => IdMonad (Strict.State s) where
    idSetM f = do
        idSet <- Lens.use identifierSet
        identifierSet .= f idSet
        Lens.use identifierSet

instance HasIdSet s => IdMonad (Lazy.State s) where
    idSetM f = do
        idSet <- Lens.use identifierSet
        identifierSet .= f idSet
        Lens.use identifierSet

-- |
-- Monads with a name normalizer built in.
--
class IdMonad m => NameMonad m where
    nameNormalizerM :: m (Text -> Name)


-- |
-- Create a new entry in the name database from a string.
-- 
newName#
    :: HasCallStack
    => (Text -> Name)
    -> Text
    -> IdSet
    -> (Id, IdSet)
newName# normalizer nm = newId# (normalizer nm)

{-# INLINE newName# #-}

newName
    :: (HasCallStack, NameMonad m)
    => Text
    -> m Id
newName nm = do
    normalizer <- nameNormalizerM
    withIdSetM (newName# normalizer nm)

-- |
-- Create a new entry in the name database that combines the base
-- name and suffix into a new ID.
-- 
suffix#
    :: HasCallStack
    => (Text -> Name)
    -> Name
    -> Text
    -> IdSet
    -> (Id, IdSet)
suffix# normalizer nm0 affix idSet =
  let
    nm1 = normalizer $ originalName nm0 <> "_" <> affix
    nm2 = nm1 { extensions = extensions nm1 <> extensions nm0 }
  in
    newId# nm2 idSet
    
suffix
    :: (HasCallStack, NameMonad m)
    => Name
    -> Text
    -> m Id
suffix nm affix = do
    normalizer <- nameNormalizerM
    withIdSetM (suffix# normalizer nm affix)


-- FUNNY DEBUG THINGS

-- |
-- Should we trace the origins of identifiers?
-- 
originIsOn = True

