{-# LANGUAGE OverloadedStrings #-}

-- | ID resolution interface for the unified extraction pipeline.
--
-- An 'IdResolver' provides the mechanism for obtaining database IDs
-- during block processing. Two implementations exist:
--
-- * 'DbSync.Resolver.Ingest' — DedupMap\/Counter-based for 'IngestChainHistory'
-- * (future) @DbSync.Resolver.Follow@ — SELECT→INSERT for 'FollowingChainTip'
--
-- Extractors are parameterised by 'IdResolver' so the same extraction
-- logic works in both phases.
module DbSync.Resolver
  ( -- * Types
    IdResolver (..)
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to obtain database IDs during block processing.
--
-- The @m@ parameter is the effect monad — 'IO' in production,
-- potentially a test monad in tests.
--
-- Two implementations:
--
-- * __IngestChainHistory__: 'assignBlockId' increments a counter;
--   'resolveSlotLeader' does a DedupMap lookup-or-insert.
--   All in-memory, no database queries.
--
-- * __FollowingChainTip__: 'assignBlockId' returns an ID from
--   @INSERT ... RETURNING id@; 'resolveSlotLeader' does
--   @SELECT → INSERT if missing@. No in-memory caches.
data IdResolver m = IdResolver
  { -- | Assign the next block ID.
    -- Ingest: increment counter. Follow: reserved (Writer does INSERT RETURNING).
    assignBlockId     :: !(m BlockId)

    -- | Assign the next transaction ID.
    -- Ingest: increment counter. Follow: reserved (Writer does INSERT RETURNING).
  , assignTxId        :: !(m TxId)

    -- | Resolve a slot leader by its hash.
    -- Returns @(SlotLeaderId, isNew)@. When @isNew = True@, the caller
    -- should also write the 'SlotLeader' row via the 'Writer'.
    -- Ingest: DedupMap lookup-or-insert. Follow: SELECT → INSERT if missing.
  , resolveSlotLeader :: !(ByteString -> SlotLeader -> m (SlotLeaderId, Bool))

    -- | Look up the previous block's ID by its hash.
    -- Ingest: returns the last assigned BlockId (sequential processing).
    -- Follow: @SELECT id FROM block WHERE hash = $1@.
  , resolvePrevBlock  :: !(ByteString -> m (Maybe BlockId))
  }
