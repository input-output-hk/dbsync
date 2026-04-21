{-# LANGUAGE OverloadedStrings #-}

-- | Writer interface for the unified extraction pipeline.
--
-- A 'Writer' defines how extracted rows reach PostgreSQL.
-- Two implementations exist:
--
-- * __IngestChainHistory__: COPY encoding → @putCopyData@ on per-table
--   @libpq@ connections. Epoch-aligned commits.
-- * __FollowingChainTip__: Simple @INSERT@ per record via @hasql@.
--   Per-block commits with rollback support.
--
-- Extractors are parameterised by 'Writer' so the same extraction
-- logic produces output for either phase.
module DbSync.Writer
  ( -- * Types
    Writer (..)
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to persist extracted rows. Parameterised by effect monad @m@.
--
-- Each @write*@ function takes the assigned ID and the typed record.
-- The implementation decides the encoding (COPY text vs INSERT SQL).
--
-- This record will grow as more extractors are implemented (UTxO adds
-- @writeTxOut@, @writeTxIn@; Metadata adds @writeMetadata@; etc.).
data Writer m = Writer
  { -- | Write a block row with its assigned ID.
    writeBlock      :: !(BlockId -> Block -> m ())

    -- | Write a transaction row with its assigned ID.
  , writeTx         :: !(TxId -> Tx -> m ())

    -- | Write a slot leader row with its assigned ID.
    -- Only called when the 'IdResolver' signals @isNew = True@.
  , writeSlotLeader :: !(SlotLeaderId -> SlotLeader -> m ())

    -- | Commit the current batch.
    -- Ingest: @putCopyEnd@ + @COMMIT@ on all COPY connections.
    -- Follow: @COMMIT@ on the hasql connection.
  , commit          :: !(m ())
  }
