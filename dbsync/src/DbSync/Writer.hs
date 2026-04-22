{-# LANGUAGE OverloadedStrings #-}

-- | Writer interface for the unified extraction pipeline.
--
-- A 'Writer' defines how extracted rows reach PostgreSQL.
-- Two implementations exist:
--
-- * __IngestChainHistory__: COPY encoding -> @putCopyData@ on per-table
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
import DbSync.Db.Schema.UTxO (TxOut, TxIn, CollateralTxIn, ReferenceTxIn)
import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Schema.MultiAsset (MultiAsset, MaTxMint, MaTxOut)
import DbSync.Db.Schema.Ids

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to persist extracted rows. Parameterised by effect monad @m@.
--
-- Each @write*@ function takes the assigned ID and the typed record.
-- The implementation decides the encoding (COPY text vs INSERT SQL).
data Writer m = Writer
  { -- ---------------------------------------------------------------
    -- Core tables
    -- ---------------------------------------------------------------
    writeBlock      :: !(BlockId -> Block -> m ())
  , writeTx         :: !(TxId -> Tx -> m ())
  , writeSlotLeader :: !(SlotLeaderId -> SlotLeader -> m ())

    -- ---------------------------------------------------------------
    -- UTxO tables
    -- ---------------------------------------------------------------
  , writeTxOut          :: !(TxOutId -> TxOut -> m ())
  , writeTxIn           :: !(TxInId -> TxIn -> m ())
  , writeCollateralTxIn :: !(CollateralTxInId -> CollateralTxIn -> m ())
  , writeReferenceTxIn  :: !(ReferenceTxInId -> ReferenceTxIn -> m ())

    -- ---------------------------------------------------------------
    -- Metadata tables
    -- ---------------------------------------------------------------
  , writeTxMetadata :: !(TxMetadataId -> TxMetadata -> m ())

    -- ---------------------------------------------------------------
    -- MultiAsset tables
    -- ---------------------------------------------------------------
  , writeMultiAsset :: !(MultiAssetId -> MultiAsset -> m ())
  , writeMaTxMint   :: !(MaTxMintId -> MaTxMint -> m ())
  , writeMaTxOut    :: !(MaTxOutId -> MaTxOut -> m ())

    -- ---------------------------------------------------------------
    -- Transaction control
    -- ---------------------------------------------------------------
  , commit :: !(m ())
  }
