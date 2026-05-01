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

    -- * Accessor class
  , HasWriter (..)
  ) where

import Cardano.Prelude (IO)

import DbSync.Db.Schema.CBOR (TxCbor)
import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Schema.MultiAsset (MultiAsset, MaTxMint, MaTxOut)
import DbSync.Db.Schema.Pool (PoolHash, PoolUpdate, PoolMetadataRef, PoolOwner, PoolRetire, PoolRelay)
import DbSync.Db.Schema.StakeDelegation (StakeAddress, StakeRegistration, StakeDeregistration, Delegation, Withdrawal)
import DbSync.Db.Schema.UTxO (TxOut, TxIn, CollateralTxIn, ReferenceTxIn)

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
    -- StakeDelegation tables
    -- ---------------------------------------------------------------
  , writeStakeAddress        :: !(StakeAddressId -> StakeAddress -> m ())
  , writeStakeRegistration   :: !(StakeRegistrationId -> StakeRegistration -> m ())
  , writeStakeDeregistration :: !(StakeDeregistrationId -> StakeDeregistration -> m ())
  , writeDelegation          :: !(DelegationId -> Delegation -> m ())
  , writeWithdrawal          :: !(WithdrawalId -> Withdrawal -> m ())

    -- ---------------------------------------------------------------
    -- Pool tables
    -- ---------------------------------------------------------------
  , writePoolHash        :: !(PoolHashId -> PoolHash -> m ())
  , writePoolUpdate      :: !(PoolUpdateId -> PoolUpdate -> m ())
  , writePoolMetadataRef :: !(PoolMetadataRefId -> PoolMetadataRef -> m ())
  , writePoolOwner       :: !(PoolOwnerId -> PoolOwner -> m ())
  , writePoolRetire      :: !(PoolRetireId -> PoolRetire -> m ())
  , writePoolRelay       :: !(PoolRelayId -> PoolRelay -> m ())

    -- ---------------------------------------------------------------
    -- CBOR tables
    -- ---------------------------------------------------------------
  , writeTxCbor :: !(TxCborId -> TxCbor -> m ())

    -- ---------------------------------------------------------------
    -- EpochSyncStats tables
    -- ---------------------------------------------------------------
  , writeEpochSyncStats :: !(EpochSyncStatsId -> EpochSyncStats -> m ())

    -- ---------------------------------------------------------------
    -- Transaction control
    -- ---------------------------------------------------------------
  , commit :: !(m ())
  }

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

-- | Access the (IO-effecting) writer from any environment.
--
-- See 'DbSync.Resolver.HasResolver' for the rationale on fixing the effect
-- monad to 'IO' at the env layer.
class HasWriter env where
  getWriter :: env -> Writer IO
