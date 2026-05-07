{-# LANGUAGE OverloadedStrings #-}

-- | Bridges the typed 'Writer IO' interface to the 'CopyWriter'.
--
-- The extractors call @Writer.writeBlock@, @Writer.writeTx@, etc. with
-- typed records. This adapter encodes each record to COPY text format
-- and dispatches to the 'CopyWriter' queue for the appropriate table.
module DbSync.Writer.CopyAdapter
  ( -- * Construction
    mkCopyWriterAdapter
  ) where

import Cardano.Prelude

import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Db.Schema.AdaPots (encodeAdaPotsCopy)
import DbSync.Db.Schema.Address (encodeAddressCopy)
import DbSync.Db.Schema.CBOR (encodeTxCborCopy)
import DbSync.Db.Schema.Core
  ( encodeBlockCopy
  , encodeSlotLeaderCopy
  , encodeTxCopy
  )
import DbSync.Db.Schema.UTxO
  ( encodeTxOutCopy
  , encodeTxInCopy
  , encodeCollateralTxInCopy
  , encodeReferenceTxInCopy
  )
import DbSync.Db.Schema.Metadata (encodeTxMetadataCopy)
import DbSync.Db.Schema.MultiAsset
  ( encodeMultiAssetCopy
  , encodeMaTxMintCopy
  , encodeMaTxOutCopy
  )
import DbSync.Db.Schema.StakeDelegation
  ( encodeStakeAddressCopy
  , encodeStakeRegistrationCopy
  , encodeStakeDeregistrationCopy
  , encodeDelegationCopy
  , encodeWithdrawalCopy
  )
import DbSync.Db.Schema.EpochSyncStats (encodeEpochSyncStatsCopy)
import DbSync.Db.Schema.Pool
  ( encodePoolHashCopy
  , encodePoolUpdateCopy
  , encodePoolMetadataRefCopy
  , encodePoolOwnerCopy
  , encodePoolRetireCopy
  , encodePoolRelayCopy
  )
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build a 'Writer IO' that encodes typed records and dispatches to
-- a 'CopyWriter'.
mkCopyWriterAdapter :: CopyWriter -> Writer IO
mkCopyWriterAdapter cw = Writer
  { -- Core
    writeBlock = \bid blk ->
      cwWriteRow cw "block" (encodeBlockCopy bid blk)
  , writeTx = \tid tx ->
      cwWriteRow cw "tx" (encodeTxCopy tid tx)
  , writeSlotLeader = \slid sl ->
      cwWriteRow cw "slot_leader" (encodeSlotLeaderCopy slid sl)

    -- UTxO
  , writeAddress = \aid addr ->
      cwWriteRow cw "address" (encodeAddressCopy aid addr)
  , writeTxOut = \oid txo ->
      cwWriteRow cw "tx_out" (encodeTxOutCopy oid txo)
  , writeTxIn = \iid ti ->
      cwWriteRow cw "tx_in" (encodeTxInCopy iid ti)
  , writeCollateralTxIn = \iid ci ->
      cwWriteRow cw "collateral_tx_in" (encodeCollateralTxInCopy iid ci)
  , writeReferenceTxIn = \iid ri ->
      cwWriteRow cw "reference_tx_in" (encodeReferenceTxInCopy iid ri)

    -- Metadata
  , writeTxMetadata = \mid md ->
      cwWriteRow cw "tx_metadata" (encodeTxMetadataCopy mid md)

    -- MultiAsset
  , writeMultiAsset = \mid ma ->
      cwWriteRow cw "multi_asset" (encodeMultiAssetCopy mid ma)
  , writeMaTxMint = \mid m ->
      cwWriteRow cw "ma_tx_mint" (encodeMaTxMintCopy mid m)
  , writeMaTxOut = \mid m ->
      cwWriteRow cw "ma_tx_out" (encodeMaTxOutCopy mid m)

    -- StakeDelegation
  , writeStakeAddress = \sid sa ->
      cwWriteRow cw "stake_address" (encodeStakeAddressCopy sid sa)
  , writeStakeRegistration = \sid sr ->
      cwWriteRow cw "stake_registration" (encodeStakeRegistrationCopy sid sr)
  , writeStakeDeregistration = \sid sd ->
      cwWriteRow cw "stake_deregistration" (encodeStakeDeregistrationCopy sid sd)
  , writeDelegation = \did d ->
      cwWriteRow cw "delegation" (encodeDelegationCopy did d)
  , writeWithdrawal = \wid w ->
      cwWriteRow cw "withdrawal" (encodeWithdrawalCopy wid w)

    -- Pool
  , writePoolHash = \pid ph ->
      cwWriteRow cw "pool_hash" (encodePoolHashCopy pid ph)
  , writePoolUpdate = \puid pu ->
      cwWriteRow cw "pool_update" (encodePoolUpdateCopy puid pu)
  , writePoolMetadataRef = \pmid pm ->
      cwWriteRow cw "pool_metadata_ref" (encodePoolMetadataRefCopy pmid pm)
  , writePoolOwner = \poid po ->
      cwWriteRow cw "pool_owner" (encodePoolOwnerCopy poid po)
  , writePoolRetire = \prid pr ->
      cwWriteRow cw "pool_retire" (encodePoolRetireCopy prid pr)
  , writePoolRelay = \prid pr ->
      cwWriteRow cw "pool_relay" (encodePoolRelayCopy prid pr)

    -- CBOR
  , writeTxCbor = \tcid tc ->
      cwWriteRow cw "tx_cbor" (encodeTxCborCopy tcid tc)

    -- EpochSyncStats
  , writeEpochSyncStats = \essid ess ->
      cwWriteRow cw "epoch_sync_stats" (encodeEpochSyncStatsCopy essid ess)

    -- EpochBoundary
  , writeAdaPots = \apid pots ->
      cwWriteRow cw "ada_pots" (encodeAdaPotsCopy apid pots)

    -- Transaction control
  , commit = cwCommit cw
  }
