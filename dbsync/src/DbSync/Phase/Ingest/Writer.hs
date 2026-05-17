{-# LANGUAGE OverloadedStrings #-}

-- | Bridges the typed 'Writer IO' interface to the 'LoaderStream'.
--
-- The extractors call @Writer.writeBlock@, @Writer.writeTx@, etc. with
-- typed records. This adapter encodes each record to the loader-stream
-- wire format and dispatches to the queue for the appropriate table.
module DbSync.Phase.Ingest.Writer
  ( -- * Construction
    mkWriter
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
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
  , encodeCollateralTxOutCopy
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
-- a 'LoaderStream'.
mkWriter :: LoaderStream -> Writer IO
mkWriter bs = Writer
  { -- Core
    writeBlock = \bid blk ->
      lsWriteRow bs "block" (encodeBlockCopy bid blk)
  , writeTx = \tid tx ->
      lsWriteRow bs "tx" (encodeTxCopy tid tx)
  , writeSlotLeader = \slid sl ->
      lsWriteRow bs "slot_leader" (encodeSlotLeaderCopy slid sl)

    -- UTxO
  , writeAddress = \aid addr ->
      lsWriteRow bs "address" (encodeAddressCopy aid addr)
  , writeTxOut = \oid txo ->
      lsWriteRow bs "tx_out" (encodeTxOutCopy oid txo)
  , writeTxIn = \iid ti ->
      lsWriteRow bs "tx_in" (encodeTxInCopy iid ti)
  , writeCollateralTxIn = \iid ci ->
      lsWriteRow bs "collateral_tx_in" (encodeCollateralTxInCopy iid ci)
  , writeCollateralTxOut = \oid co ->
      lsWriteRow bs "collateral_tx_out" (encodeCollateralTxOutCopy oid co)
  , writeReferenceTxIn = \iid ri ->
      lsWriteRow bs "reference_tx_in" (encodeReferenceTxInCopy iid ri)

    -- Metadata
  , writeTxMetadata = \mid md ->
      lsWriteRow bs "tx_metadata" (encodeTxMetadataCopy mid md)

    -- MultiAsset
  , writeMultiAsset = \mid ma ->
      lsWriteRow bs "multi_asset" (encodeMultiAssetCopy mid ma)
  , writeMaTxMint = \mid m ->
      lsWriteRow bs "ma_tx_mint" (encodeMaTxMintCopy mid m)
  , writeMaTxOut = \mid m ->
      lsWriteRow bs "ma_tx_out" (encodeMaTxOutCopy mid m)

    -- StakeDelegation
  , writeStakeAddress = \sid sa ->
      lsWriteRow bs "stake_address" (encodeStakeAddressCopy sid sa)
  , writeStakeRegistration = \sid sr ->
      lsWriteRow bs "stake_registration" (encodeStakeRegistrationCopy sid sr)
  , writeStakeDeregistration = \sid sd ->
      lsWriteRow bs "stake_deregistration" (encodeStakeDeregistrationCopy sid sd)
  , writeDelegation = \did d ->
      lsWriteRow bs "delegation" (encodeDelegationCopy did d)
  , writeWithdrawal = \wid w ->
      lsWriteRow bs "withdrawal" (encodeWithdrawalCopy wid w)

    -- Pool
  , writePoolHash = \pid ph ->
      lsWriteRow bs "pool_hash" (encodePoolHashCopy pid ph)
  , writePoolUpdate = \puid pu ->
      lsWriteRow bs "pool_update" (encodePoolUpdateCopy puid pu)
  , writePoolMetadataRef = \pmid pm ->
      lsWriteRow bs "pool_metadata_ref" (encodePoolMetadataRefCopy pmid pm)
  , writePoolOwner = \poid po ->
      lsWriteRow bs "pool_owner" (encodePoolOwnerCopy poid po)
  , writePoolRetire = \prid pr ->
      lsWriteRow bs "pool_retire" (encodePoolRetireCopy prid pr)
  , writePoolRelay = \prid pr ->
      lsWriteRow bs "pool_relay" (encodePoolRelayCopy prid pr)

    -- CBOR
  , writeTxCbor = \tcid tc ->
      lsWriteRow bs "tx_cbor" (encodeTxCborCopy tcid tc)

    -- EpochSyncStats
  , writeEpochSyncStats = \essid ess ->
      lsWriteRow bs "epoch_sync_stats" (encodeEpochSyncStatsCopy essid ess)

    -- EpochBoundary
  , writeAdaPots = \apid pots ->
      lsWriteRow bs "ada_pots" (encodeAdaPotsCopy apid pots)

    -- Transaction control
  , commit = lsCommit bs
  }
