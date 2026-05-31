-- | Bridges the typed 'Writer IO' interface to the 'LoaderStream'.
--
-- Each per-table write function lives in @Writer\/\<extractor\>.hs@
-- next door. This module composes them into a single 'Writer' record.
module DbSync.Phase.Ingest.Writer
  ( mkWriter
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Phase.Ingest.Writer.Cbor (writeTxCborCopy)
import DbSync.Phase.Ingest.Writer.Core
  ( writeBlockCopy
  , writeSlotLeaderCopy
  , writeTxCopy
  )
import DbSync.Phase.Ingest.Writer.Epoch (writeEpochSyncStatsCopy)
import DbSync.Phase.Ingest.Writer.EpochBoundary
  ( writeAdaPotsCopy
  , writeCostModelCopy
  , writeEpochParamCopy
  , writeEpochStateCopy
  )
import DbSync.Phase.Ingest.Writer.Metadata (writeTxMetadataCopy)
import DbSync.Phase.Ingest.Writer.MultiAsset
  ( writeMaTxMintCopy
  , writeMaTxOutCopy
  , writeMultiAssetCopy
  )
import DbSync.Phase.Ingest.Writer.Pool
  ( writePoolHashCopy
  , writePoolMetadataRefCopy
  , writePoolOwnerCopy
  , writePoolRelayCopy
  , writePoolRetireCopy
  , writePoolUpdateCopy
  )
import DbSync.Phase.Ingest.Writer.StakeDelegation
  ( writeDelegationCopy
  , writePotTransferCopy
  , writeReserveCopy
  , writeStakeAddressCopy
  , writeStakeDeregistrationCopy
  , writeStakeRegistrationCopy
  , writeTreasuryCopy
  , writeWithdrawalCopy
  )
import DbSync.Phase.Ingest.Writer.UTxO
  ( writeAddressCopy
  , writeCollateralTxInCopy
  , writeCollateralTxOutCopy
  , writeReferenceTxInCopy
  , writeTxInCopy
  , writeTxOutCopy
  )
import DbSync.Writer (Writer (..))

-- | Build a 'Writer IO' that encodes typed records and dispatches to
-- a 'LoaderStream'. One field per table, sourced from the
-- corresponding per-extractor module.
mkWriter :: LoaderStream -> Writer IO
mkWriter ls = Writer
  { -- Core
    writeBlock      = writeBlockCopy ls
  , writeTx         = writeTxCopy ls
  , writeSlotLeader = writeSlotLeaderCopy ls

    -- UTxO
  , writeAddress         = writeAddressCopy ls
  , writeTxOut           = writeTxOutCopy ls
  , writeTxIn            = writeTxInCopy ls
  , writeCollateralTxIn  = writeCollateralTxInCopy ls
  , writeCollateralTxOut = writeCollateralTxOutCopy ls
  , writeReferenceTxIn   = writeReferenceTxInCopy ls

    -- Metadata
  , writeTxMetadata = writeTxMetadataCopy ls

    -- MultiAsset
  , writeMultiAsset = writeMultiAssetCopy ls
  , writeMaTxMint   = writeMaTxMintCopy ls
  , writeMaTxOut    = writeMaTxOutCopy ls

    -- StakeDelegation (incl. pot rebalancing)
  , writeStakeAddress        = writeStakeAddressCopy ls
  , writeStakeRegistration   = writeStakeRegistrationCopy ls
  , writeStakeDeregistration = writeStakeDeregistrationCopy ls
  , writeDelegation          = writeDelegationCopy ls
  , writeWithdrawal          = writeWithdrawalCopy ls
  , writePotTransfer         = writePotTransferCopy ls
  , writeTreasury            = writeTreasuryCopy ls
  , writeReserve             = writeReserveCopy ls

    -- Pool
  , writePoolHash        = writePoolHashCopy ls
  , writePoolUpdate      = writePoolUpdateCopy ls
  , writePoolMetadataRef = writePoolMetadataRefCopy ls
  , writePoolOwner       = writePoolOwnerCopy ls
  , writePoolRetire      = writePoolRetireCopy ls
  , writePoolRelay       = writePoolRelayCopy ls

    -- CBOR
  , writeTxCbor = writeTxCborCopy ls

    -- EpochSyncStats
  , writeEpochSyncStats = writeEpochSyncStatsCopy ls

    -- EpochBoundary
  , writeAdaPots     = writeAdaPotsCopy ls
  , writeEpochParam  = writeEpochParamCopy ls
  , writeEpochState  = writeEpochStateCopy ls
  , writeCostModel   = writeCostModelCopy ls

    -- Transaction control
  , commit = lsCommit ls
  }
