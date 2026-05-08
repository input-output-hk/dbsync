{-# LANGUAGE OverloadedStrings #-}

-- | Test writer for the unified extraction pipeline.
--
-- Captures written records in 'IORef's for test assertions.
-- No database, no COPY encoding — just accumulates typed records.
module DbSync.Writer.Testing
  ( -- * Construction
    mkTestWriter
  , TestWriterState (..)
  , emptyTestWriterState
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.Ids
  ( AddressId
  , BlockId
  , PoolHashId
  , SlotLeaderId
  , StakeAddressId
  , TxId
  , TxOutId
  )
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Schema.UTxO (TxOut)
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Accumulated state from a test writer.
data TestWriterState = TestWriterState
  { twBlocks         :: ![(BlockId, Block)]
  , twTxs            :: ![(TxId, Tx)]
  , twSlotLeaders    :: ![(SlotLeaderId, SlotLeader)]
  , twAddresses      :: ![(AddressId, Address)]
  , twTxOuts         :: ![(TxOutId, TxOut)]
  , twStakeAddresses :: ![(StakeAddressId, StakeAddress)]
  , twPoolHashes     :: ![(PoolHashId, PoolHash)]
  , twCommits        :: !Int
  }
  deriving stock (Show)

-- | Empty test writer state.
emptyTestWriterState :: TestWriterState
emptyTestWriterState = TestWriterState [] [] [] [] [] [] [] 0

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build a 'Writer' that captures the row-producing calls used by
-- the unit tests. Calls without a corresponding field on
-- 'TestWriterState' are no-ops.
mkTestWriter :: IORef TestWriterState -> Writer IO
mkTestWriter ref = Writer
  { -- Core
    writeBlock = \bid blk ->
      atomicModifyIORef' ref $ \s ->
        (s { twBlocks = twBlocks s ++ [(bid, blk)] }, ())
  , writeTx = \tid tx ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxs = twTxs s ++ [(tid, tx)] }, ())
  , writeSlotLeader = \slid sl ->
      atomicModifyIORef' ref $ \s ->
        (s { twSlotLeaders = twSlotLeaders s ++ [(slid, sl)] }, ())

    -- UTxO
  , writeAddress = \aid addr ->
      atomicModifyIORef' ref $ \s ->
        (s { twAddresses = twAddresses s ++ [(aid, addr)] }, ())
  , writeTxOut = \oid txOut ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxOuts = twTxOuts s ++ [(oid, txOut)] }, ())
  , writeTxIn           = \_ _ -> pure ()
  , writeCollateralTxIn = \_ _ -> pure ()
  , writeReferenceTxIn  = \_ _ -> pure ()

    -- Metadata (no-op)
  , writeTxMetadata = \_ _ -> pure ()

    -- MultiAsset (no-ops)
  , writeMultiAsset = \_ _ -> pure ()
  , writeMaTxMint   = \_ _ -> pure ()
  , writeMaTxOut    = \_ _ -> pure ()

    -- StakeDelegation
  , writeStakeAddress = \said sa ->
      atomicModifyIORef' ref $ \s ->
        (s { twStakeAddresses = twStakeAddresses s ++ [(said, sa)] }, ())
  , writeStakeRegistration   = \_ _ -> pure ()
  , writeStakeDeregistration = \_ _ -> pure ()
  , writeDelegation          = \_ _ -> pure ()
  , writeWithdrawal          = \_ _ -> pure ()

    -- Pool
  , writePoolHash = \phid ph ->
      atomicModifyIORef' ref $ \s ->
        (s { twPoolHashes = twPoolHashes s ++ [(phid, ph)] }, ())
  , writePoolUpdate      = \_ _ -> pure ()
  , writePoolMetadataRef = \_ _ -> pure ()
  , writePoolOwner       = \_ _ -> pure ()
  , writePoolRetire      = \_ _ -> pure ()
  , writePoolRelay       = \_ _ -> pure ()

    -- CBOR (no-op)
  , writeTxCbor = \_ _ -> pure ()

    -- EpochSyncStats (no-op)
  , writeEpochSyncStats = \_ _ -> pure ()

    -- EpochBoundary (no-op)
  , writeAdaPots = \_ _ -> pure ()

    -- Transaction control
  , commit =
      atomicModifyIORef' ref $ \s ->
        (s { twCommits = twCommits s + 1 }, ())
  }
