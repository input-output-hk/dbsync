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

import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Accumulated state from a test writer.
data TestWriterState = TestWriterState
  { twBlocks      :: ![(BlockId, Block)]
  , twTxs         :: ![(TxId, Tx)]
  , twSlotLeaders :: ![(SlotLeaderId, SlotLeader)]
  , twCommits     :: !Int
  }
  deriving stock (Show)

-- | Empty test writer state.
emptyTestWriterState :: TestWriterState
emptyTestWriterState = TestWriterState [] [] [] 0

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build a 'Writer' that accumulates Core records in the given 'IORef'.
-- UTxO, Metadata, and MultiAsset write functions are no-ops in this
-- writer — use a specialised test writer if you need to capture those.
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

    -- UTxO (no-ops for core-only tests)
  , writeTxOut          = \_ _ -> pure ()
  , writeTxIn           = \_ _ -> pure ()
  , writeCollateralTxIn = \_ _ -> pure ()
  , writeReferenceTxIn  = \_ _ -> pure ()

    -- Metadata (no-op)
  , writeTxMetadata = \_ _ -> pure ()

    -- MultiAsset (no-ops)
  , writeMultiAsset = \_ _ -> pure ()
  , writeMaTxMint   = \_ _ -> pure ()
  , writeMaTxOut    = \_ _ -> pure ()

    -- Transaction control
  , commit =
      atomicModifyIORef' ref $ \s ->
        (s { twCommits = twCommits s + 1 }, ())
  }
