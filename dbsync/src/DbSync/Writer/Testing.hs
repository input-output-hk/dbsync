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

-- | Build a 'Writer' that accumulates all written records in the given 'IORef'.
--
-- For use in tests. No IO, no database — just list append.
mkTestWriter :: IORef TestWriterState -> Writer IO
mkTestWriter ref = Writer
  { writeBlock = \bid blk ->
      atomicModifyIORef' ref $ \s ->
        (s { twBlocks = twBlocks s ++ [(bid, blk)] }, ())

  , writeTx = \tid tx ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxs = twTxs s ++ [(tid, tx)] }, ())

  , writeSlotLeader = \slid sl ->
      atomicModifyIORef' ref $ \s ->
        (s { twSlotLeaders = twSlotLeaders s ++ [(slid, sl)] }, ())

  , commit =
      atomicModifyIORef' ref $ \s ->
        (s { twCommits = twCommits s + 1 }, ())
  }
