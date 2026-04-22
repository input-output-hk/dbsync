{-# LANGUAGE OverloadedStrings #-}

-- | Bridges the typed 'Writer IO' interface to the 'CopyWriter'.
--
-- The extractors call @Writer.writeBlock@, @Writer.writeTx@, etc. with
-- typed records. This adapter encodes each record to COPY text format
-- (via the encoders in @DbSync.Db.Schema.Core@) and dispatches to the
-- 'CopyWriter' queue for the appropriate table.
--
-- This is the only module that knows BOTH the typed record structure
-- AND the 'CopyWriter' queue interface. Adding a new extractor
-- (e.g. UTxO) means adding @writeTxOut@, @writeTxIn@ here and wiring
-- in the corresponding COPY encoders.
module DbSync.Writer.CopyAdapter
  ( -- * Construction
    mkCopyWriterAdapter
  ) where

import Cardano.Prelude

import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Db.Schema.Core
  ( encodeBlockCopy
  , encodeSlotLeaderCopy
  , encodeTxCopy
  )
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build a 'Writer IO' that encodes typed records and dispatches to
-- a 'CopyWriter'.
--
-- Each @write*@ function calls the corresponding COPY encoder from
-- @DbSync.Db.Schema.Core@ and then dispatches the resulting 'ByteString'
-- to the appropriate table's 'TBQueue' via 'cwWriteRow'.
mkCopyWriterAdapter :: CopyWriter -> Writer IO
mkCopyWriterAdapter cw = Writer
  { writeBlock = \bid blk ->
      cwWriteRow cw "block" (encodeBlockCopy bid blk)

  , writeTx = \tid tx ->
      cwWriteRow cw "tx" (encodeTxCopy tid tx)

  , writeSlotLeader = \slid sl ->
      cwWriteRow cw "slot_leader" (encodeSlotLeaderCopy slid sl)

  , commit = cwCommit cw
  }
