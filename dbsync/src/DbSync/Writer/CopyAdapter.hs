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

    -- Transaction control
  , commit = cwCommit cw
  }
