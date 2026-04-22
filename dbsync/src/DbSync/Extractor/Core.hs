{-# LANGUAGE OverloadedStrings #-}

-- | Core extractor.
--
-- Extracts the fundamental tables: @block@, @tx@, and @slot_leader@.
-- This extractor is always enabled and cannot be disabled.
--
-- Uses pre-assigned IDs from 'BlockContext' — it does NOT call
-- 'assignBlockId' or 'assignTxId' itself. Those are assigned
-- centrally by 'processBlock'.
module DbSync.Extractor.Core
  ( coreExtractor

    -- * Internal helpers (exported for testing and Pipeline)
  , mkBlock
  , mkTx
  , mkSlotLeader
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.ByteString as BS

import DbSync.Block.Types (GenericBlock (..))
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , Tx (..)
  , blockTableDef
  , slotLeaderTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Ids (BlockId (..), SlotLeaderId, TxId)
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

-- | The core extractor definition.
--
-- Produces rows for the @block@, @tx@, and @slot_leader@ tables.
-- Always enabled, no dependencies on other extractors.
coreExtractor :: ExtractorDef
coreExtractor = ExtractorDef
  { pdName         = "core"
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = [blockTableDef, txTableDef, slotLeaderTableDef]
  , pdProcess      = processCore
  }

-- ---------------------------------------------------------------------------
-- * Processing function
-- ---------------------------------------------------------------------------

-- | Process a single block through the core extractor.
--
-- Uses pre-assigned IDs from BlockContext:
-- 1. Write slot leader row if new
-- 2. Write block row
-- 3. Write tx rows
processCore :: ProcessBlockFn
processCore _resolver writer ctx = do
  let gb = bcGenBlock ctx
      blockId = bcBlockId ctx
      slId = bcSlotLeaderId ctx

  -- 1. Write slot leader row if new
  when (bcSlotLeaderNew ctx) $
    writeSlotLeader writer slId (mkSlotLeader gb)

  -- 2. Write block
  let block = mkBlock gb (bcPrevBlockId ctx) slId
  writeBlock writer blockId block

  -- 3. Write transactions
  forM_ (bcTxs ctx) $ \tc -> do
    let tx = mkTx blockId (tcGenTx tc)
    writeTx writer (tcTxId tc) tx

-- ---------------------------------------------------------------------------
-- * Record builders (pure, shared across phases)
-- ---------------------------------------------------------------------------

-- | Build a 'Block' record from a 'GenericBlock'.
mkBlock :: GenericBlock -> Maybe BlockId -> SlotLeaderId -> Block
mkBlock gb prevId slId = Block
  { blockHash          = blkHash gb
  , blockEpochNo       = Just (unEpochNo $ blkEpochNo gb)
  , blockSlotNo        = Just (unSlotNo $ blkSlotNo gb)
  , blockEpochSlotNo   = Just (blkEpochSlotNo gb)
  , blockBlockNo       = Just (unBlockNo $ blkBlockNo gb)
  , blockPreviousId    = prevId
  , blockSlotLeaderId  = slId
  , blockSize          = blkSize gb
  , blockTime          = blkTime gb
  , blockTxCount       = fromIntegral (length (blkTxs gb))
  , blockProtoMajor    = blkProtoMajor gb
  , blockProtoMinor    = blkProtoMinor gb
  , blockVrfKey        = blkVrfKey gb
  , blockOpCert        = blkOpCert gb
  , blockOpCertCounter = blkOpCertCounter gb
  }

-- | Build a 'SlotLeader' record.
-- Pool hash resolution is deferred — 'slotLeaderPoolHashId' is 'Nothing'.
mkSlotLeader :: GenericBlock -> SlotLeader
mkSlotLeader gb = SlotLeader
  { slotLeaderHash        = blkSlotLeader gb
  , slotLeaderPoolHashId  = Nothing
  , slotLeaderDescription = mkSlotLeaderDesc (blkSlotLeader gb)
  }

-- | Build a 'Tx' record from a 'GenericTx'.
mkTx :: BlockId -> G.GenericTx -> Tx
mkTx blkId gtx = Tx
  { txHash             = G.txHash gtx
  , txBlockId          = blkId
  , txBlockIndex       = G.txBlockIndex gtx
  , txOutSum           = DbLovelace (G.txOutSum gtx)
  , txFee              = DbLovelace (G.txFee gtx)
  , txDeposit          = Nothing  -- requires ledger state
  , txSize             = G.txSize gtx
  , txInvalidBefore    = DbWord64 <$> G.txInvalidBefore gtx
  , txInvalidHereafter = DbWord64 <$> G.txInvalidHereafter gtx
  , txValidContract    = G.txValidContract gtx
  , txScriptSize       = G.txScriptSize gtx
  , txTreasuryDonation = DbLovelace (G.txTreasuryDonation gtx)
  }

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Generate a slot leader description from the raw hash.
mkSlotLeaderDesc :: ByteString -> Text
mkSlotLeaderDesc hash =
  "Pool-" <> shortHash
  where
    shortHash = toS @[Char] @Text $ concatMap hexByte (take 8 $ BS.unpack hash)
    hexByte :: Word8 -> [Char]
    hexByte w =
      let hi = w `div` 16
          lo = w `mod` 16
      in [hexDigit hi, hexDigit lo]
    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
