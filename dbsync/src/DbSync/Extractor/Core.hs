{-# LANGUAGE OverloadedStrings #-}

-- | Core extractor.
--
-- Extracts the fundamental tables: @block@, @tx@, and @slot_leader@.
-- This extractor is always enabled and cannot be disabled.
--
-- The extraction function is __pure__ — no IO, no database access.
-- It takes a 'GenericBlock' and 'ExtractState', and produces
-- COPY-encoded rows grouped by table name ('RowBatches') plus
-- an updated state with incremented ID counters and dedup maps.
module DbSync.Extractor.Core
  ( coreExtractor
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import DbSync.Block.Types (GenericBlock (..), GenericTx)
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , Tx (..)
  , blockTableDef
  , encodeBlockCopy
  , encodeSlotLeaderCopy
  , encodeTxCopy
  , slotLeaderTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Ids (BlockId (..), SlotLeaderId (..), TxId (..))
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..))
import DbSync.Extractor
  ( ExtractFn
  , ExtractState (..)
  , ExtractorDef (..)
  , RowBatches (..)
  )
import DbSync.Id.Counter (IdCounters (..), nextId)
import DbSync.Id.DedupMap (DedupMaps (..), lookupOrInsert)

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
  , pdExtract      = extractCore
  }

-- ---------------------------------------------------------------------------
-- * Extraction function
-- ---------------------------------------------------------------------------

-- | Pure extraction: 'GenericBlock' + 'ExtractState' -> ('RowBatches', 'ExtractState')
--
-- For each block:
--
--   1. Look up (or create) the slot leader in the dedup map
--   2. Assign a block ID from the counter
--   3. Build the 'Block' record and encode it for COPY
--   4. For each transaction, assign a tx ID and build the 'Tx' record
--   5. Return all COPY-encoded rows grouped by table name
extractCore :: ExtractFn
extractCore genBlock st =
  let
    -- 1. Slot leader dedup
    counters0 = esIdCounters st
    dedups0   = esDedupMaps st
    leaderHash = blkSlotLeader genBlock
    (slotLeaderIdRaw, isNew, updatedSlotLeaderMap) =
      lookupOrInsert leaderHash (dmsSlotLeader dedups0)
    slId = SlotLeaderId slotLeaderIdRaw
    dedups1 = dedups0 { dmsSlotLeader = updatedSlotLeaderMap }

    -- Slot leader row (only emitted if this is a new leader)
    slotLeaderRows
      | isNew     = [ encodeSlotLeaderCopy slId (mkSlotLeader genBlock) ]
      | otherwise = []

    -- 2. Assign block ID
    (blockIdRaw, blockCounter') = nextId (icBlockId counters0)
    blockId = BlockId blockIdRaw
    counters1 = counters0 { icBlockId = blockCounter' }

    -- 3. Build block record
    previousId = BlockId <$> esLastBlockId st
    block = mkBlock genBlock previousId slId
    blockRow = encodeBlockCopy blockId block

    -- 4. Build tx records
    (txRows, counters2) = extractTxs blockId (blkTxs genBlock) counters1

    -- 5. Assemble output
    batches = RowBatches $ Map.fromList
      [ ("block",       [blockRow])
      , ("tx",          txRows)
      , ("slot_leader", slotLeaderRows)
      ]

    st' = st
      { esIdCounters  = counters2
      , esDedupMaps   = dedups1
      , esLastBlockId = Just blockIdRaw
      }
  in
    (batches, st')

-- ---------------------------------------------------------------------------
-- * Internal helpers
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

-- | Generate a slot leader description from the raw hash.
-- Uses hex encoding of the first 8 bytes for a readable identifier.
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

-- | Extract 'Tx' rows from a list of 'GenericTx', assigning IDs.
extractTxs :: BlockId -> [G.GenericTx] -> IdCounters -> ([ByteString], IdCounters)
extractTxs blockId txs counters0 = foldl' go ([], counters0) txs
  where
    go :: ([ByteString], IdCounters) -> G.GenericTx -> ([ByteString], IdCounters)
    go (accRows, ctrs) gtx =
      let (txIdRaw, txCounter') = nextId (icTxId ctrs)
          txId = TxId txIdRaw
          ctrs' = ctrs { icTxId = txCounter' }
          tx = mkTx blockId gtx
          row = encodeTxCopy txId tx
      in (accRows ++ [row], ctrs')

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
