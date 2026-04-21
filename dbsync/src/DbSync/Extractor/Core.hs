{-# LANGUAGE OverloadedStrings #-}

-- | Core extractor.
--
-- Extracts the fundamental tables: @block@, @tx@, and @slot_leader@.
-- This extractor is always enabled and cannot be disabled.
--
-- The extraction logic uses the 'IdResolver' for ID assignment and
-- the 'Writer' for row output, so the same code works in both
-- 'IngestChainHistory' (COPY + DedupMaps) and 'FollowingChainTip'
-- (INSERT + DB queries).
module DbSync.Extractor.Core
  ( coreExtractor

    -- * Internal helpers (exported for testing)
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
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn)
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
-- 1. Resolve the slot leader (dedup lookup-or-insert)
-- 2. If new slot leader, write the SlotLeader row
-- 3. Assign a BlockId
-- 4. Write the Block row
-- 5. For each transaction, assign TxId and write Tx row
processCore :: ProcessBlockFn
processCore resolver writer genBlock = do
  -- 1. Slot leader resolution
  let leaderHash = blkSlotLeader genBlock
      leader = mkSlotLeader genBlock
  (slId, isNew) <- resolveSlotLeader resolver leaderHash leader

  -- 2. Write slot leader row if new
  when isNew $
    writeSlotLeader writer slId leader

  -- 3. Resolve previous block and assign block ID
  previousId <- resolvePrevBlock resolver (blkPreviousHash genBlock)
  blockId <- assignBlockId resolver

  -- 4. Build and write block
  let block = mkBlock genBlock previousId slId
  writeBlock writer blockId block

  -- 5. Process transactions
  forM_ (blkTxs genBlock) $ \gtx -> do
    txId <- assignTxId resolver
    let tx = mkTx blockId gtx
    writeTx writer txId tx

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
