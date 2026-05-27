{-# LANGUAGE OverloadedStrings #-}

-- | Test helpers for property tests over arbitrary 'CardanoBlock' values.
--
-- The upstream @ouroboros-consensus:unstable-cardano-testlib@ provides
-- @Arbitrary (CardanoBlock StandardCrypto)@ and friends. The header on
-- @Test.Consensus.Cardano.Generators@ explicitly notes that generated
-- values are only intended for serialisation roundtrip tests — they are
-- CBOR-shape valid but not necessarily ledger-valid. That is fine for
-- the algebraic / shape invariants we assert here. Ledger-validity-
-- sensitive scenarios live in Slice 5's mock-driven tests.
--
-- == What's exposed
--
-- * 'runPureExtract' — drive a single 'CardanoBlock' through
--   @parseBlock + processBlock@ against 'mkTestWriter', returning the
--   accumulated 'TestWriterState'.
-- * 'runPureExtractMany' — same idea over a list of blocks, with
--   resolver/extract state carried across so 'BlockId' / 'TxId'
--   sequences are monotonic over the run.
-- * 'syntheticSlotDetails' — deterministic 'SlotDetails' from a
--   'SlotNo'. Time fields are derived from the slot number; epoch math
--   uses Byron-era epoch sizing (21600 slots) for stability across
--   eras. Property tests only assert on shape invariants, not on
--   timestamps, so this is fine.
-- * 'mkInitExtractState' — fresh 'ExtractState' with every @Id@ counter
--   seeded at 1.
module DbSync.Test.Property.Invariants
  ( -- * Pipeline runners
    runPureExtract
  , runPureExtractMany

    -- * Building blocks
  , syntheticSlotDetails
  ) where

import Cardano.Prelude

import Data.IORef (newIORef, readIORef)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- LedgerSupportsProtocol orphans

import DbSync.Block.Parser (parseBlock)
import DbSync.Extractor (ExtractorDef, freshExtractState)
import DbSync.Phase.Ingest.DedupMap (DedupMaps, newMaps)
import DbSync.Block.Pipeline (processBlock)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestUtxoStore)
import DbSync.StateQuery.Types (SlotDetails (..))
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState, emptyTestWriterState, mkTestWriter)

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Drive a single 'CardanoBlock' through the pure pipeline and
-- return the accumulated 'TestWriterState'.
runPureExtract
  :: [ExtractorDef]
  -> CardanoBlock StandardCrypto
  -> IO TestWriterState
runPureExtract extractors block =
  runPureExtractMany extractors [block]

-- | Drive a list of 'CardanoBlock's through the pure pipeline,
-- carrying resolver + extract state across so @BlockId@ / @TxId@
-- sequences are continuous. Returns the final 'TestWriterState'.
runPureExtractMany
  :: [ExtractorDef]
  -> [CardanoBlock StandardCrypto]
  -> IO TestWriterState
runPureExtractMany extractors blocks = withTestUtxoStore $ \utxoStore -> do
  stRef     <- newIORef freshExtractState
  dedupMaps <- newMaps :: IO DedupMaps
  addrBuf   <- newAddressBufferRef
  ref       <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf utxoStore Nothing)
                              (mkTestWriter ref) extractors
  for_ blocks $ \block -> do
    let sd        = syntheticSlotDetails (blockSlot block)
        !genBlock = parseBlock sd block
    runReaderT (processBlock genBlock) env
  readIORef ref

-- | Deterministic 'SlotDetails' derived from a 'SlotNo'. Time fields
-- are derived from the slot number (1 second per slot, epoch 0 at
-- POSIX 0). Epoch math uses Byron sizing (21600 slots) for stability.
-- Property tests should not assert on timestamp accuracy — only on
-- the shape invariants this helper preserves.
syntheticSlotDetails :: SlotNo -> SlotDetails
syntheticSlotDetails sn@(SlotNo s) = SlotDetails
  { sdSlotTime    = posixSecondsToUTCTime (fromIntegral s)
  , sdCurrentTime = posixSecondsToUTCTime (fromIntegral s)
  , sdEpochNo     = EpochNo (s `div` epochSizeWord)
  , sdSlotNo      = sn
  , sdEpochSlot   = s `mod` epochSizeWord
  , sdEpochSize   = EpochSize epochSizeWord
  }
  where
    epochSizeWord :: Word64
    epochSizeWord = 21600
