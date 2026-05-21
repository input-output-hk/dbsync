{-# LANGUAGE OverloadedStrings #-}

-- | Property tests asserting shape invariants of the pure extraction
-- pipeline (parseBlock + processBlock + mkTestWriter) over arbitrary
-- 'CardanoBlock' values across all eras.
--
-- Generated blocks come from @ouroboros-consensus:unstable-cardano-testlib@.
-- They are CBOR-shape valid but not ledger-valid — sufficient for
-- shape invariants, not for semantic checks.
--
-- Generator size is capped at 65 (mainnet P99 of txs/block). The
-- mainnet MAX (~385) is a rare outlier and out of scope here.
module DbSync.PropertySpec (spec) where

import Cardano.Prelude

import qualified Data.Set as Set

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import Test.QuickCheck (Property, ioProperty, (===))

import Test.Consensus.Cardano.Generators ()  -- Arbitrary (CardanoBlock StandardCrypto)

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import qualified DbSync.Block.Types as G
import DbSync.Block.Parser (parseBlock)
import qualified DbSync.Db.Schema.Core as SC
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Extractor (ExtractorDef)
import DbSync.Extractor.Core (coreExtractor, hasNoDepositActivity)
import DbSync.Test.Property.Invariants
  ( runPureExtract
  , runPureExtractMany
  , syntheticSlotDetails
  )
import DbSync.Test.Writer (TestWriterState (..))

spec :: Spec
spec = describe "DbSync.PropertySpec (Arbitrary CardanoBlock)" $
  -- size 65 = mainnet P99 txs/block.
  modifyMaxSize (const 65) $ do

    prop "processBlock is total: parseBlock + processBlock does not throw"
      prop_processBlockIsTotal

    prop "block count preserved: |twBlocks| == length input"
      prop_blockCountPreserved

    -- Runs each block twice; halve maxSuccess.
    modifyMaxSuccess (const 50) $
      prop "extract is deterministic: same input twice → same writer state"
        prop_extractIsDeterministic

    -- List property: quadratic cost. Smaller lists, fewer cases.
    modifyMaxSize (const 15) $ modifyMaxSuccess (const 30) $
      prop "block IDs are monotonic and gap-free across blocks"
        prop_blockIdsMonotonic

    prop "plain-transfer valid-contract txs get deposit = Just 0"
      prop_plainTransfersGetZeroDeposit

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The pipeline must not throw on any CBOR-shape valid 'CardanoBlock'.
-- Failure modes worth noticing: partial pattern matches in the parser,
-- 'undefined'/'panic' in an extractor, an extractor that assumes a
-- field that some era doesn't populate.
prop_processBlockIsTotal :: CardanoBlock StandardCrypto -> Property
prop_processBlockIsTotal block = ioProperty $ do
  _state <- runPureExtract enabledExtractors block
  pure True

-- | The pipeline is pure: same input → same output. Catches
-- non-determinism creeping in (e.g. unstable Map iteration in an
-- extractor, or a 'Show' instance using a 'TypeRep'-derived hash that
-- changes across runs).
prop_extractIsDeterministic :: CardanoBlock StandardCrypto -> Property
prop_extractIsDeterministic block = ioProperty $ do
  s1 <- runPureExtract enabledExtractors block
  s2 <- runPureExtract enabledExtractors block
  -- 'TestWriterState' has a 'Show' instance and we compare shapes via
  -- the row counts; a full structural Eq would be nicer but the
  -- record's row types currently lack 'Eq'. Counts cover the non-
  -- determinism failure modes we care about (number of rows produced
  -- from the same input must be stable).
  pure $ shape s1 == shape s2
  where
    shape :: TestWriterState -> (Int, Int, Int, Int)
    shape s =
      ( length (twBlocks s)
      , length (twTxs s)
      , length (twSlotLeaders s)
      , twCommits s
      )

-- | One block in → exactly one block row out, regardless of era or
-- contents. A failure here means an extractor is silently dropping
-- blocks, or the resolver is returning a stale 'BlockId'.
prop_blockCountPreserved :: CardanoBlock StandardCrypto -> Property
prop_blockCountPreserved block = ioProperty $ do
  s <- runPureExtract enabledExtractors block
  pure $ length (twBlocks s) === 1

-- | Across @N@ blocks the resolver must hand out 'BlockId' 1, 2, ..., N
-- in order. Catches resolver / counter bugs (skipped IDs, repeated IDs,
-- IDs allocated from the wrong counter).
--
-- Uses @[CardanoBlock]@ rather than a single block so the property
-- exercises continuity across the resolver state.
prop_blockIdsMonotonic :: [CardanoBlock StandardCrypto] -> Property
prop_blockIdsMonotonic blocks = ioProperty $ do
  s <- runPureExtractMany enabledExtractors blocks
  -- 'twBlocks' is in insertion order (oldest first) because
  -- 'mkTestWriter' appends with @++@.
  let actualIds :: [Int]
      actualIds = map (fromIntegral . unBlockId . fst) (twBlocks s)
      expected  = [1 .. length blocks]
  pure $ actualIds === expected
  where
    unBlockId :: BlockId -> Int64
    unBlockId (BlockId n) = n

-- | Every valid-contract tx with no certs, withdrawals or treasury
-- donation receives @txDeposit = Just 0@ from the Ingest pipeline
-- — the conservation short-circuit in
-- 'Extractor.Core.computeTxFinancials' fires before any deposit
-- lookup, so the post-load backfill never sees these rows.
prop_plainTransfersGetZeroDeposit
  :: CardanoBlock StandardCrypto -> Property
prop_plainTransfersGetZeroDeposit block = ioProperty $ do
  let sd       = syntheticSlotDetails (blockSlot block)
      genBlock = parseBlock sd block
      plainTxHashes = Set.fromList
        [ G.txHash gtx
        | gtx <- G.blkTxs genBlock
        , G.txValidContract gtx
        , hasNoDepositActivity gtx
        ]
  s <- runPureExtract enabledExtractors block
  let offenders =
        [ (SC.txHash t, SC.txDeposit t)
        | (_, t) <- twTxs s
        , SC.txHash t `Set.member` plainTxHashes
        , SC.txDeposit t /= Just 0
        ]
  pure $ offenders === []

-- ---------------------------------------------------------------------------
-- Pipeline configuration
-- ---------------------------------------------------------------------------

-- | The extractor set under test. Stick to the 'core' extractor
-- (block, tx, slot_leader) for the initial property suite — adding
-- more extractors later only widens the surface, never narrows what's
-- covered.
enabledExtractors :: [ExtractorDef]
enabledExtractors = [coreExtractor]
