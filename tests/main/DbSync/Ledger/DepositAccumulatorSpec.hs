{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the per-epoch deposit-param accumulator.
--
-- The IO entry points ('recordEpochParams', 'drainCompletedEpochs',
-- 'takeAllEpochs') are thin wrappers around the pure helpers
-- ('insertParams', 'partitionCompleted'); driving the pure helpers
-- directly keeps the suite IORef-free and makes the partition /
-- merge semantics explicit.
module DbSync.Ledger.DepositAccumulatorSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Types (DbLovelace (..))
import DbSync.Ledger.DepositAccumulator
  ( EpochParams (..)
  , depositColumnVectors
  , insertParams
  , partitionCompleted
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkParams :: Word64 -> Word64 -> EpochParams
mkParams sk pl =
  EpochParams (DbLovelace sk) (DbLovelace pl)

epochA, epochB, epochC :: EpochNo
epochA = EpochNo 100
epochB = EpochNo 101
epochC = EpochNo 102

paramsA, paramsB, paramsC :: EpochParams
paramsA = mkParams 2_000_000 500_000_000
paramsB = mkParams 2_000_000 500_000_000
paramsC = mkParams 2_000_000 500_000_000

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  insertParamsSpec
  partitionCompletedSpec
  depositColumnVectorsSpec

insertParamsSpec :: Spec
insertParamsSpec = describe "insertParams" $ do

  it "inserts a new epoch into an empty map" $ do
    let m = insertParams epochA paramsA Map.empty
    Map.toList m `shouldBe` [(epochA, paramsA)]

  it "overwrites an existing entry for the same epoch" $ do
    let updated = mkParams 9_000_000 9_000_000_000
        m = insertParams epochA updated (Map.singleton epochA paramsA)
    Map.lookup epochA m `shouldBe` Just updated

  it "preserves entries for other epochs" $ do
    let m0 = Map.fromList [(epochA, paramsA), (epochC, paramsC)]
        m1 = insertParams epochB paramsB m0
    Map.keys m1 `shouldBe` [epochA, epochB, epochC]

partitionCompletedSpec :: Spec
partitionCompletedSpec = describe "partitionCompleted" $ do

  it "returns an empty toFlush set when the map is empty" $ do
    let (remaining, toFlush) = partitionCompleted epochB Map.empty
    Map.null remaining `shouldBe` True
    Map.null toFlush   `shouldBe` True

  it "drains every entry at or before the watermark" $ do
    let m0 = Map.fromList [(epochA, paramsA), (epochB, paramsB), (epochC, paramsC)]
        (remaining, toFlush) = partitionCompleted epochB m0
    Map.keys toFlush  `shouldBe` [epochA, epochB]
    Map.keys remaining `shouldBe` [epochC]

  it "leaves no entries behind when the watermark covers everything" $ do
    let m0 = Map.fromList [(epochA, paramsA), (epochB, paramsB)]
        (remaining, toFlush) = partitionCompleted epochC m0
    Map.keys toFlush   `shouldBe` [epochA, epochB]
    Map.null remaining `shouldBe` True

  it "drains nothing when every entry is past the watermark" $ do
    let m0 = Map.fromList [(epochB, paramsB), (epochC, paramsC)]
        (remaining, toFlush) = partitionCompleted epochA m0
    Map.null toFlush   `shouldBe` True
    Map.keys remaining `shouldBe` [epochB, epochC]

  -- The 'atomicModifyIORef'' contract is @(new, result)@; the helper
  -- has to return @(remaining, toFlush)@ in that order so the IORef
  -- keeps the in-progress epochs and the caller receives the drained
  -- set.
  it "returns the remaining map first (matches atomicModifyIORef')" $ do
    let m0 = Map.singleton epochA paramsA
        (newState, result) = partitionCompleted epochA m0
    Map.null newState `shouldBe` True
    Map.keys result   `shouldBe` [epochA]

depositColumnVectorsSpec :: Spec
depositColumnVectorsSpec = describe "depositColumnVectors" $ do

  it "produces empty vectors for an empty map" $
    depositColumnVectors Map.empty `shouldBe` ([], [], [])

  it "emits parallel arrays in ascending epoch order" $ do
    let p1 = mkParams 1 11
        p2 = mkParams 2 22
        p3 = mkParams 3 33
        m  = Map.fromList [(EpochNo 5, p3), (EpochNo 3, p1), (EpochNo 4, p2)]
    depositColumnVectors m
      `shouldBe`
        ( [3, 4, 5]
        , [DbLovelace 1, DbLovelace 2, DbLovelace 3]
        , [DbLovelace 11, DbLovelace 22, DbLovelace 33]
        )
