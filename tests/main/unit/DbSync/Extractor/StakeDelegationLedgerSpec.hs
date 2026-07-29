{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the StakeDelegationLedger extractor's boundary handler.
--
-- The catch-up slice attached to a boundary payload must drain into
-- @epoch_stake@ rows exactly like a per-block slice, including the
-- @epoch_stake_progress@ row on a final slice.
module DbSync.Extractor.StakeDelegationLedgerSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.AppM (runAppM)
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Db.Schema.StakeDelegation (EpochStake (..), EpochStakeProgress (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.StakeDelegationLedger (runStakeDelegationLedgerBoundary)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import DbSync.Worker.Ledger.Keys (PoolKeyHash, StakeCred)
import DbSync.Worker.Ledger.StakeDist (StakeSlice (..), StakeSliceRes (..))
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "runStakeDelegationLedgerBoundary — catch-up slice" $ do
    it "does nothing without a catch-up slice" $ do
      written <- runBoundary (mkBoundary NoSlices)
      twEpochStakes written `shouldBe` []
      twEpochStakeProgresses written `shouldBe` []

    it "drains a final catch-up slice into rows plus a progress marker" $ do
      let slice = StakeSlice
            { sliceEpochNo = EpochNo 13
            , sliceDistr   =
                [ (stakeCred 0xAA, (Coin 123, poolKey 0x01))
                , (stakeCred 0xBB, (Coin 456, poolKey 0x01))
                ]
            }
      written <- runBoundary (mkBoundary (Slice slice True))
      map epochStakeAmount (twEpochStakes written)
        `shouldBe` [DbLovelace 123, DbLovelace 456]
      map epochStakeEpochNo (twEpochStakes written) `shouldBe` [13, 13]
      twEpochStakeProgresses written
        `shouldBe` [EpochStakeProgress 13 True]

    it "emits no progress marker for a non-final slice" $ do
      let slice = StakeSlice
            { sliceEpochNo = EpochNo 13
            , sliceDistr   = [(stakeCred 0xAA, (Coin 123, poolKey 0x01))]
            }
      written <- runBoundary (mkBoundary (Slice slice False))
      length (twEpochStakes written) `shouldBe` 1
      twEpochStakeProgresses written `shouldBe` []

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Boundary payload carrying only the supplied catch-up slice; no
-- 'NewEpoch' means the reward path stays quiet.
mkBoundary :: StakeSliceRes -> BoundaryApplyData
mkBoundary catchup = BoundaryApplyData
  { bndNewEpoch          = Strict.Nothing
  , bndEvents            = []
  , bndGovActionState    = Nothing
  , bndGovExpiresAfter   = Strict.Nothing
  , bndSlotDetails       = dummySlotDetails
  , bndCatchupStakeSlice = catchup
  }

stakeCred :: Word8 -> StakeCred
stakeCred b =
  Ledger.KeyHashObj $ Ledger.KeyHash $
    fromMaybe (panic "stakeCred: bad hash size") (Crypto.hashFromBytes (BS.replicate 28 b))

poolKey :: Word8 -> PoolKeyHash
poolKey b =
  Ledger.KeyHash $
    fromMaybe (panic "poolKey: bad hash size") (Crypto.hashFromBytes (BS.replicate 28 b))

dummySlotDetails :: SlotDetails
dummySlotDetails = SlotDetails
  { sdSlotTime    = epochZero
  , sdCurrentTime = epochZero
  , sdEpochNo     = EpochNo 0
  , sdSlotNo      = SlotNo 0
  , sdEpochSlot   = 0
  , sdEpochSize   = EpochSize 21600
  }
  where
    epochZero :: UTCTime
    epochZero = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- ---------------------------------------------------------------------------
-- Harness
-- ---------------------------------------------------------------------------

-- | Run one boundary against a real 'mkIngestResolver' and a
-- 'TestWriter', capturing the written rows.
runBoundary :: BoundaryApplyData -> IO TestWriterState
runBoundary bad = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing
      writer   = mkTestWriter wrRef
      env      = mkTestPipelineEnv resolver writer []
  runAppM env (runStakeDelegationLedgerBoundary bad (BlockId 1))
  readIORef wrRef
