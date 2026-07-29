{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the PoolStats extractor's boundary handler.
--
-- 'runPoolStatsBoundary' must emit one row per pool in the union of
-- the active-stake map, the blocks-made map, and the DRep snapshot's
-- SPO voting distribution — zero-filling the maps a pool is absent
-- from — so registered-but-inactive pools still get a row.
module DbSync.Extractor.PoolStatsSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import qualified Cardano.Ledger.Conway.Governance as Gov
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.Default (def)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Ouroboros.Consensus.Cardano.Block (ConwayEra)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.AppM (runAppM)
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Db.Schema.Pool (PoolStat (..))
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.PoolStats (runPoolStatsBoundary)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import DbSync.Worker.Ledger.Keys (PoolKeyHash)
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "runPoolStatsBoundary — no-op cases" $ do
    it "does nothing on a mid-epoch block (no NewEpoch)" $ do
      written <- runBoundary True (mkBoundary Strict.Nothing)
      twPoolStats written `shouldBe` []

    it "does nothing on a Byron boundary (no pool distr)" $ do
      written <- runBoundary True $
        mkBoundary (Strict.Just (mkNewEpoch Strict.Nothing Strict.Nothing))
      twPoolStats written `shouldBe` []

  describe "runPoolStatsBoundary — row set" $ do
    it "emits the union of stake, blocks and SPO-voting pools" $ do
      written <- runBoundary True (mkBoundary (Strict.Just unionEpoch))
      case twPoolStats written of
        [stakeOnly, blocksOnly, votingOnly] -> do
          -- pool 0x01: active stake, no blocks, not in SPO distr
          poolStatEpochNo stakeOnly            `shouldBe` 300
          poolStatStake stakeOnly              `shouldBe` DbWord64 7_000_000
          poolStatNumberOfDelegators stakeOnly `shouldBe` DbWord64 3
          poolStatNumberOfBlocks stakeOnly     `shouldBe` DbWord64 0
          poolStatVotingPower stakeOnly        `shouldBe` Nothing
          -- pool 0x02: made blocks, zero stake
          poolStatStake blocksOnly              `shouldBe` DbWord64 0
          poolStatNumberOfDelegators blocksOnly `shouldBe` DbWord64 0
          poolStatNumberOfBlocks blocksOnly     `shouldBe` DbWord64 21
          poolStatVotingPower blocksOnly        `shouldBe` Nothing
          -- pool 0x03: only in the DRep snapshot's SPO distribution
          poolStatStake votingOnly              `shouldBe` DbWord64 0
          poolStatNumberOfBlocks votingOnly     `shouldBe` DbWord64 0
          poolStatVotingPower votingOnly        `shouldBe` Just (DbWord64 9_000_000)
        rows -> panic ("expected 3 pool_stat rows, got " <> show (length rows))

    it "merges all three sources into one row for the same pool" $ do
      let ne = mkNewEpoch
            (Strict.Just
              ( Map.singleton (poolKey 0x01) (Coin 5_000_000, 2)
              , Map.singleton (poolKey 0x01) 4
              ))
            (Strict.Just (spoSnapshot (Map.singleton (poolKey 0x01) (CompactCoin 6_000_000))))
      written <- runBoundary True (mkBoundary (Strict.Just ne))
      case twPoolStats written of
        [row] -> do
          poolStatStake row              `shouldBe` DbWord64 5_000_000
          poolStatNumberOfDelegators row `shouldBe` DbWord64 2
          poolStatNumberOfBlocks row     `shouldBe` DbWord64 4
          poolStatVotingPower row        `shouldBe` Just (DbWord64 6_000_000)
        rows -> panic ("expected 1 pool_stat row, got " <> show (length rows))

  describe "runPoolStatsBoundary — governance gating" $ do
    it "with governance off, SPO-voting-only pools get no row and voting_power is NULL" $ do
      written <- runBoundary False (mkBoundary (Strict.Just unionEpoch))
      length (twPoolStats written) `shouldBe` 2
      map poolStatVotingPower (twPoolStats written) `shouldBe` [Nothing, Nothing]

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Boundary payload with everything empty except the supplied epoch.
mkBoundary :: Strict.Maybe Generic.NewEpoch -> BoundaryApplyData
mkBoundary ne = BoundaryApplyData
  { bndNewEpoch        = ne
  , bndEvents          = []
  , bndGovActionState  = Nothing
  , bndGovExpiresAfter = Strict.Nothing
  , bndSlotDetails     = dummySlotDetails
  }

-- | Epoch-300 crossing with three disjoint pools: 0x01 has active
-- stake, 0x02 made blocks, 0x03 appears only in the SPO voting
-- distribution.
unionEpoch :: Generic.NewEpoch
unionEpoch = mkNewEpoch
  (Strict.Just
    ( Map.singleton (poolKey 0x01) (Coin 7_000_000, 3)
    , Map.singleton (poolKey 0x02) 21
    ))
  (Strict.Just (spoSnapshot (Map.singleton (poolKey 0x03) (CompactCoin 9_000_000))))

mkNewEpoch
  :: Strict.Maybe (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural)
  -> Strict.Maybe (Gov.DRepPulsingState ConwayEra)
  -> Generic.NewEpoch
mkNewEpoch poolDistr drepState = Generic.NewEpoch
  { Generic.neEpoch       = EpochNo 300
  , Generic.neIsEBB       = False
  , Generic.neAdaPots     = Strict.Nothing
  , Generic.neEpochUpdate = Generic.EpochUpdate
      { Generic.euProtoParams = Strict.Nothing
      , Generic.euNonce       = Ledger.NeutralNonce
      }
  , Generic.neDRepState   = drepState
  , Generic.neEnacted     = Strict.Nothing
  , Generic.nePoolDistr   = poolDistr
  }

-- | A completed pulser whose snapshot carries the supplied SPO
-- distribution and nothing else.
spoSnapshot :: Map PoolKeyHash (CompactForm Coin) -> Gov.DRepPulsingState ConwayEra
spoSnapshot spo = Gov.DRComplete (def { Gov.psPoolDistr = spo }) def

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
runBoundary :: Bool -> BoundaryApplyData -> IO TestWriterState
runBoundary governanceOn bad = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing
      writer   = mkTestWriter wrRef
      env      = mkTestPipelineEnv resolver writer []
  runAppM env (runPoolStatsBoundary governanceOn bad (BlockId 1))
  readIORef wrRef
