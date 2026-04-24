-- | Unit tests for 'DbSync.Ledger.Types'.
--
-- Structural checks on the ledger types. The test suite does
-- __not__ try to construct a full 'NoLedgerEnv' because its
-- 'nleProtocolInfo' field requires real genesis data, and its strict
-- bang means we can't sneak a bottom past the constructor. A
-- \"construct NoLedgerEnv and assert fields\" test will be added once
-- the boot flow has fixtures for 'Consensus.ProtocolInfo' (genesis
-- JSON + test @NodeConfig@).
--
-- A 'NoThunks' assertion is also deferred: it needs a 'NoThunks'
-- instance on 'DbSyncStateRef', which cascades into instances for
-- consensus @LedgerTablesHandle@ + @StrictTVar@. That will arrive
-- alongside the actual 'LedgerWorker'.
module DbSync.Ledger.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Ledger.Types
  ( DepositsMap (..)
  , EpochBlockNo (..)
  , LedgerDB (..)
  , emptyDepositsMap
  , lookupDepositsMap
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "LedgerDB" $
    it "ledgerDbCheckpoints starts empty for an empty LedgerDB" $
      Seq.length (ledgerDbCheckpoints (LedgerDB Seq.empty)) `shouldBe` 0

  describe "DepositsMap" $ do
    it "emptyDepositsMap has no entries" $
      lookupDepositsMap "any-hash" emptyDepositsMap `shouldBe` Nothing

    it "lookupDepositsMap round-trips a single entry" $ do
      let m = DepositsMap (Map.fromList [("some-hash", 2_000_000)])
      lookupDepositsMap "some-hash" m `shouldBe` Just 2_000_000

    it "lookupDepositsMap returns Nothing for a missing hash" $ do
      let m = DepositsMap (Map.fromList [("a", 1), ("b", 2)])
      lookupDepositsMap "c" m `shouldBe` Nothing

  describe "EpochBlockNo" $ do
    it "EpochBlockNo is ordered by its Word64 payload" $ do
      compare (EpochBlockNo 1) (EpochBlockNo 2) `shouldBe` LT
      compare (EpochBlockNo 5) (EpochBlockNo 5) `shouldBe` EQ
      compare (EpochBlockNo 7) (EpochBlockNo 3) `shouldBe` GT

    it "ByronEpochBlockNo is distinct from EpochBlockNo 0" $
      (ByronEpochBlockNo == EpochBlockNo 0) `shouldBe` False
