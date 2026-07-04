-- | Unit tests for 'DbSync.Worker.Ledger.Types'.
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
module DbSync.Worker.Ledger.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))

import qualified Data.Map.Lazy as LMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.Types
  ( BlockApplyData (..)
  , DepositsMap (..)
  , EpochBlockNo (..)
  , LedgerDB (..)
  , ProposedCommitteeMember (..)
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
      let m = DepositsMap (Map.fromList [("some-hash", Coin 2_000_000)])
      lookupDepositsMap "some-hash" m `shouldBe` Just (Coin 2_000_000)

    it "lookupDepositsMap returns Nothing for a missing hash" $ do
      let m = DepositsMap (Map.fromList [("a", Coin 1), ("b", Coin 2)])
      lookupDepositsMap "c" m `shouldBe` Nothing

  describe "NFData forcing reaches the leaves" $ do
    -- Queued payloads rely on 'force' making them self-contained; a
    -- value-lazy map slipping through would pin its closures for a
    -- whole epoch. Data.Map.Lazy builds the same 'Map' type without
    -- forcing values, standing in for any lazily-produced entry.
    it "DepositsMap: a thunked deposit value explodes under force" $
      evaluate (force (DepositsMap (LMap.singleton "h" (panic "unforced deposit"))))
        `shouldThrow` anyException

    it "DepositsMap: a fully-evaluated map survives force intact" $ do
      m <- evaluate $ force $ DepositsMap (Map.fromList [("some-hash", Coin 2_000_000)])
      lookupDepositsMap "some-hash" m `shouldBe` Just (Coin 2_000_000)

    it "BlockApplyData: a thunked committee member explodes under force" $
      let bomb =
            BlockApplyData
              { badDepositsMap      = emptyDepositsMap
              , badStakeSlice       = Generic.NoSlices
              , badPoolsRegistered  = Set.empty
              , badGovExpiresAfter  = Strict.Nothing
              , badStakeKeyDeposit  = Strict.Nothing
              , badPoolDeposit      = Strict.Nothing
              , badCommitteeMembers =
                  Map.singleton ("tx", 0)
                    [ProposedCommitteeMember (panic "unforced cold key") False 0]
              }
       in evaluate (force bomb) `shouldThrow` anyException

  describe "EpochBlockNo" $ do
    it "EpochBlockNo is ordered by its Word64 payload" $ do
      compare (EpochBlockNo 1) (EpochBlockNo 2) `shouldBe` LT
      compare (EpochBlockNo 5) (EpochBlockNo 5) `shouldBe` EQ
      compare (EpochBlockNo 7) (EpochBlockNo 3) `shouldBe` GT

    it "ByronEpochBlockNo is distinct from EpochBlockNo 0" $
      (ByronEpochBlockNo == EpochBlockNo 0) `shouldBe` False
