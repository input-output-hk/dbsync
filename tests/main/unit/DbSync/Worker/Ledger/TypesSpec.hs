-- | Strictness bombs for the ledger-worker payload types.
--
-- Queued payloads rely on 'force' making them self-contained before
-- they cross the worker boundary; these tests feed bottoms through
-- the hand-written 'NFData' instances and expect a throw.
module DbSync.Worker.Ledger.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import qualified Data.Map.Lazy as LMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import DbSync.StateQuery (SlotDetails (..))
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.Types
  ( BlockApplyData (..)
  , BoundaryApplyData (..)
  , DepositsMap (..)
  , ProposedCommitteeMember (..)
  , emptyDepositsMap
  , lookupDepositsMap
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  describe "NFData forcing reaches the leaves" $ do
    -- A value-lazy map slipping through would pin its closures for a
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
              , badPrices           = Strict.Nothing
              , badCommitteeMembers =
                  Map.singleton ("tx", 0)
                    [ProposedCommitteeMember (panic "unforced cold key") False 0]
              }
       in evaluate (force bomb) `shouldThrow` anyException

    it "BlockApplyData: a thunked prices value explodes under force" $
      let bomb =
            BlockApplyData
              { badDepositsMap      = emptyDepositsMap
              , badStakeSlice       = Generic.NoSlices
              , badPoolsRegistered  = Set.empty
              , badGovExpiresAfter  = Strict.Nothing
              , badStakeKeyDeposit  = Strict.Nothing
              , badPoolDeposit      = Strict.Nothing
              , badPrices           = Strict.Just (panic "unforced prices")
              , badCommitteeMembers = Map.empty
              }
       in evaluate (force bomb) `shouldThrow` anyException

    it "BoundaryApplyData: a thunked catch-up stake entry explodes under force" $
      let bomb =
            BoundaryApplyData
              { bndNewEpoch          = Strict.Nothing
              , bndEvents            = []
              , bndGovActionState    = Nothing
              , bndGovExpiresAfter   = Strict.Nothing
              , bndSlotDetails       = dummySlotDetails
              , bndCatchupStakeSlice =
                  Generic.Slice
                    (Generic.StakeSlice (EpochNo 1) [panic "unforced stake entry"])
                    True
              }
       in evaluate (force bomb) `shouldThrow` anyException

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
