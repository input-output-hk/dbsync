{-# LANGUAGE OverloadedStrings #-}

-- | Hand-rolled 'GenericBlock' fixtures shared between specs that
-- exercise the post-load pass and the backfill SQL.
--
-- These are minimal, era-aware blocks designed to exercise every
-- backfill code path with the fewest rows possible:
--
--   * @producerBlock@ — a Shelley block with one tx that produces
--     three outputs at known lovelace values. Acts as the spend
--     source for the next two blocks.
--   * @spendingBlock@ — a Shelley block with two txs: a valid
--     contract that consumes @producer.0@, and a phase-2 failure
--     with collateral input from @producer.1@ and a collateral
--     return output. Drives the phase-2 fee + phase-2 deposit +
--     valid-contract deposit backfills.
--   * @byronBlock@ — a Byron-era block (@proto_major = 1@) with
--     one tx that spends @producer.2@. Drives the Byron fee
--     backfill, which is gated on the block's @proto_major < 2@
--     filter.
--   * @withdrawalBlock@ — a Shelley block carrying a single
--     withdrawal-only tx that spends @producer.3@. Exercises the
--     conservation short-circuit: the extractor writes
--     @tx.deposit = 0@ at parse time so the backfill never sees the
--     row.
--
-- Using these rather than 'DbSync.Test.MockChain' is a deliberate
-- choice: the chaingen interpreter is Conway-only and can't easily
-- forge a Byron block or a Plutus-failing tx without elaborate
-- script setup. The trade-off is that these fixtures don't
-- exercise real ledger transitions; tests that need those use
-- 'MockChain' instead.
module DbSync.Test.Fixtures
  ( producerBlock
  , spendingBlock
  , byronBlock
  , withdrawalBlock
  , producerHash
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS

import DbSync.Block.Types
  ( BlockEra (..)
  , CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxWithdrawal (..)
  )

-- ---------------------------------------------------------------------------
-- * Shared sample values
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | Shelley-shaped raw address. Header byte 0x00 (BasePaymentKey
-- StakeKey) + 56 padding bytes — well-formed enough for the UTxO
-- extractor's stake-credential parser.
sampleAddrRaw :: ByteString
sampleAddrRaw = BS.pack (0x00 : replicate 56 0x11)

-- | Pad a short ByteString to 32 bytes — the canonical hash length
-- the parser hands the rest of the pipeline.
padHash32 :: ByteString -> ByteString
padHash32 bs = bs <> BS.replicate (max 0 (32 - BS.length bs)) 0

-- | Common output shape used in every fixture.
mkOut :: Word16 -> Word64 -> GenericTxOut
mkOut idx value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddress     = "addr_test1xyz"
  , txOutAddressRaw  = sampleAddrRaw
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

-- | An empty Shelley GenericTx. Field shapes mirror what the block
-- parser would hand the extractor; individual fixtures override
-- only the fields that matter to their scenario.
emptyTx :: ByteString -> GenericTx
emptyTx hash = GenericTx
  { txHash             = hash
  , txBlockIndex       = 0
  , txSize             = 200
  , txFee              = 0
  , txOutSum           = 0
  , txValidContract    = True
  , txScriptSize       = 0
  , txTreasuryDonation = 0
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txInputs           = []
  , txOutputs          = []
  , txCollateralInputs = []
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  , txCertificates     = []
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  }

-- ---------------------------------------------------------------------------
-- * Producer block
-- ---------------------------------------------------------------------------

-- | Tx hash of the producer. Other fixtures reference this when
-- declaring inputs they want to spend.
producerHash :: ByteString
producerHash = padHash32 "PROD"

producerTx :: GenericTx
producerTx = (emptyTx producerHash)
  { txBlockIndex = 0
  , txSize       = 100
  , txFee        = 170000
  , txOutSum     = 12100000
  , txOutputs    =
      [ mkOut 0 5000000
      , mkOut 1 5000000
      , mkOut 2 2000000
      , mkOut 3  100000
      ]
  }

-- | Shelley block carrying just the producer.
producerBlock :: GenericBlock
producerBlock = GenericBlock
  { blkEra           = Shelley
  , blkHash          = padHash32 "BLK1"
  , blkPreviousHash  = ""
  , blkSlotNo        = SlotNo 100
  , blkBlockNo       = BlockNo 1
  , blkEpochNo       = EpochNo 0
  , blkEpochSlotNo   = 100
  , blkSize          = 512
  , blkTime          = sampleTime
  , blkSlotLeader    = BS.replicate 28 0xab
  , blkProtoMajor    = 9
  , blkProtoMinor    = 0
  , blkVrfKey        = Just "vrf_vk1test"
  , blkOpCert        = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB         = False
  , blkTxs           = [producerTx]
  }

-- ---------------------------------------------------------------------------
-- * Spending block (valid + phase-2)
-- ---------------------------------------------------------------------------

-- | Valid-contract consumer. Spends @(producerHash, 0)@ for a
-- 5_000_000 input; one output for 4_500_000; fee 200_000. Carries a
-- stake-registration cert so the deposit identity-formula backfill
-- targets the row (plain transfers ship with @0@ at parse time and
-- bypass the SQL). The fallback computes
-- @5_000_000 - 4_500_000 - 200_000 - 0 = 300_000@.
consumerTx :: GenericTx
consumerTx = (emptyTx (padHash32 "VALID"))
  { txBlockIndex   = 0
  , txSize         = 200
  , txFee          = 200000
  , txOutSum       = 4500000
  , txInputs       = [GenericTxIn producerHash 0]
  , txOutputs      = [mkOut 0 4500000]
  , txCertificates =
      [ GenericTxCertificate
          { txCertIndex  = 0
          , txCertAction = CertStakeRegistration (BS.replicate 28 0xee) Nothing
          }
      ]
  }

-- | Phase-2 failure. Mirrors what the parser writes after the
-- isValid check: @txFee = 0@ sentinel, no inputs/outputs/withdrawals,
-- just the collateral input and return.
phase2Tx :: GenericTx
phase2Tx = (emptyTx (padHash32 "FAIL"))
  { txBlockIndex       = 1
  , txSize             = 300
  , txValidContract    = False
  , txCollateralInputs = [GenericTxIn producerHash 1]
  , txCollateralOutput = Just (mkOut 0 2000000)
  }

-- | Block carrying the consumer and the phase-2 failure.
spendingBlock :: GenericBlock
spendingBlock = producerBlock
  { blkHash         = padHash32 "BLK2"
  , blkPreviousHash = blkHash producerBlock
  , blkSlotNo       = SlotNo 120
  , blkBlockNo      = BlockNo 2
  , blkEpochSlotNo  = 120
  , blkTxs          = [consumerTx, phase2Tx]
  }

-- ---------------------------------------------------------------------------
-- * Byron block
-- ---------------------------------------------------------------------------

-- | A Byron-era tx spending @producer.2@. Fee is the @0@ sentinel;
-- the post-load Byron fee backfill should replace it with the
-- @inputs - outputs@ difference (500_000).
byronTx :: GenericTx
byronTx = (emptyTx (padHash32 "BYRON"))
  { txBlockIndex = 0
  , txSize       = 150
  , txOutSum     = 1500000
  , txInputs     = [GenericTxIn producerHash 2]
  , txOutputs    = [mkOut 0 1500000]
  }

-- | Byron block. @proto_major < 2@ is what the Byron fee backfill
-- gates on; the slot leader differs so the dedup map allocates a
-- new @slot_leader@ row, and Byron blocks deliberately skip the
-- @pool_hash@ write.
byronBlock :: GenericBlock
byronBlock = producerBlock
  { blkEra           = Byron
  , blkHash          = padHash32 "BLK3"
  , blkPreviousHash  = blkHash spendingBlock
  , blkSlotNo        = SlotNo 200
  , blkBlockNo       = BlockNo 3
  , blkEpochSlotNo   = 200
  , blkSlotLeader    = BS.replicate 28 0xcd
  , blkProtoMajor    = 1
  , blkProtoMinor    = 0
  , blkVrfKey        = Nothing
  , blkOpCert        = Nothing
  , blkOpCertCounter = Nothing
  , blkTxs           = [byronTx]
  }

-- ---------------------------------------------------------------------------
-- * Withdrawal-only block (conservation short-circuit)
-- ---------------------------------------------------------------------------

-- | Withdrawal-only tx. Spends @(producerHash, 3)@ for a 100_000
-- input; one output for 50_000; fee 50_000; one zero-amount
-- withdrawal. Carries no certificates, so 'hasNoDepositActivity'
-- returns 'True' and the extractor writes @tx.deposit = 0@ at parse
-- time.
withdrawalTx :: GenericTx
withdrawalTx = (emptyTx (padHash32 "WD"))
  { txBlockIndex  = 0
  , txSize        = 200
  , txFee         = 50000
  , txOutSum      = 50000
  , txInputs      = [GenericTxIn producerHash 3]
  , txOutputs     = [mkOut 0 50000]
  , txWithdrawals =
      [ GenericTxWithdrawal
          { txwRewardAddress = BS.replicate 29 0xdd
          , txwAmount        = 0
          }
      ]
  }

-- | Shelley block carrying the withdrawal-only tx.
withdrawalBlock :: GenericBlock
withdrawalBlock = producerBlock
  { blkHash         = padHash32 "BLK4"
  , blkPreviousHash = blkHash byronBlock
  , blkSlotNo       = SlotNo 300
  , blkBlockNo      = BlockNo 4
  , blkEpochSlotNo  = 300
  , blkTxs          = [withdrawalTx]
  }
