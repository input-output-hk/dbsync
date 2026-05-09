{-# LANGUAGE OverloadedStrings #-}

-- | Core extractor.
--
-- Extracts the fundamental tables: @block@, @tx@, and @slot_leader@.
-- This extractor is always enabled and cannot be disabled.
--
-- Uses pre-assigned IDs from 'BlockContext' — it does NOT call
-- 'assignBlockId' or 'assignTxId' itself. Those are assigned
-- centrally by 'processBlock'.
module DbSync.Extractor.Core
  ( coreExtractor

    -- * Internal helpers (exported for testing and Pipeline)
  , mkBlock
  , mkTx
  , mkSlotLeader
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
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
import DbSync.Db.Schema.Ids (BlockId (..), PoolHashId, SlotLeaderId)
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..), unDbLovelace)
import DbSync.Extractor
  ( BlockContext (..)
  , BlockLedgerData (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  )
import DbSync.Ledger.Types (lookupDepositsMap)
import DbSync.Phase (SyncPhase (..))
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
-- Uses pre-assigned IDs from BlockContext:
-- 1. Write slot leader row if new
-- 2. Write block row
-- 3. Write tx rows (with phase- and ledger-aware fee/deposit dispatch)
processCore :: ProcessBlockFn
processCore resolver writer ctx = do
  let gb = bcGenBlock ctx
      blockId = bcBlockId ctx
      slId = bcSlotLeaderId ctx

  -- 1. Write slot leader row if new
  when (bcSlotLeaderNew ctx) $
    writeSlotLeader writer slId (mkSlotLeader (bcSlotLeaderPoolHashId ctx) gb)

  -- 2. Write block
  let block = mkBlock gb (bcPrevBlockId ctx) slId
  writeBlock writer blockId block

  -- 3. Write transactions
  forM_ (bcTxs ctx) $ \tc -> do
    (fee, deposit) <- computeTxFinancials resolver ctx (tcGenTx tc)
    let tx = (mkTx blockId (tcGenTx tc))
              { txFee     = fee
              , txDeposit = deposit
              }
    writeTx writer (tcTxId tc) tx

-- | Pick @tx.fee@ and @tx.deposit@ for one transaction based on
-- whether it succeeded, whether the ledger worker is on, and which
-- lifecycle phase is driving the run. Branches:
--
--   * Phase-2 failure, Follow — inline collateral diff via
--     'resolveInputValues'; @deposit = Just 0@.
--   * Phase-2 failure, Ingest — keep parser's @fee = 0@ sentinel
--     (post-load SQL fills it); @deposit = Just 0@.
--   * Valid + ledger ON, deposit observed — @bcDepositsMap@ value.
--   * Valid + ledger ON, no deposit event — @deposit = Nothing@
--     (plain transfer; matches original behaviour).
--   * Valid + ledger OFF, Follow — inline identity via
--     'resolveInputValues'.
--   * Valid + ledger OFF, Ingest — @deposit = Nothing@ (post-load
--     SQL fills it from the same identity formula).
computeTxFinancials
  :: IdResolver IO
  -> BlockContext
  -> G.GenericTx
  -> IO (DbLovelace, Maybe Int64)
computeTxFinancials resolver ctx gtx
  | not (G.txValidContract gtx) = phase2 (bcSyncPhase ctx)
  | otherwise = valid (bcSyncPhase ctx) (bcLedgerData ctx)
  where
    parserFee = DbLovelace (G.txFee gtx)

    phase2 FollowingChainTip = do
      collInValues <- resolveInputValues resolver
        [(G.txInHash i, G.txInIndex i) | i <- G.txCollateralInputs gtx]
      let collInSum  = sum (map (maybe 0 unDbLovelace) collInValues)
          collOutSum = maybe 0 G.txOutValue (G.txCollateralOutput gtx)
      pure (DbLovelace (collInSum - collOutSum), Just 0)
    phase2 _ = pure (parserFee, Just 0)

    valid _ bld
      | bldLedgerEnabled bld =
          let mDep = lookupDepositsMap (G.txHash gtx) (bldDepositsMap bld)
           in pure (parserFee, fmap coinToInt64 mDep)
    valid FollowingChainTip _ = do
      inValues <- resolveInputValues resolver
        [(G.txInHash i, G.txInIndex i) | i <- G.txInputs gtx]
      let inSum    = sum (map (maybe 0 unDbLovelace) inValues) :: Word64
          wdSum    = sum (map G.txwAmount (G.txWithdrawals gtx)) :: Word64
          outSum   = G.txOutSum gtx
          fee      = G.txFee gtx
          donation = G.txTreasuryDonation gtx
          dep      = fromIntegral inSum + fromIntegral wdSum
                   - fromIntegral outSum - fromIntegral fee
                   - fromIntegral donation :: Int64
      pure (parserFee, Just dep)
    valid _ _ = pure (parserFee, Nothing)

coinToInt64 :: Coin -> Int64
coinToInt64 (Coin n) = fromInteger n

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
--
-- The pool-hash FK arrives pre-resolved from the pipeline; it is
-- 'Nothing' for Byron blocks (the leader hash is a genesis-key
-- delegate, not a stake-pool key) and for EBBs.
mkSlotLeader :: Maybe PoolHashId -> GenericBlock -> SlotLeader
mkSlotLeader mPoolHashId gb = SlotLeader
  { slotLeaderHash        = blkSlotLeader gb
  , slotLeaderPoolHashId  = mPoolHashId
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
