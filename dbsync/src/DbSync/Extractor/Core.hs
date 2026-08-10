{-# LANGUAGE OverloadedStrings #-}

-- | Owns @block@, @tx@ and @slot_leader@, plus the shared dedup tables
-- @stake_address@ and @pool_hash@ that the pipeline writes whatever
-- the config enables. The operator cannot disable this extractor.
--
-- It reads the ids 'processBlock' pre-assigned on the 'BlockContext'
-- and never calls 'assignBlockId' or 'assignTxId' itself.
module DbSync.Extractor.Core
  ( coreExtractor

    -- * Slot-leader construction (used by 'DbSync.Extractor.Pipeline').
  , mkSlotLeader

    -- * Deposit dispatch helpers (exported for testing).
  , hasNoDepositActivity
  , affectsDeposit
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.ByteString as BS

import DbSync.Parser.Types (BlockEra (..), GenericBlock (..))
import qualified DbSync.Parser.Types as G
import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , Tx (..)
  , blockTableDef
  , poolHashTableDef
  , slotLeaderTableDef
  , stakeAddressTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Ids (BlockId (..), PoolHashId, SlotLeaderId)
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..), unDbLovelace)
import DbSync.Extractor
  ( BlockContext (..)
  , BlockLedgerData (..)
  , ExtractorDef (..)
  , LedgerOutputs (..)
  , ProcessBlockFn
  , TxContext (..)
  )
import DbSync.Worker.Ledger.Types (lookupDepositsMap)
import DbSync.Phase.Type (isFollowPath)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Util (coinToInt64)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

coreExtractor :: ExtractorDef
coreExtractor = ExtractorDef
  { pdName    = "core"
  , pdTables  =
      [ blockTableDef
      , txTableDef
      , slotLeaderTableDef
      , stakeAddressTableDef
      , poolHashTableDef
      ]
  , pdProcess = processCore
  }

-- ---------------------------------------------------------------------------
-- * Processing function
-- ---------------------------------------------------------------------------

processCore :: ProcessBlockFn
processCore ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let gb = bcGenBlock ctx
      blockId = bcBlockId ctx
      slId = bcSlotLeaderId ctx

  -- 1. Write slot leader row if new
  when (bcSlotLeaderNew ctx) $
    liftIO $ writeSlotLeader writer slId (mkSlotLeader (bcSlotLeaderPoolHashId ctx) gb)

  -- 2. Write block
  let block = mkBlock gb (bcPrevBlockId ctx) slId
  liftIO $ writeBlock writer blockId block

  -- 3. Write transactions
  forM_ (bcTxs ctx) $ \tc -> do
    (fee, deposit) <- liftIO $ computeTxFinancials resolver ctx (tcGenTx tc)
    let tx = (mkTx blockId (tcGenTx tc))
              { txFee     = fee
              , txDeposit = deposit
              }
    liftIO $ writeTx writer (tcTxId tc) tx

-- | Pick @tx.fee@ and @tx.deposit@ for one transaction. Three
-- independent axes drive the dispatch: phase-2 failure against valid
-- contract; Follow (INSERT, resolves input values inline) against
-- Ingest (COPY, post-load SQL fills the columns); and ledger worker on
-- (observed deposits) against off (identity backfill).
-- 'isFollowPath' puts 'FollowingVolatileTail' and 'FollowingChainTip'
-- on the same Follow path.
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

    phase2 p
      | isFollowPath p = do
          collInValues <- resolveInputValues resolver
            [(G.txInHash i, G.txInIndex i) | i <- G.txCollateralInputs gtx]
          let collInSum  = sum (map (maybe 0 unDbLovelace) collInValues)
              collOutSum = maybe 0 G.txOutValue (G.txCollateralOutput gtx)
          pure (DbLovelace (collInSum - collOutSum), Just 0)
      | otherwise = pure (parserFee, Just 0)

    valid p bld
      -- Identity formula is 0 by conservation; skip ledger and resolver.
      | hasNoDepositActivity gtx = pure (parserFee, Just 0)
      | LedgerDataOn outputs <- bld =
          let mDep = lookupDepositsMap (G.txHash gtx) (loDepositsMap outputs)
           in pure (parserFee, fmap coinToInt64 mDep)
      | isFollowPath p = do
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
      | otherwise = pure (parserFee, Nothing)

-- | True for txs whose @tx.deposit@ is @0@ by conservation:
-- @deposit = inputs + withdrawals - outputs - fee - treasury_donation@
-- is zero for any tx that carries no deposit-affecting certificate and
-- no gov-action proposal (each proposal locks @govActionDeposit@).
hasNoDepositActivity :: G.GenericTx -> Bool
hasNoDepositActivity g =
  not (any (affectsDeposit . G.txCertAction) (G.txCertificates g))
    && null (G.txProposals g)

-- | Whether a single certificate kind locks or refunds ada in the
-- deposit pot. 'G.CertOther' is treated as affecting defensively —
-- it carries raw CBOR of an undecoded variant.
affectsDeposit :: G.CertAction -> Bool
affectsDeposit = \case
  G.CertStakeRegistration{}    -> True
  G.CertStakeDeregistration{}  -> True
  G.CertPoolRegistration{}     -> True
  G.CertPoolRetirement{}       -> True
  G.CertConwayRegDeleg{}       -> True
  G.CertDRepRegistration{}     -> True
  G.CertDRepDeregistration{}   -> True
  G.CertOther{}                -> True
  G.CertDelegation{}           -> False
  -- Registering variants (deposit present) lock a stake-key deposit.
  G.CertConwayDelegVote _ _ mDeposit        -> isJust mDeposit
  G.CertConwayDelegStakeVote _ _ _ mDeposit -> isJust mDeposit
  G.CertDRepUpdate{}           -> False
  G.CertCommitteeAuth{}        -> False
  G.CertCommitteeResign{}      -> False
  G.CertMir{}                  -> False

-- ---------------------------------------------------------------------------
-- * Record builders (pure, shared across phases)
-- ---------------------------------------------------------------------------

-- | An Epoch Boundary Block carries no real block or slot number, so
-- those columns stay NULL. Only the epoch number means anything there.
mkBlock :: GenericBlock -> Maybe BlockId -> SlotLeaderId -> Block
mkBlock gb prevId slId = Block
  { blockHash          = blkHash gb
  , blockEpochNo       = Just (unEpochNo $ blkEpochNo gb)
  , blockSlotNo        = notEbb (unSlotNo $ blkSlotNo gb)
  , blockEpochSlotNo   = notEbb (blkEpochSlotNo gb)
  , blockBlockNo       = notEbb (unBlockNo $ blkBlockNo gb)
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
  where
    notEbb :: a -> Maybe a
    notEbb x = if blkIsEBB gb then Nothing else Just x

-- | The pipeline pre-resolves the pool-hash FK. It is 'Nothing' for
-- EBBs and for Byron blocks, where the leader hash names a genesis-key
-- delegate rather than a stake-pool key.
mkSlotLeader :: Maybe PoolHashId -> GenericBlock -> SlotLeader
mkSlotLeader mPoolHashId gb = SlotLeader
  { slotLeaderHash        = blkSlotLeader gb
  , slotLeaderPoolHashId  = mPoolHashId
  , slotLeaderDescription = mkSlotLeaderDesc gb mPoolHashId
  }

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

-- | A Byron EBB carries a synthetic null leader. A genesis-key
-- delegate signs a regular Byron block. From Shelley on, a block with
-- no resolved pool hash comes from a genesis-key delegate; any other
-- block comes from a registered stake pool.
mkSlotLeaderDesc :: GenericBlock -> Maybe PoolHashId -> Text
mkSlotLeaderDesc gb mPoolHashId
  | blkIsEBB gb           = "Epoch boundary slot leader"
  | blkEra gb == Byron    = "ByronGenesisKey-" <> shortHash
  | Nothing <- mPoolHashId = "ShelleyGenesisKey-" <> shortHash
  | otherwise             = "Pool-" <> shortHash
  where
    shortHash = toS @[Char] @Text $ concatMap hexByte (take 8 $ BS.unpack (blkSlotLeader gb))
    hexByte :: Word8 -> [Char]
    hexByte w =
      let hi = w `div` 16
          lo = w `mod` 16
      in [hexDigit hi, hexDigit lo]
    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
