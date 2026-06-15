{-# LANGUAGE OverloadedStrings #-}

-- | Stake delegation extractor.
--
-- Extracts stake registrations, deregistrations, delegations, and
-- withdrawals into their respective tables. Also maintains the
-- @stake_address@ dedup table.
--
-- Pool hash references created by delegation certificates are also
-- written to @pool_hash@ if encountered for the first time (since
-- the Pool extractor may not have seen them yet).
module DbSync.Extractor.StakeDelegation
  ( stakeDelegationExtractor
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import DbSync.Parser.Types
  ( GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , CertAction (..)
  , MirAction (..)
  , MirPot (..)
  )
import DbSync.Db.Schema.EpochBoundary
  ( PotTransfer (..)
  , Reserve (..)
  , Treasury (..)
  , potTransferTableDef
  , reserveTableDef
  , treasuryTableDef
  )
import DbSync.Db.Schema.Ids (TxId)
import DbSync.Db.Schema.StakeDelegation
import DbSync.Db.Types (DbLovelace (..), toDbInt65)
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , HasNetwork (..)
  , ProcessBlockFn
  , TxContext (..)
  , blockStakeKeyDeposit
  )
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash, resolveAndWriteStakeAddress)
import DbSync.Resolver (HasResolver (..))
import DbSync.Util (coinToDbLovelace, rewardAddrCred)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

stakeDelegationExtractor :: ExtractorDef
stakeDelegationExtractor = ExtractorDef
  { pdName    = "stake_delegation"
  , pdTables  =
      [ stakeRegistrationTableDef
      , stakeDeregistrationTableDef
      , delegationTableDef
      , withdrawalTableDef
      , potTransferTableDef
      , reserveTableDef
      , treasuryTableDef
      ]
  , pdProcess = processStakeDelegation
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processStakeDelegation :: ProcessBlockFn
processStakeDelegation ctx = do
  writer <- asks getWriter
  let gb       = bcGenBlock ctx
      epochNo  = unEpochNo (blkEpochNo gb)
      slotNo   = unSlotNo (blkSlotNo gb)

  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    -- Phase-2 failures don't materialise stake registrations,
    -- delegations, or withdrawals on-chain, so the entire body of
    -- this loop is gated above.
    forM_ (txCertificates gtx) $ \cert -> do
      let certIdx = txCertIndex cert
      case txCertAction cert of

        -- Stake registration (Shelley-Babbage + Conway)
        CertStakeRegistration credHash mDeposit -> do
          saId <- resolveAndWriteStakeAddress credHash
          liftIO $ writeStakeRegistration writer StakeRegistration
            { stakeRegistrationAddrId    = saId
            , stakeRegistrationCertIndex = certIdx
            , stakeRegistrationEpochNo   = epochNo
            , stakeRegistrationTxId      = txId
            , stakeRegistrationDeposit   = stakeDeposit mDeposit
            }

        -- Stake deregistration
        CertStakeDeregistration credHash -> do
          saId <- resolveAndWriteStakeAddress credHash
          liftIO $ writeStakeDeregistration writer StakeDeregistration
            { stakeDeregistrationAddrId     = saId
            , stakeDeregistrationCertIndex  = certIdx
            , stakeDeregistrationEpochNo    = epochNo
            , stakeDeregistrationTxId       = txId
            , stakeDeregistrationRedeemerId = Nothing
            }

        -- Delegation
        CertDelegation credHash poolKeyHash -> do
          saId <- resolveAndWriteStakeAddress credHash
          (phId, _) <- resolveAndWritePoolHash poolKeyHash
          liftIO $ writeDelegation writer Delegation
            { delegationAddrId        = saId
            , delegationCertIndex     = certIdx
            , delegationPoolHashId    = phId
            , delegationActiveEpochNo = epochNo + 2
            , delegationTxId          = txId
            , delegationSlotNo        = slotNo
            , delegationRedeemerId    = Nothing
            }

        -- Conway combined: register + delegate
        CertConwayRegDeleg credHash poolKeyHash mDeposit -> do
          saId <- resolveAndWriteStakeAddress credHash
          (phId, _) <- resolveAndWritePoolHash poolKeyHash
          liftIO $ writeStakeRegistration writer StakeRegistration
            { stakeRegistrationAddrId    = saId
            , stakeRegistrationCertIndex = certIdx
            , stakeRegistrationEpochNo   = epochNo
            , stakeRegistrationTxId      = txId
            , stakeRegistrationDeposit   = stakeDeposit mDeposit
            }
          liftIO $ writeDelegation writer Delegation
            { delegationAddrId        = saId
            , delegationCertIndex     = certIdx
            , delegationPoolHashId    = phId
            , delegationActiveEpochNo = epochNo + 2
            , delegationTxId          = txId
            , delegationSlotNo        = slotNo
            , delegationRedeemerId    = Nothing
            }

        -- Combined stake-pool + DRep delegation; the DRep half is
        -- consumed by the governance extractor.
        CertConwayDelegStakeVote credHash poolKeyHash _drep -> do
          saId <- resolveAndWriteStakeAddress credHash
          (phId, _) <- resolveAndWritePoolHash poolKeyHash
          liftIO $ writeDelegation writer Delegation
            { delegationAddrId        = saId
            , delegationCertIndex     = certIdx
            , delegationPoolHashId    = phId
            , delegationActiveEpochNo = epochNo + 2
            , delegationTxId          = txId
            , delegationSlotNo        = slotNo
            , delegationRedeemerId    = Nothing
            }

        -- Move-Instantaneous-Reward cert (Shelley-Babbage only).
        CertMir pot action ->
          processMirCert txId certIdx pot action

        -- All other cert types: handled by Pool or Governance extractors
        _ -> pure ()

    -- 2. Process withdrawals
    forM_ (txWithdrawals gtx) $ \w -> do
      let credHash = rewardAddrCred (txwRewardAddress w)
      saId <- resolveAndWriteStakeAddress credHash
      liftIO $ writeWithdrawal writer Withdrawal
        { withdrawalAddrId     = saId
        , withdrawalTxId       = txId
        , withdrawalAmount     = DbLovelace (txwAmount w)
        , withdrawalRedeemerId = Nothing
        }
  where
    -- Conway+ certs carry the deposit inline; Shelley-Babbage rely
    -- on the worker's protocol-param value when the ledger is on.
    stakeDeposit :: Maybe Word64 -> Maybe DbLovelace
    stakeDeposit (Just d) = Just (DbLovelace d)
    stakeDeposit Nothing  = coinToDbLovelace <$> blockStakeKeyDeposit (bcLedgerData ctx)

-- ---------------------------------------------------------------------------
-- * MIR cert handling
-- ---------------------------------------------------------------------------

-- | Write the rows produced by one MIR certificate.
--
-- 'MirToStakeAddresses' writes one @reserve@ or @treasury@ row per
-- recipient, depending on the named pot. 'MirPotToPot' writes a
-- single @pot_transfer@ row carrying signed treasury/reserves
-- deltas: a positive amount with @MirReserves@ credits the
-- treasury and debits the reserves.
processMirCert
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => TxId -> Word16 -> MirPot -> MirAction -> m ()
processMirCert txId certIdx pot = \case
  MirToStakeAddresses recipients ->
    forM_ recipients (writeMirRecipient txId certIdx pot)
  MirPotToPot xfer ->
    writePotTransferRow txId certIdx pot xfer

writeMirRecipient
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => TxId -> Word16 -> MirPot -> (ByteString, Integer) -> m ()
writeMirRecipient txId certIdx pot (credHash, dcoin) = do
  writer <- asks getWriter
  saId   <- resolveAndWriteStakeAddress credHash
  let amount = toDbInt65 (fromInteger dcoin)
  case pot of
    MirReserves ->
      liftIO $ writeReserve writer Reserve
        { reserveAddrId    = saId
        , reserveCertIndex = certIdx
        , reserveAmount    = amount
        , reserveTxId      = txId
        }
    MirTreasury ->
      liftIO $ writeTreasury writer Treasury
        { treasuryAddrId    = saId
        , treasuryCertIndex = certIdx
        , treasuryAmount    = amount
        , treasuryTxId      = txId
        }

-- | A pot-to-pot transfer with positive amount on the named pot
-- credits the /other/ pot. Treasury and reserves deltas are
-- mirror-image signed values.
writePotTransferRow
  :: ( HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => TxId -> Word16 -> MirPot -> Integer -> m ()
writePotTransferRow txId certIdx pot xfer = do
  writer <- asks getWriter
  let signed = fromInteger xfer :: Int64
      (toTreasury, toReserves) = case pot of
        MirReserves -> (signed, negate signed)
        MirTreasury -> (negate signed, signed)
  liftIO $ writePotTransfer writer PotTransfer
    { potTransferTxId      = txId
    , potTransferCertIndex = certIdx
    , potTransferTreasury  = toDbInt65 toTreasury
    , potTransferReserves  = toDbInt65 toReserves
    }

