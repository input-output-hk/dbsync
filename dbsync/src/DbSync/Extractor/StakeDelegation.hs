{-# LANGUAGE OverloadedStrings #-}

-- | Writes stake registrations, deregistrations, delegations and
-- withdrawals, plus the MIR-derived pot rows.
--
-- A delegation certificate can name a pool the Pool extractor has not
-- seen yet, so this extractor writes @pool_hash@ and @stake_address@
-- rows too, through the shared dedup helpers.
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
  , CredHash (..)
  , MirAction (..)
  , MirPot (..)
  , rewardAddrCredHash
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
  , redeemerIdAt
  )
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash, resolveStakeCred)
import DbSync.Resolver (HasResolver (..))
import DbSync.Util (coinToDbLovelace)
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
        CertStakeRegistration cred mDeposit -> do
          saId <- resolveStakeCred cred
          liftIO $ writeStakeRegistration writer StakeRegistration
            { stakeRegistrationAddrId    = saId
            , stakeRegistrationCertIndex = certIdx
            , stakeRegistrationEpochNo   = epochNo
            , stakeRegistrationTxId      = txId
            , stakeRegistrationDeposit   = stakeDeposit mDeposit
            }

        -- Stake deregistration
        CertStakeDeregistration cred -> do
          saId <- resolveStakeCred cred
          liftIO $ writeStakeDeregistration writer StakeDeregistration
            { stakeDeregistrationAddrId     = saId
            , stakeDeregistrationCertIndex  = certIdx
            , stakeDeregistrationEpochNo    = epochNo
            , stakeDeregistrationTxId       = txId
            , stakeDeregistrationRedeemerId = redeemerIdAt tc (txCertRedeemerIx cert)
            }

        -- Delegation
        CertDelegation cred poolKeyHash -> do
          saId <- resolveStakeCred cred
          phId <- resolveAndWritePoolHash poolKeyHash
          liftIO $ writeDelegation writer Delegation
            { delegationAddrId        = saId
            , delegationCertIndex     = certIdx
            , delegationPoolHashId    = phId
            , delegationActiveEpochNo = epochNo + 2
            , delegationTxId          = txId
            , delegationSlotNo        = slotNo
            , delegationRedeemerId    = redeemerIdAt tc (txCertRedeemerIx cert)
            }

        -- Conway combined: register + delegate
        CertConwayRegDeleg cred poolKeyHash mDeposit -> do
          saId <- resolveStakeCred cred
          phId <- resolveAndWritePoolHash poolKeyHash
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
            , delegationRedeemerId    = redeemerIdAt tc (txCertRedeemerIx cert)
            }

        -- DRep-only delegation. The DRep half is consumed by the
        -- governance extractor; here a present deposit (RegDeleg variant)
        -- materialises the stake_registration.
        CertConwayDelegVote cred _drep mDeposit ->
          forM_ mDeposit $ \_ -> do
            saId <- resolveStakeCred cred
            liftIO $ writeStakeRegistration writer StakeRegistration
              { stakeRegistrationAddrId    = saId
              , stakeRegistrationCertIndex = certIdx
              , stakeRegistrationEpochNo   = epochNo
              , stakeRegistrationTxId      = txId
              , stakeRegistrationDeposit   = stakeDeposit mDeposit
              }

        -- Combined stake-pool + DRep delegation; the DRep half is
        -- consumed by the governance extractor. A present deposit means
        -- the cert also registers the stake key (RegDeleg variant), so a
        -- stake_registration row is materialised here too.
        CertConwayDelegStakeVote cred poolKeyHash _drep mDeposit -> do
          saId <- resolveStakeCred cred
          phId <- resolveAndWritePoolHash poolKeyHash
          forM_ mDeposit $ \_ ->
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
            , delegationRedeemerId    = redeemerIdAt tc (txCertRedeemerIx cert)
            }

        -- Move-Instantaneous-Reward cert (Shelley-Babbage only).
        CertMir pot action ->
          processMirCert txId certIdx pot action

        -- All other cert types: handled by Pool or Governance extractors
        _ -> pure ()

    -- Process withdrawals
    forM_ (txWithdrawals gtx) $ \w -> do
      saId <- resolveStakeCred (rewardAddrCredHash (txwRewardAddress w))
      liftIO $ writeWithdrawal writer Withdrawal
        { withdrawalAddrId     = saId
        , withdrawalTxId       = txId
        , withdrawalAmount     = DbLovelace (txwAmount w)
        , withdrawalRedeemerId = redeemerIdAt tc (txwRedeemerIx w)
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
  => TxId -> Word16 -> MirPot -> (CredHash, Integer) -> m ()
writeMirRecipient txId certIdx pot (cred, dcoin) = do
  writer <- asks getWriter
  saId   <- resolveStakeCred cred
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

