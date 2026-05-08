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

import qualified Data.ByteString as BS

import DbSync.Block.Types
  ( GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , CertAction (..)
  )
import DbSync.Db.Schema.StakeDelegation
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash, resolveAndWriteStakeAddress)
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

stakeDelegationExtractor :: ExtractorDef
stakeDelegationExtractor = ExtractorDef
  { pdName         = "stake_delegation"
  , pdVersion      = 1
  , pdDependencies = [("core", 1)]
  , pdTables       = [ stakeAddressTableDef
                     , stakeRegistrationTableDef
                     , stakeDeregistrationTableDef
                     , delegationTableDef
                     , withdrawalTableDef
                     ]
  , pdProcess      = processStakeDelegation
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processStakeDelegation :: ProcessBlockFn
processStakeDelegation resolver writer ctx = do
  let gb       = bcGenBlock ctx
      epochNo  = unEpochNo (blkEpochNo gb)
      slotNo   = unSlotNo (blkSlotNo gb)
      network  = bcNetwork ctx

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
          saId <- resolveAndWriteStakeAddress network resolver writer credHash
          srId <- assignStakeRegistrationId resolver
          let sr = StakeRegistration
                { stakeRegistrationAddrId    = saId
                , stakeRegistrationCertIndex = certIdx
                , stakeRegistrationEpochNo   = epochNo
                , stakeRegistrationTxId      = txId
                , stakeRegistrationDeposit   = DbLovelace <$> mDeposit
                }
          writeStakeRegistration writer srId sr

        -- Stake deregistration
        CertStakeDeregistration credHash -> do
          saId <- resolveAndWriteStakeAddress network resolver writer credHash
          sdId <- assignStakeDeregistrationId resolver
          let sd = StakeDeregistration
                { stakeDeregistrationAddrId     = saId
                , stakeDeregistrationCertIndex  = certIdx
                , stakeDeregistrationEpochNo    = epochNo
                , stakeDeregistrationTxId       = txId
                , stakeDeregistrationRedeemerId = Nothing
                }
          writeStakeDeregistration writer sdId sd

        -- Delegation
        CertDelegation credHash poolKeyHash -> do
          saId <- resolveAndWriteStakeAddress network resolver writer credHash
          (phId, _) <- resolveAndWritePoolHash resolver writer poolKeyHash
          dId  <- assignDelegationId resolver
          let d = Delegation
                { delegationAddrId        = saId
                , delegationCertIndex     = certIdx
                , delegationPoolHashId    = phId
                , delegationActiveEpochNo = epochNo + 2
                , delegationTxId          = txId
                , delegationSlotNo        = slotNo
                , delegationRedeemerId    = Nothing
                }
          writeDelegation writer dId d

        -- Conway combined: register + delegate
        CertConwayRegDeleg credHash poolKeyHash mDeposit -> do
          saId <- resolveAndWriteStakeAddress network resolver writer credHash
          (phId, _) <- resolveAndWritePoolHash resolver writer poolKeyHash
          -- Write registration
          srId <- assignStakeRegistrationId resolver
          let sr = StakeRegistration
                { stakeRegistrationAddrId    = saId
                , stakeRegistrationCertIndex = certIdx
                , stakeRegistrationEpochNo   = epochNo
                , stakeRegistrationTxId      = txId
                , stakeRegistrationDeposit   = DbLovelace <$> mDeposit
                }
          writeStakeRegistration writer srId sr
          -- Write delegation
          dId <- assignDelegationId resolver
          let d = Delegation
                { delegationAddrId        = saId
                , delegationCertIndex     = certIdx
                , delegationPoolHashId    = phId
                , delegationActiveEpochNo = epochNo + 2
                , delegationTxId          = txId
                , delegationSlotNo        = slotNo
                , delegationRedeemerId    = Nothing
                }
          writeDelegation writer dId d

        -- Combined stake-pool + DRep delegation; the DRep half is
        -- consumed by the governance extractor.
        CertConwayDelegStakeVote credHash poolKeyHash _drep -> do
          saId <- resolveAndWriteStakeAddress network resolver writer credHash
          (phId, _) <- resolveAndWritePoolHash resolver writer poolKeyHash
          dId  <- assignDelegationId resolver
          let d = Delegation
                { delegationAddrId        = saId
                , delegationCertIndex     = certIdx
                , delegationPoolHashId    = phId
                , delegationActiveEpochNo = epochNo + 2
                , delegationTxId          = txId
                , delegationSlotNo        = slotNo
                , delegationRedeemerId    = Nothing
                }
          writeDelegation writer dId d

        -- All other cert types: handled by Pool or Governance extractors
        _ -> pure ()

    -- 2. Process withdrawals
    forM_ (txWithdrawals gtx) $ \w -> do
      let rewardAddr = txwRewardAddress w
          -- Use last 28 bytes as credential hash (skip 1-byte header)
          credHash = if BS.length rewardAddr > 1
                       then BS.drop 1 rewardAddr
                       else rewardAddr
      saId <- resolveAndWriteStakeAddress network resolver writer credHash
      wId  <- assignWithdrawalId resolver
      let wd = Withdrawal
            { withdrawalAddrId     = saId
            , withdrawalTxId       = txId
            , withdrawalAmount     = DbLovelace (txwAmount w)
            , withdrawalRedeemerId = Nothing
            }
      writeWithdrawal writer wId wd


