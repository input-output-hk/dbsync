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
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Pool (PoolHash (..))
import DbSync.Db.Schema.StakeDelegation
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
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

  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    -- 1. Process certificates
    forM_ (txCertificates gtx) $ \cert -> do
      let certIdx = txCertIndex cert
      case txCertAction cert of

        -- Stake registration (Shelley-Babbage + Conway)
        CertStakeRegistration credHash mDeposit -> do
          saId <- resolveAndWriteStakeAddress resolver writer credHash
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
          saId <- resolveAndWriteStakeAddress resolver writer credHash
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
          saId <- resolveAndWriteStakeAddress resolver writer credHash
          phId <- resolveAndWritePoolHash resolver writer poolKeyHash
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
          saId <- resolveAndWriteStakeAddress resolver writer credHash
          phId <- resolveAndWritePoolHash resolver writer poolKeyHash
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

        -- Conway: delegate to stake pool + DRep (ignore DRep for now)
        CertConwayDelegStakeVote credHash poolKeyHash _drepHash -> do
          saId <- resolveAndWriteStakeAddress resolver writer credHash
          phId <- resolveAndWritePoolHash resolver writer poolKeyHash
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
      saId <- resolveAndWriteStakeAddress resolver writer credHash
      wId  <- assignWithdrawalId resolver
      let wd = Withdrawal
            { withdrawalAddrId     = saId
            , withdrawalTxId       = txId
            , withdrawalAmount     = DbLovelace (txwAmount w)
            , withdrawalRedeemerId = Nothing
            }
      writeWithdrawal writer wId wd

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Resolve a stake address by credential hash. If new, write the
-- @stake_address@ row.
resolveAndWriteStakeAddress
  :: IdResolver IO
  -> Writer IO
  -> ByteString    -- ^ Stake credential hash (28 bytes)
  -> IO StakeAddressId
resolveAndWriteStakeAddress resolver writer credHash = do
  let sa = StakeAddress
        { stakeAddressHashRaw    = credHash
        , stakeAddressView       = "stake_" <> hexEncode credHash
        , stakeAddressScriptHash = Nothing
        }
  (saId, isNew) <- resolveStakeAddress resolver credHash sa
  when isNew $
    writeStakeAddress writer saId sa
  pure saId

-- | Resolve a pool hash by key hash. If new, write the @pool_hash@ row.
-- This ensures the dedup entry and row exist even if the Pool extractor
-- hasn't processed the pool registration cert yet.
resolveAndWritePoolHash
  :: IdResolver IO
  -> Writer IO
  -> ByteString    -- ^ Pool key hash (28 bytes)
  -> IO PoolHashId
resolveAndWritePoolHash resolver writer poolKeyHash = do
  let ph = PoolHash
        { poolHashHashRaw = poolKeyHash
        , poolHashView    = "pool_" <> hexEncode poolKeyHash
        }
  (phId, isNew) <- resolvePoolHash resolver poolKeyHash ph
  when isNew $
    writePoolHash writer phId ph
  pure phId

-- | Hex-encode a 'ByteString' to 'Text'.
hexEncode :: ByteString -> Text
hexEncode = toS @[Char] @Text . concatMap hexByte . BS.unpack
  where
    hexByte :: Word8 -> [Char]
    hexByte w =
      let hi = w `div` 16
          lo = w `mod` 16
      in [hexDigit hi, hexDigit lo]
    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
