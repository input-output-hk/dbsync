{-# LANGUAGE OverloadedStrings #-}

-- | Pool extractor.
--
-- Extracts pool registrations, retirements, metadata references,
-- owners, and relays into their respective tables. Also maintains
-- the @pool_hash@ dedup table.
--
-- Depends on the StakeDelegation extractor for @stake_address@
-- resolution (pool reward addresses and owner addresses are
-- resolved as stake addresses).
module DbSync.Extractor.Pool
  ( poolExtractor
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))

import DbSync.Block.Types
  ( GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , CertAction (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
  )
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Pool
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor
  ( BlockContext (..)
  , BlockLedgerData (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  )
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash, resolveAndWriteStakeAddress)
import DbSync.Resolver (IdResolver (..))
import DbSync.Util (coinToDbLovelace, rewardAddrCred)
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

poolExtractor :: ExtractorDef
poolExtractor = ExtractorDef
  { pdName         = "pool"
  , pdVersion      = 1
  , pdDependencies = [("core", 1), ("stake_delegation", 1)]
  , pdTables       = [ poolHashTableDef
                     , poolUpdateTableDef
                     , poolMetadataRefTableDef
                     , poolOwnerTableDef
                     , poolRetireTableDef
                     , poolRelayTableDef
                     ]
  , pdProcess      = processPool
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processPool :: ProcessBlockFn
processPool resolver writer ctx = do
  let gb      = bcGenBlock ctx
      epochNo = unEpochNo (blkEpochNo gb)
      network = bcNetwork ctx
      -- Worker-supplied protocol param when ledger ON; 'Nothing'
      -- otherwise — pool_update.deposit stays NULL to match the
      -- original schema's behaviour for ledger-disabled runs.
      mPoolDeposit = bldPoolDeposit (bcLedgerData ctx)

  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    -- Phase-2 failures don't register, retire, or otherwise mutate
    -- pool state on-chain.
    forM_ (txCertificates gtx) $ \cert -> do
      let certIdx = txCertIndex cert
      case txCertAction cert of

        -- Pool registration
        CertPoolRegistration prd -> do
          (phId, isFirstReg) <- resolveAndWritePoolHash resolver writer (prdPoolHash prd)

          mMetaId <- case prdMetadata prd of
            Nothing -> pure Nothing
            Just (url, hash) -> do
              pmId <- assignPoolMetadataRefId resolver
              let pm = PoolMetadataRef
                    { poolMetadataRefPoolId         = phId
                    , poolMetadataRefUrl             = url
                    , poolMetadataRefHash            = hash
                    , poolMetadataRefRegisteredTxId  = txId
                    }
              writePoolMetadataRef writer pmId pm
              pure (Just pmId)

          let rewardCredHash = rewardAddrCred (prdRewardAddr prd)
          rewardAddrId <- resolveAndWriteStakeAddress network resolver writer rewardCredHash

          puId <- assignPoolUpdateId resolver
          -- Only first registration is charged the deposit; re-
          -- registration of an already-known pool keeps the
          -- existing deposit on file.
          let mDeposit = if isFirstReg then coinToDbLovelace <$> mPoolDeposit
                                       else Nothing
              pu = PoolUpdate
                { poolUpdateHashId         = phId
                , poolUpdateCertIndex      = certIdx
                , poolUpdateVrfKeyHash     = prdVrfKeyHash prd
                , poolUpdatePledge         = DbLovelace (prdPledge prd)
                  -- First registration takes effect at @epoch + 2@;
                  -- a re-registration of an already-known pool takes
                  -- effect one epoch later.
                , poolUpdateActiveEpochNo  = epochNo + (if isFirstReg then 2 else 3)
                , poolUpdateMetaId         = mMetaId
                , poolUpdateMargin         = prdMargin prd
                , poolUpdateFixedCost      = DbLovelace (prdCost prd)
                , poolUpdateRegisteredTxId = txId
                , poolUpdateRewardAddrId   = rewardAddrId
                , poolUpdateDeposit        = mDeposit
                }
          writePoolUpdate writer puId pu

          -- 5. Write pool owners
          forM_ (prdOwners prd) $ \ownerHash -> do
            ownerAddrId <- resolveAndWriteStakeAddress network resolver writer ownerHash
            poId <- assignPoolOwnerId resolver
            let po = PoolOwner
                  { poolOwnerAddrId       = ownerAddrId
                  , poolOwnerPoolUpdateId = puId
                  }
            writePoolOwner writer poId po

          -- 6. Write pool relays
          forM_ (prdRelays prd) $ \relayData -> do
            prId <- assignPoolRelayId resolver
            let pr = mkPoolRelay puId relayData
            writePoolRelay writer prId pr

        -- Pool retirement
        CertPoolRetirement poolKeyHash retiringEpoch -> do
          (phId, _) <- resolveAndWritePoolHash resolver writer poolKeyHash
          prId <- assignPoolRetireId resolver
          let pr = PoolRetire
                { poolRetireHashId        = phId
                , poolRetireCertIndex     = certIdx
                , poolRetireAnnouncedTxId = txId
                , poolRetireRetiringEpoch = retiringEpoch
                }
          writePoolRetire writer prId pr

        -- All other cert types: not pool-related
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Build a 'PoolRelay' from relay data. Each variant only fills the
-- fields it carries; everything else defaults to 'Nothing'.
mkPoolRelay :: PoolUpdateId -> PoolRelayData -> PoolRelay
mkPoolRelay puId = \case
  PoolRelaySingleAddr mPort mIpv4 mIpv6 ->
    (emptyRelay puId) { poolRelayIpv4 = mIpv4, poolRelayIpv6 = mIpv6, poolRelayPort = mPort }
  PoolRelayDnsName mPort dnsName ->
    (emptyRelay puId) { poolRelayDnsName = Just dnsName, poolRelayPort = mPort }
  PoolRelayDnsSrv srvName ->
    (emptyRelay puId) { poolRelayDnsSrvName = Just srvName }
  where
    emptyRelay i = PoolRelay
      { poolRelayUpdateId   = i
      , poolRelayIpv4       = Nothing
      , poolRelayIpv6       = Nothing
      , poolRelayDnsName    = Nothing
      , poolRelayDnsSrvName = Nothing
      , poolRelayPort       = Nothing
      }

