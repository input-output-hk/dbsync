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
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set

import DbSync.Parser.Types
  ( CertAction (..)
  , CredHash (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
  , rewardAddrCredHash
  )
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Pool
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  , blockPoolDeposit
  , blockRegisteredPools
  )
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash, resolveStakeCred)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Util (coinToDbLovelace)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

poolExtractor :: ExtractorDef
poolExtractor = ExtractorDef
  { pdName    = "pool"
  , pdTables  =
      [ poolUpdateTableDef
      , poolMetadataRefTableDef
      , poolOwnerTableDef
      , poolRetireTableDef
      , poolRelayTableDef
      ]
  , pdProcess = processPool
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processPool :: ProcessBlockFn
processPool ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let gb      = bcGenBlock ctx
      epochNo = unEpochNo (blkEpochNo gb)
      -- Worker-supplied protocol param when ledger ON; 'Nothing'
      -- otherwise — pool_update.deposit stays NULL to match the
      -- original schema's behaviour for ledger-disabled runs.
      mPoolDeposit = blockPoolDeposit (bcLedgerData ctx)
      -- Pools already registered in the ledger before this block.
      registeredPools = blockRegisteredPools (bcLedgerData ctx)

  -- Pools registered earlier within this same block. Combined with
  -- 'registeredPools' it tells us whether a registration activates a
  -- currently-inactive pool (+2) or re-registers an active one (+3).
  seenRef <- liftIO $ newIORef Set.empty

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
          phId <- resolveAndWritePoolHash (prdPoolHash prd)

          -- A registration activates a pool (+2) when it's neither a
          -- current ledger member nor already registered earlier in
          -- this block; otherwise it re-registers an active pool (+3).
          seenThisBlock <- liftIO $ readIORef seenRef
          let poolBytes      = prdPoolHash prd
              isReactivation =
                not (Set.member poolBytes registeredPools)
                  && not (Set.member poolBytes seenThisBlock)
          liftIO $ modifyIORef' seenRef (Set.insert poolBytes)

          mMetaId <- case prdMetadata prd of
            Nothing -> pure Nothing
            Just (url, hash) -> do
              pmId <- liftIO $ assignPoolMetadataRefId resolver
              let pm = PoolMetadataRef
                    { poolMetadataRefPoolId         = phId
                    , poolMetadataRefUrl             = url
                    , poolMetadataRefHash            = hash
                    , poolMetadataRefRegisteredTxId  = txId
                    }
              liftIO $ writePoolMetadataRef writer pmId pm
              pure (Just pmId)

          rewardAddrId <- resolveStakeCred (rewardAddrCredHash (prdRewardAddr prd))

          puId <- liftIO $ assignPoolUpdateId resolver
          -- A deposit is charged whenever a registration activates an
          -- inactive pool — its first registration and every later
          -- reactivation after a retirement. Re-registering a pool that
          -- is already active keeps the deposit already on file.
          let mDeposit = if isReactivation then coinToDbLovelace <$> mPoolDeposit
                                           else Nothing
              pu = PoolUpdate
                { poolUpdateHashId         = phId
                , poolUpdateCertIndex      = certIdx
                , poolUpdateVrfKeyHash     = prdVrfKeyHash prd
                , poolUpdatePledge         = DbLovelace (prdPledge prd)
                  -- Activating an inactive pool takes effect at
                  -- @epoch + 2@; re-registering an already-active pool
                  -- takes effect one epoch later.
                , poolUpdateActiveEpochNo  = epochNo + (if isReactivation then 2 else 3)
                , poolUpdateMetaId         = mMetaId
                , poolUpdateMargin         = prdMargin prd
                , poolUpdateFixedCost      = DbLovelace (prdCost prd)
                , poolUpdateRegisteredTxId = txId
                , poolUpdateRewardAddrId   = rewardAddrId
                , poolUpdateDeposit        = mDeposit
                }
          liftIO $ writePoolUpdate writer puId pu

          -- Write pool owners
          forM_ (prdOwners prd) $ \ownerHash -> do
            ownerAddrId <- resolveStakeCred (CredHash ownerHash False)
            liftIO $ writePoolOwner writer PoolOwner
              { poolOwnerAddrId       = ownerAddrId
              , poolOwnerPoolUpdateId = puId
              }

          -- Write pool relays
          forM_ (prdRelays prd) $ \relayData ->
            liftIO $ writePoolRelay writer (mkPoolRelay puId relayData)

        -- Pool retirement
        CertPoolRetirement poolKeyHash retiringEpoch -> do
          phId <- resolveAndWritePoolHash poolKeyHash
          liftIO $ writePoolRetire writer PoolRetire
            { poolRetireHashId        = phId
            , poolRetireCertIndex     = certIdx
            , poolRetireAnnouncedTxId = txId
            , poolRetireRetiringEpoch = retiringEpoch
            }

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

