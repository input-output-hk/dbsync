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

import qualified Data.ByteString as BS

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
import DbSync.Db.Schema.StakeDelegation (StakeAddress (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
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

  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    forM_ (txCertificates gtx) $ \cert -> do
      let certIdx = txCertIndex cert
      case txCertAction cert of

        -- Pool registration
        CertPoolRegistration prd -> do
          -- 1. Resolve pool hash
          phId <- resolveAndWritePoolHash resolver writer (prdPoolHash prd)

          -- 2. Optionally write metadata ref
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

          -- 3. Resolve reward address as stake address
          let rewardAddr = prdRewardAddr prd
              rewardCredHash = if BS.length rewardAddr > 1
                                 then BS.drop 1 rewardAddr
                                 else rewardAddr
          rewardAddrId <- resolveAndWriteStakeAddress resolver writer rewardCredHash

          -- 4. Write pool update
          puId <- assignPoolUpdateId resolver
          let pu = PoolUpdate
                { poolUpdateHashId         = phId
                , poolUpdateCertIndex      = certIdx
                , poolUpdateVrfKeyHash     = prdVrfKeyHash prd
                , poolUpdatePledge         = DbLovelace (prdPledge prd)
                , poolUpdateActiveEpochNo  = epochNo + 2
                , poolUpdateMetaId         = mMetaId
                , poolUpdateMargin         = prdMargin prd
                , poolUpdateFixedCost      = DbLovelace (prdCost prd)
                , poolUpdateRegisteredTxId = txId
                , poolUpdateRewardAddrId   = rewardAddrId
                , poolUpdateDeposit        = Nothing  -- deposit requires ledger state
                }
          writePoolUpdate writer puId pu

          -- 5. Write pool owners
          forM_ (prdOwners prd) $ \ownerHash -> do
            ownerAddrId <- resolveAndWriteStakeAddress resolver writer ownerHash
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
          phId <- resolveAndWritePoolHash resolver writer poolKeyHash
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

-- | Build a 'PoolRelay' from relay data.
mkPoolRelay :: PoolUpdateId -> PoolRelayData -> PoolRelay
mkPoolRelay puId (PoolRelaySingleAddr mPort mIpv4 mIpv6) = PoolRelay
  { poolRelayUpdateId   = puId
  , poolRelayIpv4       = mIpv4
  , poolRelayIpv6       = mIpv6
  , poolRelayDnsName    = Nothing
  , poolRelayDnsSrvName = Nothing
  , poolRelayPort       = mPort
  }
mkPoolRelay puId (PoolRelayDnsName mPort dnsName) = PoolRelay
  { poolRelayUpdateId   = puId
  , poolRelayIpv4       = Nothing
  , poolRelayIpv6       = Nothing
  , poolRelayDnsName    = Just dnsName
  , poolRelayDnsSrvName = Nothing
  , poolRelayPort       = mPort
  }
mkPoolRelay puId (PoolRelayDnsSrv srvName) = PoolRelay
  { poolRelayUpdateId   = puId
  , poolRelayIpv4       = Nothing
  , poolRelayIpv6       = Nothing
  , poolRelayDnsName    = Nothing
  , poolRelayDnsSrvName = Just srvName
  , poolRelayPort       = Nothing
  }

-- | Resolve a pool hash by key hash. If new, write the @pool_hash@ row.
resolveAndWritePoolHash
  :: IdResolver IO
  -> Writer IO
  -> ByteString
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

-- | Resolve a stake address by credential hash. If new, write the
-- @stake_address@ row.
resolveAndWriteStakeAddress
  :: IdResolver IO
  -> Writer IO
  -> ByteString
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
