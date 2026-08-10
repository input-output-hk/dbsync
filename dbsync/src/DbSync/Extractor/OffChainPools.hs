{-# LANGUAGE OverloadedStrings #-}

-- | Owns @off_chain_pool_data@, @off_chain_pool_fetch_error@,
-- @delisted_pool@ and @reserved_pool_ticker@. The per-block pass only
-- reports pool registrations that carry off-chain metadata, through
-- 'enqueuePoolMetaFetch'. 'DbSync.Worker.OffChain.Pool' writes the
-- result rows.
module DbSync.Extractor.OffChainPools
  ( offChainPoolsExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.OffChainPool
  ( offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Pool
  ( delistedPoolTableDef
  , reservedPoolTickerTableDef
  )
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  )
import DbSync.Parser.Types
  ( CertAction (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , PoolRegistrationData (..)
  )
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Worker.OffChain.Types (PoolMetadataRef (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

offChainPoolsExtractor :: ExtractorDef
offChainPoolsExtractor = ExtractorDef
  { pdName    = "off_chain_pools"
  , pdTables  =
      [ offChainPoolDataTableDef
      , offChainPoolFetchErrorTableDef
      , delistedPoolTableDef
      , reservedPoolTickerTableDef
      ]
  , pdProcess = processOffChainPools
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processOffChainPools :: ProcessBlockFn
processOffChainPools ctx = do
  resolver <- asks getResolver
  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $
    forM_ (txCertificates (tcGenTx tc)) $ \cert ->
      case txCertAction cert of
        CertPoolRegistration prd
          | Just (url, h) <- prdMetadata prd ->
              liftIO $ enqueuePoolMetaFetch resolver $ PoolMetadataRef
                { pmrPoolId   = prdPoolHash prd
                , pmrUrl      = url
                , pmrMetaHash = h
                }
        _ -> pure ()
