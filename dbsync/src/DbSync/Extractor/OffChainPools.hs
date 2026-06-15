{-# LANGUAGE OverloadedStrings #-}

-- | Off-chain pools extractor.
--
-- Owns the @off_chain_pool_data@, @off_chain_pool_fetch_error@,
-- @delisted_pool@, and @reserved_pool_ticker@ tables. The per-block
-- pass observes pool registrations that carry off-chain metadata
-- and notifies the worker queue via 'enqueuePoolMetaFetch'. Result
-- rows are written by 'DbSync.Worker.OffChain.Pool', not by this
-- extractor.
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
