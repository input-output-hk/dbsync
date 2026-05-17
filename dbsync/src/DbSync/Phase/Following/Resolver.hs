{-# LANGUAGE OverloadedStrings #-}

-- | FollowingChainTip ID resolver.
--
-- Two implementations:
--
-- * 'mkFollowResolver' — every @assign*Id@ does a @nextval@
--   round-trip; every @resolve*@ does a @SELECT@ then a @nextval@
--   on miss. Used by the integration test suite.
--
-- * 'mkBufferedFollowResolver' — @assign*Id@ pops from a queue of
--   IDs pre-allocated in one pipeline at start of block;
--   @resolve*@ still SELECTs synchronously but checks a per-block
--   in-process map first (so a SELECT seeing a sibling's
--   not-yet-flushed INSERT still finds it). @recordTxOutAddress@
--   queues the dependent @UPDATE@ on the shared 'WriteBuffer'.
--   Used in production.
--
-- Both share the same dedup contracts and the same FK invariants;
-- the diff test confirms identical rows in PG.
module DbSync.Phase.Following.Resolver
  ( mkFollowResolver
  , mkBufferedFollowResolver
  ) where

import Cardano.Prelude

import qualified Data.Map.Strict as Map
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids
import DbSync.Db.Statement.Address
  ( insertAddressRowStmt
  , nextAddressIdStmt
  , queryAddressIdStmt
  )
import DbSync.Db.Statement.Block (nextBlockIdStmt)
import DbSync.Db.Statement.CollateralTxIn (nextCollateralTxInIdStmt)
import DbSync.Db.Statement.CollateralTxOut
  ( nextCollateralTxOutIdStmt
  , updateCollateralTxOutAddressIdStmt
  )
import DbSync.Db.Statement.Delegation (nextDelegationIdStmt)
import DbSync.Db.Statement.MaTxMint (nextMaTxMintIdStmt)
import DbSync.Db.Statement.MaTxOut (nextMaTxOutIdStmt)
import DbSync.Db.Statement.MultiAsset
  ( nextMultiAssetIdStmt
  , queryMultiAssetIdStmt
  )
import DbSync.Db.Schema.MultiAsset (multiAssetName, multiAssetPolicy)
import DbSync.Db.Statement.PoolHash (nextPoolHashIdStmt, queryPoolHashIdStmt)
import DbSync.Db.Statement.PoolMetadataRef (nextPoolMetadataRefIdStmt)
import DbSync.Db.Statement.PoolOwner (nextPoolOwnerIdStmt)
import DbSync.Db.Statement.PoolRelay (nextPoolRelayIdStmt)
import DbSync.Db.Statement.PoolRetire (nextPoolRetireIdStmt)
import DbSync.Db.Statement.PoolUpdate (nextPoolUpdateIdStmt)
import DbSync.Db.Statement.ReferenceTxIn (nextReferenceTxInIdStmt)
import DbSync.Db.Statement.SlotLeader
  ( nextSlotLeaderIdStmt
  , querySlotLeaderIdStmt
  )
import DbSync.Db.Statement.StakeAddress
  ( nextStakeAddressIdStmt
  , queryStakeAddressIdStmt
  )
import DbSync.Db.Statement.StakeDeregistration (nextStakeDeregistrationIdStmt)
import DbSync.Db.Statement.StakeRegistration (nextStakeRegistrationIdStmt)
import DbSync.Db.Statement.Tx (nextTxIdStmt)
import DbSync.Db.Statement.TxCbor (nextTxCborIdStmt)
import DbSync.Db.Statement.TxIn (nextTxInIdStmt)
import DbSync.Db.Statement.TxMetadata (nextTxMetadataIdStmt)
import DbSync.Db.Statement.TxOut
  ( nextTxOutIdStmt
  , queryOutputValueStmt
  , updateTxOutAddressIdStmt
  )
import DbSync.Db.Statement.Withdrawal (nextWithdrawalIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)
import DbSync.Resolver (IdResolver (..))

mkFollowResolver :: Conn.Connection -> IO (IdResolver IO)
mkFollowResolver conn = do
  lastBlock <- newIORef Nothing
  pure $ resolver conn lastBlock

resolver :: Conn.Connection -> IORef (Maybe BlockId) -> IdResolver IO
resolver conn lastBlock = IdResolver
  { assignBlockId = do
      bid <- run conn () nextBlockIdStmt
      writeIORef lastBlock (Just bid)
      pure bid

  , resolveSlotLeader = \hash _leader -> do
      mId <- run conn hash querySlotLeaderIdStmt
      case mId of
        Just sid -> pure (sid, False)
        Nothing  -> do
          sid <- run conn () nextSlotLeaderIdStmt
          pure (sid, True)

  , resolvePrevBlock = \_hash -> readIORef lastBlock

  , assignTxId = run conn () nextTxIdStmt

    -- Address recording: resolve the FK synchronously and UPDATE the
    -- row that 'writeTxOut' has just inserted with @address_id = NULL@.
    -- See 'resolveAddressId'.
  , recordTxOutAddress = \txOutId rawBytes addr -> do
      aid <- resolveAddressId conn rawBytes addr
      run conn (aid, txOutId) updateTxOutAddressIdStmt
  , recordCollateralTxOutAddress = \outId rawBytes addr -> do
      aid <- resolveAddressId conn rawBytes addr
      run conn (aid, outId) updateCollateralTxOutAddressIdStmt

    -- UTxO IDs (no resolver-side dedup — straight nextval per row)
  , assignTxOutId            = run conn () nextTxOutIdStmt
  , assignTxInId             = run conn () nextTxInIdStmt
  , assignCollateralTxInId   = run conn () nextCollateralTxInIdStmt
  , assignCollateralTxOutId  = run conn () nextCollateralTxOutIdStmt
  , assignReferenceTxInId    = run conn () nextReferenceTxInIdStmt

    -- Metadata IDs (no resolver-side dedup)
  , assignTxMetadataId       = run conn () nextTxMetadataIdStmt

    -- MultiAsset IDs.
    -- 'multi_asset' is dedup-keyed by (policy, name) — SELECT first,
    -- nextval on miss. The dedup key handed in by the extractor (a
    -- 'ShortByteString' formed from policy ++ name) is ignored here;
    -- we use the structured policy / name fields for the SELECT.
  , resolveMultiAsset = \_key ma -> do
      mId <- run conn (multiAssetPolicy ma, multiAssetName ma)
                     queryMultiAssetIdStmt
      case mId of
        Just maId -> pure (maId, False)
        Nothing   -> do
          maId <- run conn () nextMultiAssetIdStmt
          pure (maId, True)
  , assignMaTxMintId         = run conn () nextMaTxMintIdStmt
  , assignMaTxOutId          = run conn () nextMaTxOutIdStmt

    -- StakeDelegation IDs.
    -- 'stake_address' deduplicates by 28-byte credential hash. The
    -- resolver mirrors the slot_leader / multi_asset pattern: SELECT
    -- by hash, allocate from the sequence on miss.
  , resolveStakeAddress = \hash _sa -> do
      mId <- run conn hash queryStakeAddressIdStmt
      case mId of
        Just saId -> pure (saId, False)
        Nothing   -> do
          saId <- run conn () nextStakeAddressIdStmt
          pure (saId, True)
  , assignStakeRegistrationId   = run conn () nextStakeRegistrationIdStmt
  , assignStakeDeregistrationId = run conn () nextStakeDeregistrationIdStmt
  , assignDelegationId          = run conn () nextDelegationIdStmt
  , assignWithdrawalId          = run conn () nextWithdrawalIdStmt

    -- Pool IDs.
    -- 'pool_hash' deduplicates by 28-byte pool key hash. SELECT first,
    -- nextval on miss; same shape as 'stake_address' / 'multi_asset'.
  , resolvePoolHash = \hash _ph -> do
      mId <- run conn hash queryPoolHashIdStmt
      case mId of
        Just phId -> pure (phId, False)
        Nothing   -> do
          phId <- run conn () nextPoolHashIdStmt
          pure (phId, True)
  , assignPoolUpdateId       = run conn () nextPoolUpdateIdStmt
  , assignPoolMetadataRefId  = run conn () nextPoolMetadataRefIdStmt
  , assignPoolOwnerId        = run conn () nextPoolOwnerIdStmt
  , assignPoolRetireId       = run conn () nextPoolRetireIdStmt
  , assignPoolRelayId        = run conn () nextPoolRelayIdStmt

    -- CBOR IDs (no resolver-side dedup)
  , assignTxCborId           = run conn () nextTxCborIdStmt

  -- Filled in once their extractors gain test coverage. The IO
  -- actions defer evaluation so unused fields don't crash record
  -- construction.
  , assignEpochSyncStatsId   = todo "assignEpochSyncStatsId"
  , assignAdaPotsId          = todo "assignAdaPotsId"

    -- Inline value resolution: per-pair SELECT against tx_out.
  , resolveInputValues = \pairs ->
      forM pairs $ \pair -> run conn pair queryOutputValueStmt
  }

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> panic $ "Follow resolver session failed: " <> show e

-- | Look up an existing @address.id@ by raw bytes, or insert a new
-- row and return its allocated id. Used by @recordTxOutAddress@ and
-- @recordCollateralTxOutAddress@ to obtain the FK value before the
-- subsequent UPDATE.
resolveAddressId :: Conn.Connection -> ByteString -> Address -> IO AddressId
resolveAddressId conn rawBytes addr = do
  mId <- run conn rawBytes queryAddressIdStmt
  case mId of
    Just aid -> pure aid
    Nothing  -> do
      aid <- run conn () nextAddressIdStmt
      run conn (aid, addr) insertAddressRowStmt
      pure aid

todo :: Text -> IO a
todo name = pure $ panic $ "Phase.Following.Resolver." <> name <> " not yet implemented"

-- ---------------------------------------------------------------------------
-- * Buffered resolver (production path)
-- ---------------------------------------------------------------------------

-- | Per-block in-process dedup cache.
--
-- Keeps the @SELECT@-then-@INSERT@ contract intact when the INSERTs
-- are deferred to the end-of-block buffer flush: a second resolve
-- of the same hash within the block finds the previously-allocated
-- id in the map without consulting PG.
--
-- Lifetime is one block; a new cache is built for every
-- 'processForward' invocation. Cross-block reuse is Stage 2; here
-- each cache starts empty and is discarded after COMMIT.
data BlockDedupCache = BlockDedupCache
  { bdcSlotLeader   :: !(IORef (Map ByteString SlotLeaderId))
  , bdcPoolHash     :: !(IORef (Map ByteString PoolHashId))
  , bdcStakeAddress :: !(IORef (Map ByteString StakeAddressId))
  , bdcMultiAsset   :: !(IORef (Map (ByteString, ByteString) MultiAssetId))
  , bdcAddress      :: !(IORef (Map ByteString AddressId))
  }

newBlockDedupCache :: IO BlockDedupCache
newBlockDedupCache = BlockDedupCache
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty

-- | Buffered Follow resolver. Same observable behaviour as
-- 'mkFollowResolver'; the difference is where the work lands:
--
--   * @assign*Id@ pops from per-sequence queues in
--     'PreAllocatedIds' (no network round-trip).
--   * dedup @resolve*@ checks the per-block cache first; on miss,
--     does the same @SELECT@ then @nextval@ pattern as the
--     immediate resolver. The corresponding INSERT is queued via
--     the 'Writer' as today; the per-block cache shadows the
--     not-yet-flushed row.
--   * @recordTxOutAddress@\/@recordCollateralTxOutAddress@ queue
--     their INSERT (when new) and UPDATE on the supplied
--     'WriteBuffer' rather than running them inline.
mkBufferedFollowResolver
  :: Conn.Connection
  -> PreAllocatedIds
  -> WriteBuffer
  -> IO (IdResolver IO)
mkBufferedFollowResolver conn preAlloc buf = do
  lastBlock <- newIORef Nothing
  cache     <- newBlockDedupCache
  pure (bufferedResolver conn preAlloc buf lastBlock cache)

bufferedResolver
  :: Conn.Connection
  -> PreAllocatedIds
  -> WriteBuffer
  -> IORef (Maybe BlockId)
  -> BlockDedupCache
  -> IdResolver IO
bufferedResolver conn preAlloc buf lastBlock cache = IdResolver
  { -- Block ID: still uses 'nextBlockIdStmt' synchronously because
    -- there's exactly one per block (no batching to be done) and
    -- 'resolvePrevBlock' below needs the value materialised.
    assignBlockId = do
      bid <- run conn () nextBlockIdStmt
      writeIORef lastBlock (Just bid)
      pure bid

  , resolveSlotLeader = \hash _leader ->
      resolveDedupSimple
        conn
        hash
        (bdcSlotLeader cache)
        querySlotLeaderIdStmt
        nextSlotLeaderIdStmt

  , resolvePrevBlock = \_hash -> readIORef lastBlock

  , -- Pre-allocated assigns. Zero round-trips.
    assignTxId               = popHead "assignTxId"
                                  (paiTxIds preAlloc)
  , assignTxOutId            = popHead "assignTxOutId"
                                  (paiTxOutIds preAlloc)
  , assignTxInId             = popHead "assignTxInId"
                                  (paiTxInIds preAlloc)
  , assignCollateralTxInId   = popHead "assignCollateralTxInId"
                                  (paiCollateralTxInIds preAlloc)
  , assignCollateralTxOutId  = popHead "assignCollateralTxOutId"
                                  (paiCollateralTxOutIds preAlloc)
  , assignReferenceTxInId    = popHead "assignReferenceTxInId"
                                  (paiReferenceTxInIds preAlloc)
  , assignTxMetadataId       = popHead "assignTxMetadataId"
                                  (paiTxMetadataIds preAlloc)
  , assignMaTxMintId         = popHead "assignMaTxMintId"
                                  (paiMaTxMintIds preAlloc)
  , assignMaTxOutId          = popHead "assignMaTxOutId"
                                  (paiMaTxOutIds preAlloc)
  , assignTxCborId           = popHead "assignTxCborId"
                                  (paiTxCborIds preAlloc)
  , assignStakeRegistrationId   = popHead "assignStakeRegistrationId"
                                  (paiStakeRegistrationIds preAlloc)
  , assignStakeDeregistrationId = popHead "assignStakeDeregistrationId"
                                  (paiStakeDeregistrationIds preAlloc)
  , assignDelegationId       = popHead "assignDelegationId"
                                  (paiDelegationIds preAlloc)
  , assignWithdrawalId       = popHead "assignWithdrawalId"
                                  (paiWithdrawalIds preAlloc)
  , assignPoolUpdateId       = popHead "assignPoolUpdateId"
                                  (paiPoolUpdateIds preAlloc)
  , assignPoolMetadataRefId  = popHead "assignPoolMetadataRefId"
                                  (paiPoolMetadataRefIds preAlloc)
  , assignPoolOwnerId        = popHead "assignPoolOwnerId"
                                  (paiPoolOwnerIds preAlloc)
  , assignPoolRetireId       = popHead "assignPoolRetireId"
                                  (paiPoolRetireIds preAlloc)
  , assignPoolRelayId        = popHead "assignPoolRelayId"
                                  (paiPoolRelayIds preAlloc)

  , -- Multi-asset dedup keyed by (policy, name). The 'ShortByteString'
    -- key the extractor hands in is ignored here in favour of the
    -- structured (policy, name) tuple that the @SELECT@ statement
    -- expects.
    resolveMultiAsset = \_key ma -> do
      let policy = multiAssetPolicy ma
          name   = multiAssetName ma
          key    = (policy, name)
      m <- readIORef (bdcMultiAsset cache)
      case Map.lookup key m of
        Just maId -> pure (maId, False)
        Nothing -> do
          mId <- run conn key queryMultiAssetIdStmt
          case mId of
            Just maId -> do
              cacheInsert (bdcMultiAsset cache) key maId
              pure (maId, False)
            Nothing -> do
              maId <- run conn () nextMultiAssetIdStmt
              cacheInsert (bdcMultiAsset cache) key maId
              pure (maId, True)

  , resolveStakeAddress = \hash _sa ->
      resolveDedupSimple
        conn
        hash
        (bdcStakeAddress cache)
        queryStakeAddressIdStmt
        nextStakeAddressIdStmt

  , resolvePoolHash = \hash _ph ->
      resolveDedupSimple
        conn
        hash
        (bdcPoolHash cache)
        queryPoolHashIdStmt
        nextPoolHashIdStmt

  , recordTxOutAddress = \txOutId rawBytes addr -> do
      aid <- resolveAddressIdBuffered conn buf (bdcAddress cache) rawBytes addr
      -- The @tx_out@ row was queued by 'writeTxOut' a few lines
      -- earlier with @address_id = NULL@. The UPDATE that fills it
      -- in joins that queue here so they flush together.
      append buf (Pipeline.statement (aid, txOutId) updateTxOutAddressIdStmt)
  , recordCollateralTxOutAddress = \outId rawBytes addr -> do
      aid <- resolveAddressIdBuffered conn buf (bdcAddress cache) rawBytes addr
      append buf (Pipeline.statement (aid, outId) updateCollateralTxOutAddressIdStmt)

  , -- Stubs reserved for extractors that haven't landed; their fields
    -- are evaluated only when those extractors fire. Keeping them
    -- here lets the record construction succeed for tests that
    -- never call them.
    assignEpochSyncStatsId = todo "assignEpochSyncStatsId"
  , assignAdaPotsId        = todo "assignAdaPotsId"

  , -- @resolveInputValues@ stays per-pair for now. The pairs could
    -- be batched into one pipeline at the cost of some interface
    -- restructuring; deferred to Stage 2.
    resolveInputValues = \pairs ->
      forM pairs $ \pair -> run conn pair queryOutputValueStmt
  }

-- | Look up an existing dedup id by its raw key; allocate a fresh
-- one on miss. Returns the (id, isNew) pair the existing extractor
-- pattern uses to decide whether to fire 'writeXxx'.
resolveDedupSimple
  :: Ord key
  => Conn.Connection
  -> key
  -> IORef (Map key idType)
  -> Stmt.Statement key (Maybe idType)
  -> Stmt.Statement () idType
  -> IO (idType, Bool)
resolveDedupSimple conn key mapRef queryStmt nextStmt = do
  m <- readIORef mapRef
  case Map.lookup key m of
    Just i -> pure (i, False)
    Nothing -> do
      mId <- run conn key queryStmt
      case mId of
        Just i -> do
          cacheInsert mapRef key i
          pure (i, False)
        Nothing -> do
          i <- run conn () nextStmt
          cacheInsert mapRef key i
          pure (i, True)

-- | Same shape as the immediate 'resolveAddressId' but the INSERT
-- (when new) is queued on the 'WriteBuffer' instead of running
-- inline.
resolveAddressIdBuffered
  :: Conn.Connection
  -> WriteBuffer
  -> IORef (Map ByteString AddressId)
  -> ByteString
  -> Address
  -> IO AddressId
resolveAddressIdBuffered conn buf mapRef rawBytes addr = do
  m <- readIORef mapRef
  case Map.lookup rawBytes m of
    Just aid -> pure aid
    Nothing -> do
      mId <- run conn rawBytes queryAddressIdStmt
      case mId of
        Just aid -> do
          cacheInsert mapRef rawBytes aid
          pure aid
        Nothing -> do
          aid <- run conn () nextAddressIdStmt
          cacheInsert mapRef rawBytes aid
          append buf (Pipeline.statement (aid, addr) insertAddressRowStmt)
          pure aid

cacheInsert :: Ord k => IORef (Map k v) -> k -> v -> IO ()
cacheInsert ref k v = atomicModifyIORef' ref $ \m -> (Map.insert k v m, ())
