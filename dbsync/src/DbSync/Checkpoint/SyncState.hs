{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Checkpoint.SyncState
Description : Read\/write the @dbsync_sync_state@ singleton PG table.

The sync-state table holds the resume point that survives a crash or
restart. Every epoch boundary commits a new row; boot reads the latest
row and either resumes 'IngestChainHistory' past it, or jumps into
'FollowingChainTip' if we are close enough to the node's tip.

Note: this mechanism is independent of 'DbSync.Ledger.Snapshot' —
sync-state is a PG row that always exists (ledger-on or ledger-off),
whereas ledger snapshots are an optional on-disk artefact only
written when the ledger feature is enabled. See LEDGER-PLAN.md §7
for how the two fit together at boot.

This module owns:

  * 'SyncStateRow' — a Haskell mirror of the table row.
  * 'ControlConnection' — a dedicated @libpq@ connection used for
    non-COPY database work (sync-state read\/write, dedup-map rebuild
    queries).
  * Three read\/write operations — 'readSyncState', 'writeSyncState',
    'seedSyncState'.

'DbSync.Checkpoint.Manager.commitEpoch' layers the per-epoch
atomicity model on top of 'writeSyncState', and the resume flow
consumes the row on boot.

'rebuildDedupMaps' is currently a stub that lives here for Phase 1
convenience; it actually belongs under 'DbSync.Id.DedupMap' because
it reads from @slot_leader@, @stake_address@, @multi_asset@ etc.
rather than from @dbsync_sync_state@. When the boot flow fleshes it
out, this note is the reminder to relocate it.
-}
module DbSync.Checkpoint.SyncState
  ( -- * Types
    SyncStateRow (..)
  , ControlConnection

    -- * Connection lifecycle
  , openControlConnection
  , closeControlConnection

    -- * Read\/write
  , readSyncState
  , writeSyncState
  , seedSyncState

    -- * Dedup map rebuild (currently a stub)
  , rebuildDedupMaps

    -- * Internals exposed for testing
  , selectSyncStateSql
  , updateSyncStateSql
  , seedSyncStateSql
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import qualified Database.PostgreSQL.LibPQ as PQ

import DbSync.Error (AppError (..), throwAppError)
import DbSync.Id.DedupMap (DedupMaps, newMaps)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A single row from the @dbsync_sync_state@ table.
--
-- * @ssrLastCommitted*@ are @Nothing@ on a freshly-seeded database
--   (no epoch has been committed yet).
-- * Counter fields are monotonic and point to the __next__ id to
--   assign, matching 'DbSync.Id.Counter.IdCounter.icNext'.
-- * The row also carries @schema_version_applied@ and @ledger_enabled@
--   so boot can detect configuration mismatches.
--
-- Field order matches the column order in 'DbSync.Db.Schema.SyncState.syncStateTableDef',
-- which in turn drives 'selectSyncStateSql' and 'updateSyncStateSql'.
data SyncStateRow = SyncStateRow
  { ssrLastCommittedSlot             :: !(Maybe Word64)
  , ssrLastCommittedBlockNo          :: !(Maybe Word64)
  , ssrLastCommittedBlockHash        :: !(Maybe ByteString)
  , ssrBlockIdCounter                :: !Int64
  , ssrTxIdCounter                   :: !Int64
  , ssrTxOutIdCounter                :: !Int64
  , ssrTxInIdCounter                 :: !Int64
  , ssrCollateralTxInIdCounter       :: !Int64
  , ssrReferenceTxInIdCounter        :: !Int64
  , ssrTxMetadataIdCounter           :: !Int64
  , ssrMaTxMintIdCounter             :: !Int64
  , ssrMaTxOutIdCounter              :: !Int64
  , ssrSlotLeaderIdCounter           :: !Int64
  , ssrStakeAddressIdCounter         :: !Int64
  , ssrPoolHashIdCounter             :: !Int64
  , ssrMultiAssetIdCounter           :: !Int64
  , ssrScriptIdCounter               :: !Int64
  , ssrStakeRegistrationIdCounter    :: !Int64
  , ssrStakeDeregistrationIdCounter  :: !Int64
  , ssrDelegationIdCounter           :: !Int64
  , ssrWithdrawalIdCounter           :: !Int64
  , ssrPoolUpdateIdCounter           :: !Int64
  , ssrPoolMetadataRefIdCounter      :: !Int64
  , ssrPoolOwnerIdCounter            :: !Int64
  , ssrPoolRetireIdCounter           :: !Int64
  , ssrPoolRelayIdCounter            :: !Int64
  , ssrTxCborIdCounter               :: !Int64
  , ssrEpochSyncStatsIdCounter       :: !Int64
  , ssrSchemaVersionApplied          :: !Int
  , ssrLedgerEnabled                 :: !Bool
  }
  deriving stock (Eq, Show)

-- | A @libpq@ connection dedicated to non-COPY operations.
--
-- The connection is held open for the lifetime of the sync process
-- (opened during boot, closed on shutdown), so we pay the TLS\/auth
-- handshake only once. It lives alongside the per-table COPY
-- connections owned by 'DbSync.Copy.Writer.CopyWriter' and is the
-- sole route for:
--
--   1. Reading\/writing 'dbsync_sync_state'.
--   2. Dedup-map rebuild queries.
--   3. Resume-time @DELETE FROM block WHERE slot_no > …@ cleanup.
newtype ControlConnection = ControlConnection
  { unControlConnection :: PQ.Connection
  }

-- ---------------------------------------------------------------------------
-- * Connection lifecycle
-- ---------------------------------------------------------------------------

-- | Open a fresh 'ControlConnection'. Throws 'AppDatabaseError' if the
-- @libpq@ handshake fails.
openControlConnection :: HasCallStack => ByteString -> IO ControlConnection
openControlConnection connStr = do
  conn <- PQ.connectdb connStr
  status <- PQ.status conn
  when (status /= PQ.ConnectionOk) $ do
    errMsg <- PQ.errorMessage conn
    throwAppError AppDatabaseError $
      "Failed to open control connection: "
      <> maybe "(no error message)" (TE.decodeUtf8 . BS.copy) errMsg
  pure (ControlConnection conn)

-- | Release the underlying @libpq@ connection. Safe to call multiple
-- times — 'PQ.finish' is idempotent.
closeControlConnection :: ControlConnection -> IO ()
closeControlConnection = PQ.finish . unControlConnection

-- ---------------------------------------------------------------------------
-- * SQL text (exposed for golden tests)
-- ---------------------------------------------------------------------------

-- | @SELECT@ that reads a 'SyncStateRow' from the singleton row.
--
-- Column order here __must__ match the field order of 'SyncStateRow'
-- and the 'PQ.getvalue' indices in 'parseSyncStateRow'.
selectSyncStateSql :: ByteString
selectSyncStateSql = BS8.concat
  [ "SELECT "
  , BS8.intercalate ", "
      [ "last_committed_slot"
      , "last_committed_block_no"
      , "last_committed_block_hash"
      , "block_id_counter"
      , "tx_id_counter"
      , "tx_out_id_counter"
      , "tx_in_id_counter"
      , "collateral_tx_in_id_counter"
      , "reference_tx_in_id_counter"
      , "tx_metadata_id_counter"
      , "ma_tx_mint_id_counter"
      , "ma_tx_out_id_counter"
      , "slot_leader_id_counter"
      , "stake_address_id_counter"
      , "pool_hash_id_counter"
      , "multi_asset_id_counter"
      , "script_id_counter"
      , "stake_registration_id_counter"
      , "stake_deregistration_id_counter"
      , "delegation_id_counter"
      , "withdrawal_id_counter"
      , "pool_update_id_counter"
      , "pool_metadata_ref_id_counter"
      , "pool_owner_id_counter"
      , "pool_retire_id_counter"
      , "pool_relay_id_counter"
      , "tx_cbor_id_counter"
      , "epoch_sync_stats_id_counter"
      , "schema_version_applied"
      , "ledger_enabled"
      ]
  , " FROM dbsync_sync_state WHERE id = 1;"
  ]

-- | @UPDATE@ that writes a 'SyncStateRow' to the singleton row.
--
-- Placeholder @$i@ numbering matches 'encodeSyncStateRow' exactly.
-- @updated_at@ is refreshed to @now()@ server-side so that replicas
-- and monitoring dashboards see a consistent wall-clock timestamp.
updateSyncStateSql :: ByteString
updateSyncStateSql = BS8.concat
  [ "UPDATE dbsync_sync_state SET "
  , BS8.intercalate ", "
      [ "last_committed_slot             = $1"
      , "last_committed_block_no         = $2"
      , "last_committed_block_hash       = $3"
      , "block_id_counter                = $4"
      , "tx_id_counter                   = $5"
      , "tx_out_id_counter               = $6"
      , "tx_in_id_counter                = $7"
      , "collateral_tx_in_id_counter     = $8"
      , "reference_tx_in_id_counter      = $9"
      , "tx_metadata_id_counter          = $10"
      , "ma_tx_mint_id_counter           = $11"
      , "ma_tx_out_id_counter            = $12"
      , "slot_leader_id_counter          = $13"
      , "stake_address_id_counter        = $14"
      , "pool_hash_id_counter            = $15"
      , "multi_asset_id_counter          = $16"
      , "script_id_counter               = $17"
      , "stake_registration_id_counter   = $18"
      , "stake_deregistration_id_counter = $19"
      , "delegation_id_counter           = $20"
      , "withdrawal_id_counter           = $21"
      , "pool_update_id_counter          = $22"
      , "pool_metadata_ref_id_counter    = $23"
      , "pool_owner_id_counter           = $24"
      , "pool_retire_id_counter          = $25"
      , "pool_relay_id_counter           = $26"
      , "tx_cbor_id_counter              = $27"
      , "epoch_sync_stats_id_counter     = $28"
      , "schema_version_applied          = $29"
      , "ledger_enabled                  = $30"
      , "updated_at                      = now()"
      ]
  , " WHERE id = 1;"
  ]

-- | Idempotent seed @INSERT@. Relies on the table's 'CHECK (id = 1)'
-- plus 'DEFAULT 1' clauses to guarantee a single-row shape. All
-- counters pick up their @DEFAULT 1@. Only @schema_version_applied@
-- and @ledger_enabled@ come from the caller.
seedSyncStateSql :: ByteString
seedSyncStateSql =
  "INSERT INTO dbsync_sync_state (schema_version_applied, ledger_enabled) \
  \VALUES ($1, $2) ON CONFLICT (id) DO NOTHING;"

-- ---------------------------------------------------------------------------
-- * Read
-- ---------------------------------------------------------------------------

-- | Read the singleton row. Returns @Nothing@ if the table is present
-- but empty (i.e. 'seedSyncState' has never been called — an anomaly
-- in practice). Throws 'AppDatabaseError' on SQL errors.
readSyncState :: HasCallStack => ControlConnection -> IO (Maybe SyncStateRow)
readSyncState (ControlConnection conn) = do
  mResult <- PQ.exec conn selectSyncStateSql
  result  <- requireResult conn "readSyncState" mResult
  status  <- PQ.resultStatus result
  unless (status == PQ.TuplesOk) $ do
    errMsg <- PQ.resultErrorMessage result
    throwAppError AppDatabaseError $
      "readSyncState: unexpected result status "
      <> show status <> ": "
      <> maybe "(no error)" (TE.decodeUtf8 . BS.copy) errMsg
  nRows <- PQ.ntuples result
  if nRows < 1
    then pure Nothing
    else Just <$> parseSyncStateRow result 0

-- ---------------------------------------------------------------------------
-- * Write
-- ---------------------------------------------------------------------------

-- | Overwrite the singleton row with the given 'SyncStateRow'.
--
-- Single-statement, so atomic at the server: either the new values
-- land or the old ones stick. The @updated_at@ column is refreshed
-- server-side via @now()@. Throws 'AppDatabaseError' on SQL errors
-- or if zero rows are affected (which means 'seedSyncState' was
-- never called — a programmer error).
writeSyncState :: HasCallStack => ControlConnection -> SyncStateRow -> IO ()
writeSyncState (ControlConnection conn) row = do
  mResult <- PQ.execParams conn updateSyncStateSql (encodeSyncStateRow row) PQ.Text
  result  <- requireResult conn "writeSyncState" mResult
  status  <- PQ.resultStatus result
  unless (status == PQ.CommandOk) $ do
    errMsg <- PQ.resultErrorMessage result
    throwAppError AppDatabaseError $
      "writeSyncState: unexpected result status "
      <> show status <> ": "
      <> maybe "(no error)" (TE.decodeUtf8 . BS.copy) errMsg
  mAffected <- PQ.cmdTuples result
  case mAffected >>= parseInt64 of
    Just 1 -> pure ()
    Just n ->
      throwAppError AppDatabaseError $
        "writeSyncState: UPDATE affected " <> show n
        <> " rows, expected exactly 1. Did seedSyncState run?"
    Nothing ->
      throwAppError AppDatabaseError
        "writeSyncState: UPDATE returned no row count"

-- | Insert the singleton row with sensible defaults. Idempotent
-- (@ON CONFLICT DO NOTHING@): calling twice is a no-op. Must be
-- invoked once after 'DbSync.Db.Schema.Init.initSchema' creates the
-- table.
seedSyncState
  :: HasCallStack
  => ControlConnection
  -> Int   -- ^ @schema_version_applied@ — the schema version we are seeding at.
  -> Bool  -- ^ @ledger_enabled@ — captured so boot can detect config flips.
  -> IO ()
seedSyncState (ControlConnection conn) schemaVersion ledgerEnabled = do
  let params =
        [ txtParam (txtInt (fromIntegral schemaVersion))
        , txtParam (txtBool ledgerEnabled)
        ]
  mResult <- PQ.execParams conn seedSyncStateSql params PQ.Text
  result  <- requireResult conn "seedSyncState" mResult
  status  <- PQ.resultStatus result
  unless (status == PQ.CommandOk) $ do
    errMsg <- PQ.resultErrorMessage result
    throwAppError AppDatabaseError $
      "seedSyncState: unexpected result status "
      <> show status <> ": "
      <> maybe "(no error)" (TE.decodeUtf8 . BS.copy) errMsg

-- ---------------------------------------------------------------------------
-- * Dedup-map rebuild (stub)
-- ---------------------------------------------------------------------------

-- | Rebuild the dedup maps by streaming the relevant lookup tables
-- from PostgreSQL on boot. The eventual implementation will issue
-- server-side cursor queries against @slot_leader@, @stake_address@,
-- @pool_hash@, @multi_asset@, @script@, @drep_hash@, etc.
--
-- This is currently an explicit __stub__: returns empty maps. It is
-- only called from the resume path, which is not yet wired up — so
-- calling it at runtime is a programmer error.
rebuildDedupMaps :: ControlConnection -> IO DedupMaps
rebuildDedupMaps _conn = newMaps  -- TODO: stream from PG cursors

-- ---------------------------------------------------------------------------
-- * Internal: encode / decode
-- ---------------------------------------------------------------------------

-- | Encode a 'SyncStateRow' as a positional parameter list for
-- 'updateSyncStateSql'. The order matches the @$1 … $30@ placeholders
-- in that statement byte-for-byte.
encodeSyncStateRow :: SyncStateRow -> [Maybe (PQ.Oid, ByteString, PQ.Format)]
encodeSyncStateRow r =
  [ optParam txtWord  (ssrLastCommittedSlot r)
  , optParam txtWord  (ssrLastCommittedBlockNo r)
  , optParam txtBytea (ssrLastCommittedBlockHash r)
  , txtParam (txtInt (ssrBlockIdCounter r))
  , txtParam (txtInt (ssrTxIdCounter r))
  , txtParam (txtInt (ssrTxOutIdCounter r))
  , txtParam (txtInt (ssrTxInIdCounter r))
  , txtParam (txtInt (ssrCollateralTxInIdCounter r))
  , txtParam (txtInt (ssrReferenceTxInIdCounter r))
  , txtParam (txtInt (ssrTxMetadataIdCounter r))
  , txtParam (txtInt (ssrMaTxMintIdCounter r))
  , txtParam (txtInt (ssrMaTxOutIdCounter r))
  , txtParam (txtInt (ssrSlotLeaderIdCounter r))
  , txtParam (txtInt (ssrStakeAddressIdCounter r))
  , txtParam (txtInt (ssrPoolHashIdCounter r))
  , txtParam (txtInt (ssrMultiAssetIdCounter r))
  , txtParam (txtInt (ssrScriptIdCounter r))
  , txtParam (txtInt (ssrStakeRegistrationIdCounter r))
  , txtParam (txtInt (ssrStakeDeregistrationIdCounter r))
  , txtParam (txtInt (ssrDelegationIdCounter r))
  , txtParam (txtInt (ssrWithdrawalIdCounter r))
  , txtParam (txtInt (ssrPoolUpdateIdCounter r))
  , txtParam (txtInt (ssrPoolMetadataRefIdCounter r))
  , txtParam (txtInt (ssrPoolOwnerIdCounter r))
  , txtParam (txtInt (ssrPoolRetireIdCounter r))
  , txtParam (txtInt (ssrPoolRelayIdCounter r))
  , txtParam (txtInt (ssrTxCborIdCounter r))
  , txtParam (txtInt (ssrEpochSyncStatsIdCounter r))
  , txtParam (txtInt (fromIntegral (ssrSchemaVersionApplied r)))
  , txtParam (txtBool (ssrLedgerEnabled r))
  ]

-- | Decode a row fetched by 'selectSyncStateSql'. Column indices
-- here must match the SELECT list order.
parseSyncStateRow :: HasCallStack => PQ.Result -> PQ.Row -> IO SyncStateRow
parseSyncStateRow result row =
  SyncStateRow
    <$> getOptCol "last_committed_slot"              0  parseWord64
    <*> getOptCol "last_committed_block_no"          1  parseWord64
    <*> getOptCol "last_committed_block_hash"        2  parseBytea
    <*> getReqCol "block_id_counter"                 3  parseInt64
    <*> getReqCol "tx_id_counter"                    4  parseInt64
    <*> getReqCol "tx_out_id_counter"                5  parseInt64
    <*> getReqCol "tx_in_id_counter"                 6  parseInt64
    <*> getReqCol "collateral_tx_in_id_counter"      7  parseInt64
    <*> getReqCol "reference_tx_in_id_counter"       8  parseInt64
    <*> getReqCol "tx_metadata_id_counter"           9  parseInt64
    <*> getReqCol "ma_tx_mint_id_counter"            10 parseInt64
    <*> getReqCol "ma_tx_out_id_counter"             11 parseInt64
    <*> getReqCol "slot_leader_id_counter"           12 parseInt64
    <*> getReqCol "stake_address_id_counter"         13 parseInt64
    <*> getReqCol "pool_hash_id_counter"             14 parseInt64
    <*> getReqCol "multi_asset_id_counter"           15 parseInt64
    <*> getReqCol "script_id_counter"                16 parseInt64
    <*> getReqCol "stake_registration_id_counter"    17 parseInt64
    <*> getReqCol "stake_deregistration_id_counter"  18 parseInt64
    <*> getReqCol "delegation_id_counter"            19 parseInt64
    <*> getReqCol "withdrawal_id_counter"            20 parseInt64
    <*> getReqCol "pool_update_id_counter"           21 parseInt64
    <*> getReqCol "pool_metadata_ref_id_counter"     22 parseInt64
    <*> getReqCol "pool_owner_id_counter"            23 parseInt64
    <*> getReqCol "pool_retire_id_counter"           24 parseInt64
    <*> getReqCol "pool_relay_id_counter"            25 parseInt64
    <*> getReqCol "tx_cbor_id_counter"               26 parseInt64
    <*> getReqCol "epoch_sync_stats_id_counter"      27 parseInt64
    <*> getReqCol "schema_version_applied"           28 parseInt
    <*> getReqCol "ledger_enabled"                   29 parseBool
  where
    getOptCol :: Text -> PQ.Column -> (ByteString -> Maybe a) -> IO (Maybe a)
    getOptCol name col parser = do
      mBs <- PQ.getvalue' result row col
      case mBs of
        Nothing -> pure Nothing
        Just bs -> case parser bs of
          Just v  -> pure (Just v)
          Nothing ->
            throwAppError AppDatabaseError $
              "parseSyncStateRow: cannot parse optional column "
              <> name <> " value " <> show bs

    getReqCol :: Text -> PQ.Column -> (ByteString -> Maybe a) -> IO a
    getReqCol name col parser = do
      mBs <- PQ.getvalue' result row col
      case mBs of
        Nothing ->
          throwAppError AppDatabaseError $
            "parseSyncStateRow: unexpected NULL in required column " <> name
        Just bs -> case parser bs of
          Just v  -> pure v
          Nothing ->
            throwAppError AppDatabaseError $
              "parseSyncStateRow: cannot parse required column "
              <> name <> " value " <> show bs

-- ---------------------------------------------------------------------------
-- * Internal: encode helpers
-- ---------------------------------------------------------------------------

-- | Wrap an already-serialised value as a text-format 'PQ.execParams'
-- parameter. 'PQ.Oid' @0@ lets PostgreSQL infer the type from the
-- statement.
txtParam :: ByteString -> Maybe (PQ.Oid, ByteString, PQ.Format)
txtParam bs = Just (PQ.Oid 0, bs, PQ.Text)

-- | Like 'txtParam' but for a nullable column. 'Nothing' becomes a
-- SQL NULL parameter; 'Just' is encoded via the supplied serialiser.
optParam :: (a -> ByteString) -> Maybe a -> Maybe (PQ.Oid, ByteString, PQ.Format)
optParam encoder = fmap (\x -> (PQ.Oid 0, encoder x, PQ.Text))

-- | Encode an 'Int64' as decimal ASCII.
txtInt :: Int64 -> ByteString
txtInt = BS8.pack . show

-- | Encode a 'Word64' as decimal ASCII.
txtWord :: Word64 -> ByteString
txtWord = BS8.pack . show

-- | Encode a 'Bool' as @t@\/@f@ (PostgreSQL boolean text format).
txtBool :: Bool -> ByteString
txtBool True  = "t"
txtBool False = "f"

-- | Encode a 'ByteString' as a @\\xHEX@ literal suitable for a
-- @BYTEA@ column in text format.
txtBytea :: ByteString -> ByteString
txtBytea bs = "\\x" <> hexifyBytes bs
  where
    hexifyBytes :: ByteString -> ByteString
    hexifyBytes = BS.concatMap (\w -> BS.pack [hexNibble (shiftR w 4 .&. 0x0F), hexNibble (w .&. 0x0F)])

    hexNibble :: Word8 -> Word8
    hexNibble n
      | n < 10    = n + 0x30  -- '0'
      | otherwise = n - 10 + 0x61  -- 'a'

-- ---------------------------------------------------------------------------
-- * Internal: parse helpers
-- ---------------------------------------------------------------------------

parseInt64 :: ByteString -> Maybe Int64
parseInt64 = readMaybe . BS8.unpack

parseWord64 :: ByteString -> Maybe Word64
parseWord64 = readMaybe . BS8.unpack

parseInt :: ByteString -> Maybe Int
parseInt = readMaybe . BS8.unpack

parseBool :: ByteString -> Maybe Bool
parseBool "t" = Just True
parseBool "f" = Just False
parseBool _   = Nothing

-- | Parse PostgreSQL's default @\\xHEX@ bytea text representation
-- back to raw bytes. The legacy @\\NNN@ escape format is not
-- supported — every modern @postgresql@ install defaults to the
-- @hex@ @bytea_output@ format.
parseBytea :: ByteString -> Maybe ByteString
parseBytea bs = case BS8.stripPrefix "\\x" bs of
  Nothing  -> Nothing
  Just hex -> decodeHex hex

decodeHex :: ByteString -> Maybe ByteString
decodeHex bs
  | BS.length bs `mod` 2 /= 0 = Nothing
  | otherwise =
      fmap BS.pack (mapM decodePair (chunksOf2 bs))
  where
    chunksOf2 :: ByteString -> [ByteString]
    chunksOf2 b
      | BS.null b = []
      | otherwise = BS.take 2 b : chunksOf2 (BS.drop 2 b)

    decodePair :: ByteString -> Maybe Word8
    decodePair pair = do
      hi <- fromHexChar (BS.index pair 0)
      lo <- fromHexChar (BS.index pair 1)
      pure (hi * 16 + lo)

    fromHexChar :: Word8 -> Maybe Word8
    fromHexChar c
      | c >= 0x30 && c <= 0x39 = Just (c - 0x30)          -- 0-9
      | c >= 0x61 && c <= 0x66 = Just (c - 0x61 + 10)     -- a-f
      | c >= 0x41 && c <= 0x46 = Just (c - 0x41 + 10)     -- A-F
      | otherwise              = Nothing

-- ---------------------------------------------------------------------------
-- * Internal: result helpers
-- ---------------------------------------------------------------------------

-- | Unwrap a 'PQ.exec' \/ 'PQ.execParams' result, throwing if the
-- driver returned 'Nothing' (protocol error, connection lost, OOM).
requireResult
  :: HasCallStack
  => PQ.Connection
  -> Text           -- ^ caller name, for diagnostics
  -> Maybe PQ.Result
  -> IO PQ.Result
requireResult conn caller = \case
  Just r  -> pure r
  Nothing -> do
    errMsg <- PQ.errorMessage conn
    throwAppError AppDatabaseError $
      caller <> ": libpq returned no result: "
      <> maybe "(no error message)" (TE.decodeUtf8 . BS.copy) errMsg


