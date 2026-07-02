{-# LANGUAGE OverloadedStrings #-}

-- | Parameterised hasql 'Statement's used at boot to clean up rows
-- past the resume point, rebuild dedup maps, and fix up governance
-- enactment state.
--
-- Each statement is built per-table (table name interpolated into
-- the SQL); the @WHERE@ predicate is parameterised. Table names come
-- from 'tdName' on static 'TableDef's, so identifier interpolation
-- is safe — no user-controlled input flows into the SQL.
module DbSync.Db.Statement.Worker.Resume
  ( -- * Cleanup deletes
    deleteBySlotStmt
  , deleteByBlockSlotStmt
  , deleteByTxFkSlotStmt
  , deleteByTxOutFkSlotStmt
  , deleteByEpochSlotStmt
  , deleteByIdCounterStmt

    -- * Dedup-map rebuild selects
  , selectDedupSingleStmt
  , declareDedupSingleCursorStmt
  , fetchDedupSinglePageStmt
  , declareMultiAssetDedupCursorStmt
  , fetchMultiAssetDedupPageStmt
  , selectDrepHashDedupStmt
  , selectCommitteeHashDedupStmt
  , selectVotingAnchorDedupStmt
  , selectGovActionProposalCacheStmt

    -- * Boot-time canonicalisation
  , selectBlockHashAtSlotStmt

    -- * Governance enactment lookups
  , selectCommitteeByProposalStmt
  , selectConstitutionByProposalStmt

    -- * Governance status-column updates
  , updateGovActionRatifiedStmt
  , updateGovActionEnactedStmt
  , updateGovActionDroppedStmt
  , updateGovActionExpiredStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.MultiAsset (MultiAssetCols (..), multiAssetCols, multiAssetTableDef)
import DbSync.Db.Schema.Types (TableColumn (..))
import DbSync.Db.Sql (quoteIdent)
import DbSync.Db.Sql.Refs (table)
import DbSync.Db.Types (AnchorType, anchorTypeDecoder)

-- | @DELETE FROM <table> WHERE slot_no > $1@.
deleteBySlotStmt :: Text -> Stmt.Statement Word64 Int64
deleteBySlotStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE slot_no > $1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @DELETE FROM <table> WHERE block_id IN (SELECT id FROM block
-- WHERE slot_no > $1)@. For tables that reference @block@ via
-- @block_id@ but don't carry @slot_no@.
deleteByBlockSlotStmt :: Text -> Stmt.Statement Word64 Int64
deleteByBlockSlotStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE block_id IN (SELECT id FROM \"block\" WHERE slot_no > $1)"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Trim a table that anchors to the chain through a FK to @tx@
-- (e.g. @tx_metadata.tx_id@, @pool_retire.announced_tx_id@). Joins
-- the FK column back to @block.slot_no@. For identity-leaf fact
-- tables that carry neither @slot_no@ nor @block_id@ and have no id
-- counter, this is the only resume-time pruning that reaches them.
deleteByTxFkSlotStmt
  :: Text  -- ^ Table to delete from.
  -> Text  -- ^ FK column on that table referencing @tx.id@.
  -> Stmt.Statement Word64 Int64
deleteByTxFkSlotStmt tableName fkCol =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName, " AS t"
      , " USING \"tx\", \"block\""
      , " WHERE t.", quoteIdent fkCol, " = \"tx\".id"
      , " AND \"tx\".block_id = \"block\".id"
      , " AND \"block\".slot_no > $1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Trim a table that anchors through a FK to @tx_out@
-- (e.g. @ma_tx_out.tx_out_id@). Joins @tx_out -> tx -> block@.
deleteByTxOutFkSlotStmt
  :: Text  -- ^ Table to delete from.
  -> Text  -- ^ FK column on that table referencing @tx_out.id@.
  -> Stmt.Statement Word64 Int64
deleteByTxOutFkSlotStmt tableName fkCol =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName, " AS t"
      , " USING \"tx_out\", \"tx\", \"block\""
      , " WHERE t.", quoteIdent fkCol, " = \"tx_out\".id"
      , " AND \"tx_out\".tx_id = \"tx\".id"
      , " AND \"tx\".block_id = \"block\".id"
      , " AND \"block\".slot_no > $1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Trim a ledger-derived, epoch-keyed table (@epoch_stake@,
-- @reward@, @pool_stat@, …) that carries no @slot_no@, @block_id@,
-- tx FK, or id counter. The cutoff slot is mapped to its epoch via
-- @block@, and every row from that epoch onward is deleted: Follow
-- re-emits those epochs from scratch on replay past the resume point.
deleteByEpochSlotStmt
  :: Text  -- ^ Table to delete from.
  -> Text  -- ^ Epoch column (@epoch_no@ or @spendable_epoch@).
  -> Stmt.Statement Word64 Int64
deleteByEpochSlotStmt tableName epochCol =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE ", quoteIdent epochCol, " >= COALESCE("
      , "(SELECT epoch_no FROM \"block\""
      , " WHERE slot_no <= $1 ORDER BY slot_no DESC LIMIT 1), 0)"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @DELETE FROM <table> WHERE id >= $1@. The @$1@ is the table's
-- @*_id_counter@ in @dbsync_sync_state@ (\"next id to assign\"), so
-- this prunes rows the previous run wrote past the last-recorded
-- counter. Used for counter-tracked tables that lack slot or block
-- references.
deleteByIdCounterStmt :: Text -> Stmt.Statement Int64 Int64
deleteByIdCounterStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE id >= $1"
      ]
    encoder = E.param (E.nonNullable E.int8)

-- | @SELECT id, <keyCol> FROM <table>@ for dedup tables whose
-- natural key is a single column (@slot_leader.hash@,
-- @stake_address.hash_raw@, @pool_hash.hash_raw@).
selectDedupSingleStmt :: Text -> Text -> Stmt.Statement () [(Int64, ByteString)]
selectDedupSingleStmt tableName keyCol =
  Stmt.unpreparable sql E.noParams decoder
  where
    sql = T.concat
      [ "SELECT id, ", quoteIdent keyCol
      , " FROM ", quoteIdent tableName
      ]
    decoder = D.rowList $
      (,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)

-- | Declare a forward-only cursor over @(id, keyCol)@ for the boot
-- dedup rebuild. Reads in physical heap order, so it needs no @id@
-- index — the dedup tables have none until the Preparing phase.
declareDedupSingleCursorStmt
  :: Text -- ^ cursor name
  -> Text -- ^ table name
  -> Text -- ^ key column
  -> Stmt.Statement () ()
declareDedupSingleCursorStmt cursorName tableName keyCol =
  Stmt.unpreparable sql E.noParams D.noResult
  where
    sql = T.concat
      [ "DECLARE ", quoteIdent cursorName, " NO SCROLL CURSOR FOR "
      , "SELECT id, ", quoteIdent keyCol
      , " FROM ", quoteIdent tableName
      ]

-- | Fetch the next page from a dedup cursor. The row count is a SQL
-- literal because @FETCH@ cannot bind parameters, so it is interpolated.
fetchDedupSinglePageStmt
  :: Text  -- ^ cursor name
  -> Int64 -- ^ rows per fetch
  -> Stmt.Statement () [(Int64, ByteString)]
fetchDedupSinglePageStmt cursorName n =
  Stmt.unpreparable sql E.noParams decoder
  where
    sql = T.concat
      [ "FETCH FORWARD ", show n, " FROM ", quoteIdent cursorName ]
    decoder = D.rowList $
      (,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)

-- | Multi-asset variant of 'declareDedupSingleCursorStmt'. The dedup
-- key is @policy <> name@; the caller concatenates after decoding.
declareMultiAssetDedupCursorStmt
  :: Text -- ^ cursor name
  -> Stmt.Statement () ()
declareMultiAssetDedupCursorStmt cursorName =
  Stmt.unpreparable sql E.noParams D.noResult
  where
    sql = mconcat
      [ "DECLARE ", quoteIdent cursorName, " NO SCROLL CURSOR FOR "
      , "SELECT ", multiAssetCols.macId.tcName
      , ", ", multiAssetCols.macPolicy.tcName
      , ", ", multiAssetCols.macName.tcName
      , " FROM ", table multiAssetTableDef
      ]

-- | Multi-asset variant of 'fetchDedupSinglePageStmt'.
fetchMultiAssetDedupPageStmt
  :: Text  -- ^ cursor name
  -> Int64 -- ^ rows per fetch
  -> Stmt.Statement () [(Int64, ByteString, ByteString)]
fetchMultiAssetDedupPageStmt cursorName n =
  Stmt.unpreparable sql E.noParams decoder
  where
    sql = T.concat
      [ "FETCH FORWARD ", show n, " FROM ", quoteIdent cursorName ]
    decoder = D.rowList $
      (,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable D.bytea)

-- | @raw@ is NULL for the abstract DReps; concrete DReps carry the
-- 28-byte hash.
selectDrepHashDedupStmt
  :: Stmt.Statement () [(Int64, Maybe ByteString, Text, Bool)]
selectDrepHashDedupStmt =
  Stmt.unpreparable
    "SELECT id, raw, view, has_script FROM \"drep_hash\""
    E.noParams
    decoder
  where
    decoder = D.rowList $
      (,,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nullable D.bytea)
        <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.bool)

selectCommitteeHashDedupStmt
  :: Stmt.Statement () [(Int64, ByteString, Bool)]
selectCommitteeHashDedupStmt =
  Stmt.unpreparable
    "SELECT id, raw, has_script FROM \"committee_hash\""
    E.noParams
    decoder
  where
    decoder = D.rowList $
      (,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable D.bool)

selectVotingAnchorDedupStmt
  :: Stmt.Statement () [(Int64, Text, ByteString, AnchorType)]
selectVotingAnchorDedupStmt =
  Stmt.unpreparable
    "SELECT id, url, data_hash, type FROM \"voting_anchor\""
    E.noParams
    decoder
  where
    decoder = D.rowList $
      (,,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable anchorTypeDecoder)

-- | Feeds the boot rebuild of 'esGovActionProposalCache' so votes in
-- resumed blocks can resolve their proposal references.
selectGovActionProposalCacheStmt
  :: Stmt.Statement () [(ByteString, Word64, Int64)]
selectGovActionProposalCacheStmt =
  Stmt.unpreparable sql E.noParams decoder
  where
    sql = T.concat
      [ "SELECT t.hash, g.index, g.id"
      , " FROM \"gov_action_proposal\" g"
      , " JOIN \"tx\" t ON t.id = g.tx_id"
      ]
    decoder = D.rowList $
      (,,)
        <$> D.column (D.nonNullable D.bytea)
        <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
        <*> D.column (D.nonNullable D.int8)

-- | Used by 'DbSync.SyncState.Row.fetchBlockHashAtSlot' at boot.
selectBlockHashAtSlotStmt :: Stmt.Statement Word64 (Maybe ByteString)
selectBlockHashAtSlotStmt =
  Stmt.preparable
    "SELECT hash FROM \"block\" WHERE slot_no = $1 LIMIT 1"
    encoder
    (D.rowMaybe (D.column (D.nonNullable D.bytea)))
  where
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @committee.id@ for the originating @gov_action_proposal.id@.
-- 'Nothing' input matches @gov_action_proposal_id IS NULL@ (the
-- genesis committee row).
selectCommitteeByProposalStmt
  :: Stmt.Statement (Maybe Int64) (Maybe Int64)
selectCommitteeByProposalStmt =
  Stmt.preparable sql encoder decoder
  where
    sql =
      "SELECT id FROM \"committee\" \
      \WHERE gov_action_proposal_id IS NOT DISTINCT FROM $1 \
      \ORDER BY id DESC LIMIT 1"
    encoder = E.param (E.nullable E.int8)
    decoder = D.rowMaybe (D.column (D.nonNullable D.int8))

-- | @constitution.id@ for the originating @gov_action_proposal.id@.
selectConstitutionByProposalStmt
  :: Stmt.Statement (Maybe Int64) (Maybe Int64)
selectConstitutionByProposalStmt =
  Stmt.preparable sql encoder decoder
  where
    sql =
      "SELECT id FROM \"constitution\" \
      \WHERE gov_action_proposal_id IS NOT DISTINCT FROM $1 \
      \ORDER BY id DESC LIMIT 1"
    encoder = E.param (E.nullable E.int8)
    decoder = D.rowMaybe (D.column (D.nonNullable D.int8))

updateGovActionRatifiedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionRatifiedStmt = govActionEpochUpdateStmt "ratified_epoch"

updateGovActionEnactedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionEnactedStmt = govActionEpochUpdateStmt "enacted_epoch"

updateGovActionDroppedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionDroppedStmt = govActionEpochUpdateStmt "dropped_epoch"

updateGovActionExpiredStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionExpiredStmt = govActionEpochUpdateStmt "expired_epoch"

-- | @UPDATE gov_action_proposal SET <col> = $2 WHERE id = $1@ shared
-- by the four status-column setters.
govActionEpochUpdateStmt :: Text -> Stmt.Statement (Int64, Word64) ()
govActionEpochUpdateStmt column =
  Stmt.unpreparable sql encoder D.noResult
  where
    sql = T.concat
      [ "UPDATE \"gov_action_proposal\" SET "
      , quoteIdent column
      , " = $2 WHERE id = $1"
      ]
    encoder =
         (fst                              >$< E.param (E.nonNullable E.int8))
      <> ((fromIntegral . snd :: (Int64, Word64) -> Int64)
                                           >$< E.param (E.nonNullable E.int8))
