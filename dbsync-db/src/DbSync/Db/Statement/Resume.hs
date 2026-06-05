{-# LANGUAGE OverloadedStrings #-}

-- | Parameterised hasql 'Statement's used at boot to clean up rows
-- past the resume point.
--
-- Each statement is built per-table (table name interpolated into
-- the SQL); the @WHERE@ predicate is parameterised. The @$1@
-- placeholder carries the slot number or counter as appropriate.
--
-- Table names come from 'tdName' on the static 'TableDef's defined
-- by this package, so identifier interpolation is safe — no
-- user-controlled input flows into the SQL.
module DbSync.Db.Statement.Resume
  ( -- * Cleanup deletes
    deleteBySlotStmt
  , deleteByBlockSlotStmt
  , deleteByIdCounterStmt

    -- * Dedup-map rebuild selects
  , selectDedupSingleStmt
  , selectMultiAssetDedupStmt
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

import DbSync.Db.Sql (quoteIdent)
import DbSync.Db.Types (AnchorType, anchorTypeDecoder)

-- | @DELETE FROM <table> WHERE slot_no > $1@. Returns rows affected.
-- For tables that carry @slot_no@ directly.
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
-- WHERE slot_no > $1)@. Returns rows affected. For tables that
-- reference @block@ via @block_id@ but don't carry @slot_no@.
deleteByBlockSlotStmt :: Text -> Stmt.Statement Word64 Int64
deleteByBlockSlotStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE block_id IN (SELECT id FROM \"block\" WHERE slot_no > $1)"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @DELETE FROM <table> WHERE id >= $1@. Returns rows affected.
-- The @$1@ parameter is the corresponding @*_id_counter@ from
-- @dbsync_sync_state@ (\"next id to assign\"); the resume cleanup
-- uses this to prune rows the previous run wrote past the
-- last-recorded counter — covers both dedup tables and any other
-- counter-tracked data table that lacks a slot or block reference.
deleteByIdCounterStmt :: Text -> Stmt.Statement Int64 Int64
deleteByIdCounterStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE id >= $1"
      ]
    encoder = E.param (E.nonNullable E.int8)

-- | @SELECT id, <keyCol> FROM <table>@. Returns @(id, key)@ pairs
-- as a list. For dedup tables whose natural key is a single column
-- (slot_leader.hash, stake_address.hash_raw, pool_hash.hash_raw).
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

-- | @SELECT id, policy, name FROM multi_asset@. The dedup key is
-- @policy <> name@; the wrapper concatenates after decoding.
selectMultiAssetDedupStmt :: Stmt.Statement () [(Int64, ByteString, ByteString)]
selectMultiAssetDedupStmt =
  Stmt.unpreparable
    "SELECT id, policy, name FROM \"multi_asset\""
    E.noParams
    decoder
  where
    decoder = D.rowList $
      (,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable D.bytea)

-- | @SELECT id, raw, view, has_script FROM drep_hash@. @raw@ is
-- NULL for the abstract DReps; concrete DReps carry the 28-byte hash.
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

-- | @SELECT id, raw, has_script FROM committee_hash@.
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

-- | @SELECT id, url, data_hash, type FROM voting_anchor@.
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

-- | @SELECT tx.hash, gov_action_proposal.index, gov_action_proposal.id
-- FROM gov_action_proposal JOIN tx ON tx.id = gov_action_proposal.tx_id@.
-- Feeds the boot rebuild of 'esGovActionProposalCache' so vote rows
-- in resumed blocks can resolve their proposal references.
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

-- | @SELECT hash FROM block WHERE slot_no = $1 LIMIT 1@. Used by
-- 'DbSync.SyncState.Row.fetchBlockHashAtSlot' at boot.
selectBlockHashAtSlotStmt :: Stmt.Statement Word64 (Maybe ByteString)
selectBlockHashAtSlotStmt =
  Stmt.preparable
    "SELECT hash FROM \"block\" WHERE slot_no = $1 LIMIT 1"
    encoder
    (D.rowMaybe (D.column (D.nonNullable D.bytea)))
  where
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Look up @committee.id@ by its originating @gov_action_proposal.id@.
-- 'Nothing' input matches rows with @gov_action_proposal_id IS NULL@
-- (the genesis committee row).
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

-- | Look up @constitution.id@ by its originating @gov_action_proposal.id@.
-- 'Nothing' input matches rows with @gov_action_proposal_id IS NULL@.
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

-- | Set @gov_action_proposal.ratified_epoch@ on the given row.
updateGovActionRatifiedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionRatifiedStmt = govActionEpochUpdateStmt "ratified_epoch"

-- | Set @gov_action_proposal.enacted_epoch@ on the given row.
updateGovActionEnactedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionEnactedStmt = govActionEpochUpdateStmt "enacted_epoch"

-- | Set @gov_action_proposal.dropped_epoch@ on the given row.
updateGovActionDroppedStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionDroppedStmt = govActionEpochUpdateStmt "dropped_epoch"

-- | Set @gov_action_proposal.expired_epoch@ on the given row.
updateGovActionExpiredStmt :: Stmt.Statement (Int64, Word64) ()
updateGovActionExpiredStmt = govActionEpochUpdateStmt "expired_epoch"

-- | Single-row @UPDATE gov_action_proposal SET <col> = $2 WHERE id = $1@
-- shared by the four status-column setters.
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
