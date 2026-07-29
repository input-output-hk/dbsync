module DbSync.Compare.SpotCheck
  ( SpotCheckConfig (..)
  , spotCheckChecks
  ) where

import Cardano.Prelude
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import DbSync.Compare.Check (Check (..), CheckItem (..), CheckOutcome (..))
import DbSync.Compare.Connect (DbConn, queryBool, queryMaybeInt, queryRows, queryScalarInt)
import DbSync.Compare.Normalize (asText, bytea, floatText, jsonbCanonical, numericText, timestampEpoch)
import DbSync.Compare.RowCompare (KeyedRow, RowDiff (..), compareRowSets, toKeyedRow)
import System.Random (mkStdGen, randomR)

-- ---------------------------------------------------------------------------
-- * Configuration
-- ---------------------------------------------------------------------------

data SpotCheckConfig = SpotCheckConfig
  { scSeed :: !Int
  , scSamples :: !Int -- ^ Width of the block window sampled per era.
  , scEraFilter :: !(Maybe [Text]) -- ^ Restrict to these eras; Nothing means all.
  }

-- ---------------------------------------------------------------------------
-- * Era boundaries
-- ---------------------------------------------------------------------------

-- Mainnet epoch where each era begins. On other networks pass --eras / the
-- epoch ceiling overrides; the windowing still works, only these starts shift.
data Era = Era
  { eraName :: !Text
  , eraStart :: !Int64
  }

eras :: [Era]
eras =
  [ Era "byron" 0
  , Era "shelley" 208
  , Era "allegra" 236
  , Era "mary" 251
  , Era "alonzo" 290
  , Era "babbage" 365
  , Era "conway" 507
  ]

-- Each era clamped to the epoch ceiling; eras starting above it drop out.
eraRanges :: Int64 -> [(Era, Int64, Int64)]
eraRanges epochCeiling =
  [ (era, eraStart era, end)
  | (era, mNext) <- zip eras (map (Just . eraStart) (drop 1 eras) <> [Nothing])
  , eraStart era <= epochCeiling
  , let end = maybe epochCeiling (\nx -> min (nx - 1) epochCeiling) mNext
  ]

filterEras :: Maybe [Text] -> [(Era, Int64, Int64)] -> [(Era, Int64, Int64)]
filterEras Nothing = identity
filterEras (Just names) = filter (\(e, _, _) -> eraName e `elem` names)

-- ---------------------------------------------------------------------------
-- * Block window
-- ---------------------------------------------------------------------------

-- A contiguous block_no window for one era. block_no is chain truth, so the
-- same window selects the same blocks on both databases even though their
-- surrogate ids differ.
data Window = Window
  { wLo :: !Int64
  , wHi :: !Int64
  }

-- The window resolved to one database's surrogate-id ranges. Every scope
-- predicate is phrased over these PK-indexed id ranges so no query depends on
-- a secondary index on block_no / epoch_no / block_id (the new schema does not
-- carry those). The tx-id bounds come from the cumulative tx_count carried on
-- block, which scans only the (small) block heap.
data Scope = Scope
  { scBlockLo :: !Int64
  , scBlockHi :: !Int64
  , scTxLo :: !Int64
  , scTxHi :: !Int64
  }

-- Seeded start so the same seed picks the same windows; mixes the era start in
-- so different eras get independent windows.
eraSeed :: Int -> Era -> Int
eraSeed seed era = seed * 1000003 + fromIntegral (eraStart era)

-- The sample budget is split across several scattered clusters of varied width
-- rather than one contiguous run, so rare events (governance actions, pool
-- retirements) spread thinly across an era are far more likely to be hit. The
-- weights sum to 1; each cluster's width is its share of 'scSamples', and each
-- is dropped at an independent seeded offset within the era's blocks.
clusterWeights :: [Double]
clusterWeights = [0.34, 0.24, 0.16, 0.10, 0.07, 0.05, 0.025, 0.015]

-- Resolve an era's epoch span to several scattered block_no windows whose widths
-- together make up 'scSamples'. A whole-era span narrower than the budget
-- collapses to a single window covering it.
resolveWindows :: DbConn -> SpotCheckConfig -> Era -> Int64 -> Int64 -> IO [Window]
resolveWindows conn cfg era loEpoch hiEpoch = do
  mMin <- queryMaybeInt conn (rangeSql "min")
  mMax <- queryMaybeInt conn (rangeSql "max")
  pure $ case (mMin, mMax) of
    (Just lo, Just hi)
      | hi >= lo ->
          let budget = max 1 (scSamples cfg)
              span_ = hi - lo + 1
           in if span_ <= fromIntegral budget
                then [Window lo hi]
                else scatter lo hi budget
    _ -> []
  where
    rangeSql agg =
      "SELECT " <> agg <> "(block_no)::bigint FROM block WHERE epoch_no BETWEEN "
        <> show loEpoch <> " AND " <> show hiEpoch

    -- Place each weighted cluster at an independent seeded offset, then merge
    -- any that ended up overlapping so the windows stay disjoint.
    scatter lo hi budget =
      mergeWindows $
        snd $
          foldl' place (mkStdGen (eraSeed (scSeed cfg) era), []) (zip [0 :: Int ..] widths)
      where
        widths = clusterWidths budget
        place (g, acc) (_, w) =
          let maxStart = hi - w + 1
              (start, g') = randomR (lo, maxStart) g
           in (g', Window start (start + w - 1) : acc)

-- Split a sample budget into per-cluster widths by weight, dropping zero-width
-- clusters and giving any rounding remainder to the first.
clusterWidths :: Int -> [Int64]
clusterWidths budget =
  case filter (> 0) raw of
    [] -> [fromIntegral budget]
    (w : ws) -> (w + remainder) : ws
  where
    raw = map (\wt -> round (wt * fromIntegral budget)) clusterWeights
    remainder = fromIntegral budget - sum (filter (> 0) raw)

-- Merge overlapping or touching windows after sorting by start.
mergeWindows :: [Window] -> [Window]
mergeWindows = foldr step [] . sortBy (comparing wLo)
  where
    step w [] = [w]
    step w (n : rest)
      | wHi w + 1 >= wLo n = Window (wLo w) (max (wHi w) (wHi n)) : rest
      | otherwise = w : n : rest

-- Map the chain-truth block_no window to one database's surrogate-id ranges.
-- The block-id range is a cheap PK lookup on both sides. The tx-id range is
-- resolved per side by whichever method is both correct and indexed there:
--
--   * If the database indexes tx(block_id) (the old schema does), take
--     @min/max(tx.id) WHERE block_id BETWEEN@. This is exact even when tx ids
--     have gaps from rolled-back transactions, which the old schema does carry.
--
--   * Otherwise (the new schema) tx ids are gapless and assigned per block in
--     chain order, so the first tx id of a block is one past the cumulative
--     tx_count of every earlier block — a parallel aggregate over the block
--     heap, no secondary index needed.
--
-- The two databases assign different surrogate ids, so this runs once per side.
resolveScope :: DbConn -> Window -> IO Scope
resolveScope conn w = do
  bidLo <- queryScalarInt conn (blockIdSql "min")
  bidHi <- queryScalarInt conn (blockIdSql "max")
  hasIdx <- hasTxBlockIdIndex conn
  (txLo, txHi) <-
    if hasIdx
      then do
        lo <- queryScalarInt conn (txRangeSql "min" bidLo bidHi)
        hi <- queryScalarInt conn (txRangeSql "max" bidLo bidHi)
        pure (lo, hi)
      else do
        before <- queryScalarInt conn (cumTxSql ("id < " <> show bidLo))
        through <- queryScalarInt conn (cumTxSql ("id <= " <> show bidHi))
        pure (before + 1, through)
  pure
    Scope
      { scBlockLo = bidLo
      , scBlockHi = bidHi
      , scTxLo = txLo
      , scTxHi = txHi
      }
  where
    blockIdSql agg =
      "SELECT COALESCE(" <> agg <> "(id), 0)::bigint FROM block WHERE block_no BETWEEN "
        <> show (wLo w) <> " AND " <> show (wHi w)
    txRangeSql agg lo hi =
      "SELECT COALESCE(" <> agg <> "(id), 0)::bigint FROM tx WHERE block_id BETWEEN "
        <> show lo <> " AND " <> show hi
    cumTxSql pred_ =
      "SELECT COALESCE(SUM(tx_count), 0)::bigint FROM block WHERE " <> pred_

-- The whole era as a single block_no window, or none if the era has no blocks
-- below the ceiling. Used by 'Wide' specs (sparse tables) so the full era is
-- scanned rather than the scattered sample.
resolveFullWindow :: DbConn -> Era -> Int64 -> Int64 -> IO [Window]
resolveFullWindow conn _era loEpoch hiEpoch = do
  mMin <- queryMaybeInt conn (rangeSql "min")
  mMax <- queryMaybeInt conn (rangeSql "max")
  pure $ case (mMin, mMax) of
    (Just lo, Just hi) | hi >= lo -> [Window lo hi]
    _ -> []
  where
    rangeSql agg =
      "SELECT " <> agg <> "(block_no)::bigint FROM block WHERE epoch_no BETWEEN "
        <> show loEpoch <> " AND " <> show hiEpoch

-- Whether tx(block_id) is indexed, deciding the tx-range resolution strategy.
hasTxBlockIdIndex :: DbConn -> IO Bool
hasTxBlockIdIndex conn =
  queryBool
    conn
    "SELECT EXISTS (SELECT 1 FROM pg_index i \
    \JOIN pg_class c ON c.oid = i.indrelid \
    \JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY (i.indkey) \
    \WHERE c.relname = 'tx' AND a.attname = 'block_id')"

-- ---------------------------------------------------------------------------
-- * Field specifications
-- ---------------------------------------------------------------------------

data FieldSpec = FieldSpec
  { fsLabel :: !Text
  , fsOld :: !Text -- ^ SQL over alias t producing the old-side value.
  , fsNew :: !Text -- ^ SQL over alias t producing the new-side value.
  }

data Side = OldSide | NewSide

exprFor :: Side -> FieldSpec -> Text
exprFor OldSide = fsOld
exprFor NewSide = fsNew

-- A column compared identically on both sides.
both :: Text -> Text -> FieldSpec
both label expr = FieldSpec label expr expr

plain :: Text -> FieldSpec
plain col = both col (asText ("t." <> col))

numeric :: Text -> FieldSpec
numeric col = both col (numericText ("t." <> col))

hexed :: Text -> FieldSpec
hexed col = both col (bytea ("t." <> col))

jsonb :: Text -> FieldSpec
jsonb col = both col (jsonbCanonical ("t." <> col))

-- FK translations to the parent's natural key (identical SQL on both sides).
stakeRef :: Text -> Text
stakeRef col = bytea ("(SELECT sa.hash_raw FROM stake_address sa WHERE sa.id = t." <> col <> ")")

poolRef :: Text -> Text
poolRef col = bytea ("(SELECT ph.hash_raw FROM pool_hash ph WHERE ph.id = t." <> col <> ")")

assetPolicy :: Text
assetPolicy = bytea "(SELECT ma.policy FROM multi_asset ma WHERE ma.id = t.ident)"

assetName :: Text
assetName = bytea "(SELECT ma.name FROM multi_asset ma WHERE ma.id = t.ident)"

-- Governance / DRep / committee FK translations to their parent natural keys.
drepRef :: Text -> Text
drepRef col = bytea ("(SELECT dh.raw FROM drep_hash dh WHERE dh.id = t." <> col <> ")")

committeeKeyRef :: Text -> Text
committeeKeyRef col = bytea ("(SELECT ch.raw FROM committee_hash ch WHERE ch.id = t." <> col <> ")")

-- A gov action is named by its proposing tx hash plus the action index.
govActionRef :: Text -> Text
govActionRef col =
  bytea
    ( "(SELECT tx.hash FROM gov_action_proposal g JOIN tx ON tx.id = g.tx_id WHERE g.id = t."
        <> col
        <> ")"
    )

govActionIndexRef :: Text -> Text
govActionIndexRef col =
  asText ("(SELECT g.index FROM gov_action_proposal g WHERE g.id = t." <> col <> ")")

-- voting_anchor is content-addressed by url + data_hash; both pin a row.
votingAnchorUrl :: Text -> Text
votingAnchorUrl col = asText ("(SELECT va.url FROM voting_anchor va WHERE va.id = t." <> col <> ")")

votingAnchorHash :: Text -> Text
votingAnchorHash col =
  bytea ("(SELECT va.data_hash FROM voting_anchor va WHERE va.id = t." <> col <> ")")

blockFields :: [FieldSpec]
blockFields =
  [ plain "epoch_no"
  , plain "slot_no"
  , plain "epoch_slot_no"
  , plain "block_no"
  , numeric "size"
  , both "time" (timestampEpoch "t.time")
  , plain "tx_count"
  , plain "proto_major"
  , plain "proto_minor"
  , plain "vrf_key"
  , hexed "op_cert"
  , numeric "op_cert_counter"
  , both "previous_hash" (bytea "(SELECT b2.hash FROM block b2 WHERE b2.id = t.previous_id)")
  , both "slot_leader_hash" (bytea "(SELECT sl.hash FROM slot_leader sl WHERE sl.id = t.slot_leader_id)")
  , both "slot_leader_desc" (asText "(SELECT sl.description FROM slot_leader sl WHERE sl.id = t.slot_leader_id)")
  ]

txFields :: [FieldSpec]
txFields =
  [ plain "block_index"
  , numeric "out_sum"
  , numeric "fee"
  , numeric "deposit"
  , numeric "size"
  , plain "invalid_before"
  , plain "invalid_hereafter"
  , plain "valid_contract"
  , plain "script_size"
  , numeric "treasury_donation"
  ]

txOutKeys :: [FieldSpec]
txOutKeys = [plain "index"]

-- Address columns are inline on the old side and normalised behind address_id
-- on the new side; stake_address joins identically on both.
txOutFields :: [FieldSpec]
txOutFields =
  [ FieldSpec "address" (asText "t.address") (asText "(SELECT a.address FROM address a WHERE a.id = t.address_id)")
  , FieldSpec "has_script" (asText "t.address_has_script") (asText "(SELECT a.has_script FROM address a WHERE a.id = t.address_id)")
  , FieldSpec "payment_cred" (bytea "t.payment_cred") (bytea "(SELECT a.payment_cred FROM address a WHERE a.id = t.address_id)")
  , both "stake_addr" (stakeRef "stake_address_id")
  , numeric "value"
  , hexed "data_hash"
  ]

-- multi_assets_descr is a denormalised human-readable string the two dbsyncs
-- render differently (Show of a MultiAsset map vs a tuple list); the canonical
-- asset data lives in ma_tx_out, so the description is left out of the compare.
collateralTxOutFields :: [FieldSpec]
collateralTxOutFields = txOutFields

-- The consumed output is named by tx hash + index on both sides; old resolves
-- it through tx_out_id, new carries the hash directly.
txInKeys :: [FieldSpec]
txInKeys =
  [ FieldSpec "consumed_tx" (bytea "(SELECT t2.hash FROM tx t2 WHERE t2.id = t.tx_out_id)") (bytea "t.tx_out_hash")
  , plain "tx_out_index"
  ]

-- The hash of the tx that owns the output a row hangs off (via tx_out_id). Both
-- sides resolve identically, so it pins each row to its output across DBs.
txOutOwnerKey :: FieldSpec
txOutOwnerKey =
  both "out_tx" (bytea "(SELECT tx.hash FROM tx JOIN tx_out o ON o.tx_id = tx.id WHERE o.id = t.tx_out_id)")

-- ---------------------------------------------------------------------------
-- * Table specifications
-- ---------------------------------------------------------------------------

-- A table to compare within a block window. 'tsScope' renders the WHERE clause
-- (over alias t) that confines rows to the scattered clusters; 'tsKeyExprs' are
-- the DB-independent natural-key expressions emitted ahead of the comparison
-- fields so 'compareRowSets' can align rows across the two databases.
--
-- 'tsScope' takes /all/ clusters at once. Direct-range specs OR their BETWEENs
-- (a bitmap-OR over a PK). Subquery-scoped specs must fold the clusters into a
-- single @col IN (SELECT ... WHERE <OR of inner ranges>)@: ORing several
-- @col IN (subquery)@ predicates instead defeats the index and seq-scans the
-- whole child table.
-- How widely a table is scanned. High-volume tables (tx_out, ma_tx_out, ...)
-- use 'Sampled' — the scattered clusters — to bound query cost. Sparse tables
-- (governance actions, pool updates) hold so few rows chain-wide that a
-- 'Wide' scan of the whole era is cheap and far more likely to catch the rare
-- events a narrow sample misses.
data Coverage = Sampled | Wide

data TableSpec = TableSpec
  { tsTable :: !Text
  , tsKeyExprs :: ![FieldSpec]
  , tsFields :: ![FieldSpec]
  , tsScope :: [Scope] -> Text
  , tsCoverage :: !Coverage
  }

-- block scopes on its id range; tx on its id range. Both are PK lookups.
blockSpec :: TableSpec
blockSpec =
  TableSpec
    { tsTable = "block"
    , tsKeyExprs = [both "hash" (bytea "t.hash")]
    , tsFields = blockFields
    , tsScope = rangeScope "t.id" scBlockLo scBlockHi
    , tsCoverage = Sampled
    }

txSpec :: TableSpec
txSpec =
  TableSpec
    { tsTable = "tx"
    , tsKeyExprs = [both "hash" (bytea "t.hash")]
    , tsFields = txFields
    , tsScope = rangeScope "t.id" scTxLo scTxHi
    , tsCoverage = Sampled
    }

-- A child table scoped through its tx column. Its key is the owning tx hash
-- plus any in-row natural keys, so rows line up across DBs despite differing
-- surrogate ids.
childSpec :: Text -> Text -> [FieldSpec] -> [FieldSpec] -> TableSpec
childSpec = childSpecCov Sampled

childSpecCov :: Coverage -> Text -> Text -> [FieldSpec] -> [FieldSpec] -> TableSpec
childSpecCov coverage table txCol keys fields =
  TableSpec
    { tsTable = table
    , tsKeyExprs = txHashKey txCol : keys
    , tsFields = fields
    , tsScope = rangeScope ("t." <> txCol) scTxLo scTxHi
    , tsCoverage = coverage
    }

-- A child scoped through tx_out (reference inputs, multi-asset outputs). tx_out
-- ids are not contiguous over a tx-id range, so this confines through the
-- owning tx_id, which sits on the (tx_id, index) index.
txOutChildSpec :: Text -> [FieldSpec] -> [FieldSpec] -> TableSpec
txOutChildSpec table keys fields =
  TableSpec
    { tsTable = table
    , tsKeyExprs = keys
    , tsFields = fields
    , tsScope = subqueryScope "t.tx_out_id" "tx_out" "tx_id" scTxLo scTxHi
    , tsCoverage = Sampled
    }

-- A child scoped through a parent table that carries the tx anchor itself
-- (pool_owner/pool_relay via pool_update; committee/constitution/... via
-- gov_action_proposal). The parent's surrogate ids differ per database, so the
-- scope resolves them through the parent's own tx column inside a subquery.
parentScopedSpec
  :: Text -- ^ child table
  -> Text -- ^ child column pointing at the parent
  -> Text -- ^ parent table
  -> Text -- ^ parent's tx-anchor column
  -> [FieldSpec] -- ^ natural keys (already DB-independent)
  -> [FieldSpec] -- ^ comparison fields
  -> TableSpec
parentScopedSpec table childCol parent parentTxCol keys fields =
  TableSpec
    { tsTable = table
    , tsKeyExprs = keys
    , tsFields = fields
    , tsScope = subqueryScope ("t." <> childCol) parent parentTxCol scTxLo scTxHi
    , tsCoverage = Wide
    }

-- redeemer_data is deduplicated and shared across txs, so the owning tx must
-- come from redeemer.tx_id; the linked datum hash stays a comparison field.
redeemerSpec :: TableSpec
redeemerSpec =
  childSpec "redeemer" "tx_id"
    [plain "purpose", plain "index"]
    [ plain "unit_mem"
    , plain "unit_steps"
    , both "redeemer_data" (bytea "(SELECT rd.hash FROM redeemer_data rd WHERE rd.id = t.redeemer_data_id)")
    ]

txHashKey :: Text -> FieldSpec
txHashKey txCol = both "tx" (bytea ("(SELECT tx.hash FROM tx WHERE tx.id = t." <> txCol <> ")"))

-- The pool hash owning a row that points at pool_update via 'col'.
poolUpdatePoolKey :: Text -> FieldSpec
poolUpdatePoolKey col =
  both
    "pool"
    (bytea ("(SELECT ph.hash_raw FROM pool_update pu JOIN pool_hash ph ON ph.id = pu.hash_id WHERE pu.id = t." <> col <> ")"))

-- The registering tx hash of the pool_update a row points at via 'col'.
poolUpdateTxKey :: Text -> FieldSpec
poolUpdateTxKey col =
  both
    "pool_tx"
    (bytea ("(SELECT tx.hash FROM pool_update pu JOIN tx ON tx.id = pu.registered_tx_id WHERE pu.id = t." <> col <> ")"))

idBetween :: Text -> Int64 -> Int64 -> Text
idBetween col lo hi = col <> " BETWEEN " <> show lo <> " AND " <> show hi

-- Direct range scope: OR one BETWEEN per cluster on an indexed column. The
-- planner turns disjoint ranges into a bitmap-OR index scan.
rangeScope :: Text -> (Scope -> Int64) -> (Scope -> Int64) -> [Scope] -> Text
rangeScope col lo hi scopes =
  parens (orRanges col lo hi scopes)

-- Subquery scope: one @col IN (SELECT id FROM parent WHERE <OR of ranges>)@.
-- Folding every cluster into a single IN keeps the semi-join/nested-loop plan;
-- ORing several @col IN (subquery)@ predicates would force a child seq scan.
subqueryScope :: Text -> Text -> Text -> (Scope -> Int64) -> (Scope -> Int64) -> [Scope] -> Text
subqueryScope col parent parentCol lo hi scopes =
  col <> " IN (SELECT id FROM " <> parent <> " WHERE " <> orRanges parentCol lo hi scopes <> ")"

orRanges :: Text -> (Scope -> Int64) -> (Scope -> Int64) -> [Scope] -> Text
orRanges col lo hi scopes =
  T.intercalate " OR " [parens (idBetween col (lo s) (hi s)) | s <- scopes]

parens :: Text -> Text
parens t = "(" <> t <> ")"

tableSpecs :: [TableSpec]
tableSpecs =
  [ blockSpec
  , txSpec
  , childSpec "tx_out" "tx_id" txOutKeys txOutFields
  , childSpec "tx_in" "tx_in_id" txInKeys []
  , childSpec "collateral_tx_in" "tx_in_id" txInKeys []
  , childSpec "reference_tx_in" "tx_in_id" txInKeys []
  , childSpec "collateral_tx_out" "tx_id" txOutKeys collateralTxOutFields
  , childSpec "withdrawal" "tx_id" [both "addr" (stakeRef "addr_id")] [numeric "amount"]
  , childSpec "tx_metadata" "tx_id" [plain "key"] [jsonb "json", hexed "bytes"]
  , childSpec "ma_tx_mint" "tx_id" [both "policy" assetPolicy, both "name" assetName] [numeric "quantity"]
  , txOutChildSpec
      "ma_tx_out"
      [ txOutOwnerKey
      , both "out_index" (asText "(SELECT o.index FROM tx_out o WHERE o.id = t.tx_out_id)")
      , both "policy" assetPolicy
      , both "name" assetName
      ]
      [numeric "quantity"]
  , redeemerSpec
  , childSpec "redeemer_data" "tx_id" [hexed "hash"] [jsonb "value", hexed "bytes"]
  , childSpec "datum" "tx_id" [hexed "hash"] [jsonb "value", hexed "bytes"]
  , childSpec "script" "tx_id" [hexed "hash"] [plain "type", jsonb "json", hexed "bytes", plain "serialised_size"]
  , childSpec "extra_key_witness" "tx_id" [hexed "hash"] []
  , childSpec "stake_registration" "tx_id" [plain "cert_index"] [both "addr" (stakeRef "addr_id"), plain "epoch_no", numeric "deposit"]
  , childSpec "stake_deregistration" "tx_id" [plain "cert_index"] [both "addr" (stakeRef "addr_id"), plain "epoch_no"]
  , childSpec
      "delegation"
      "tx_id"
      [plain "cert_index"]
      [both "addr" (stakeRef "addr_id"), both "pool" (poolRef "pool_hash_id"), plain "active_epoch_no", plain "slot_no"]
  , childSpecCov
      Wide
      "pool_update"
      "registered_tx_id"
      [plain "cert_index"]
      [ both "pool" (poolRef "hash_id")
      , hexed "vrf_key_hash"
      , numeric "pledge"
      , plain "active_epoch_no"
      , both "margin" (asText "t.margin::double precision")
      , numeric "fixed_cost"
      , both "reward_addr" (stakeRef "reward_addr_id")
      , numeric "deposit"
      ]
  , childSpecCov Wide "pool_retire" "announced_tx_id" [plain "cert_index"] [both "pool" (poolRef "hash_id"), plain "retiring_epoch"]
  ]
    <> poolDetailSpecs
    <> governanceSpecs

-- ---------------------------------------------------------------------------
-- * Pool-detail table specs
-- ---------------------------------------------------------------------------

-- pool_owner / pool_relay hang off pool_update; pool_metadata_ref carries its
-- own registering tx. Each is keyed by the owning pool plus an in-row field.
poolDetailSpecs :: [TableSpec]
poolDetailSpecs =
  [ parentScopedSpec
      "pool_owner"
      "pool_update_id"
      "pool_update"
      "registered_tx_id"
      [poolUpdatePoolKey "pool_update_id", poolUpdateTxKey "pool_update_id", both "owner" (stakeRef "addr_id")]
      []
  , parentScopedSpec
      "pool_relay"
      "update_id"
      "pool_update"
      "registered_tx_id"
      [poolUpdatePoolKey "update_id", poolUpdateTxKey "update_id", plain "ipv4", plain "ipv6", plain "dns_name", plain "dns_srv_name"]
      [plain "port"]
  , childSpecCov
      Wide
      "pool_metadata_ref"
      "registered_tx_id"
      [both "pool" (poolRef "pool_id"), hexed "hash"]
      [plain "url"]
  ]

-- ---------------------------------------------------------------------------
-- * Governance table specs
-- ---------------------------------------------------------------------------

-- All Conway-era. Each is scoped through its tx anchor (directly or through
-- gov_action_proposal) and keyed by DB-independent natural keys: tx hash,
-- cert/action index, and translated parent hashes.
governanceSpecs :: [TableSpec]
governanceSpecs =
  [ childSpecCov
      Wide
      "gov_action_proposal"
      "tx_id"
      [plain "index"]
      [ numeric "deposit"
      , both "return_address" (stakeRef "return_address")
      , plain "expiration"
      , both "anchor_url" (votingAnchorUrl "voting_anchor_id")
      , both "anchor_hash" (votingAnchorHash "voting_anchor_id")
      , plain "type"
      , jsonb "description"
      , plain "ratified_epoch"
      , plain "enacted_epoch"
      , plain "dropped_epoch"
      , plain "expired_epoch"
      ]
  , childSpecCov
      Wide
      "voting_procedure"
      "tx_id"
      [plain "index", both "gov_action" (govActionRef "gov_action_proposal_id"), both "gov_action_index" (govActionIndexRef "gov_action_proposal_id")]
      [ plain "voter_role"
      , both "drep_voter" (drepRef "drep_voter")
      , both "pool_voter" (poolRef "pool_voter")
      , both "committee_voter" (committeeKeyRef "committee_voter")
      , plain "vote"
      , both "anchor_url" (votingAnchorUrl "voting_anchor_id")
      ]
  , childSpecCov
      Wide
      "drep_registration"
      "tx_id"
      [plain "cert_index", both "drep" (drepRef "drep_hash_id")]
      [numeric "deposit", both "anchor_url" (votingAnchorUrl "voting_anchor_id")]
  , childSpecCov
      Wide
      "delegation_vote"
      "tx_id"
      [plain "cert_index", both "addr" (stakeRef "addr_id"), both "drep" (drepRef "drep_hash_id")]
      []
  , childSpecCov
      Wide
      "committee_registration"
      "tx_id"
      [plain "cert_index"]
      [both "cold_key" (committeeKeyRef "cold_key_id"), both "hot_key" (committeeKeyRef "hot_key_id")]
  , childSpecCov
      Wide
      "committee_de_registration"
      "tx_id"
      [plain "cert_index"]
      [both "cold_key" (committeeKeyRef "cold_key_id"), both "anchor_url" (votingAnchorUrl "voting_anchor_id")]
  , parentScopedSpec
      "constitution"
      "gov_action_proposal_id"
      "gov_action_proposal"
      "tx_id"
      [both "gov_action" (govActionRef "gov_action_proposal_id"), both "gov_action_index" (govActionIndexRef "gov_action_proposal_id")]
      [both "anchor_url" (votingAnchorUrl "voting_anchor_id"), hexed "script_hash"]
  , parentScopedSpec
      "treasury_withdrawal"
      "gov_action_proposal_id"
      "gov_action_proposal"
      "tx_id"
      [both "gov_action" (govActionRef "gov_action_proposal_id"), both "stake_addr" (stakeRef "stake_address_id")]
      [numeric "amount"]
  , parentScopedSpec
      "committee"
      "gov_action_proposal_id"
      "gov_action_proposal"
      "tx_id"
      [both "gov_action" (govActionRef "gov_action_proposal_id"), both "gov_action_index" (govActionIndexRef "gov_action_proposal_id")]
      [plain "quorum_numerator", plain "quorum_denominator"]
  , childSpecCov
      Wide
      "param_proposal"
      "registered_tx_id"
      [plain "epoch_no", hexed "key"]
      paramProposalFields
  ]

-- param_proposal stores protocol-parameter ratios as double precision in the
-- old schema and as exact numeric in the new one, and a few
-- governance-threshold columns were renamed (dvt_p_p_* -> dvt_pp_*). Compare
-- every shared parameter, casting both sides to double precision so the float
-- and numeric encodings line up, and map the renamed columns per side.
paramProposalFields :: [FieldSpec]
paramProposalFields =
  map numeric integerParams
    <> map ratio sharedRatioParams
    <> [ renamedRatio "dvt_pp_network_group" "dvt_p_p_network_group"
       , renamedRatio "dvt_pp_economic_group" "dvt_p_p_economic_group"
       , renamedRatio "dvt_pp_technical_group" "dvt_p_p_technical_group"
       , renamedRatio "dvt_pp_gov_group" "dvt_p_p_gov_group"
       ]
  where
    integerParams =
      [ "min_fee_a", "min_fee_b", "max_block_size", "max_tx_size", "max_bh_size"
      , "key_deposit", "pool_deposit", "max_epoch", "optimal_pool_count"
      , "min_utxo_value", "min_pool_cost", "protocol_major", "protocol_minor"
      , "max_tx_ex_mem", "max_tx_ex_steps", "max_block_ex_mem", "max_block_ex_steps"
      , "max_val_size", "collateral_percent", "max_collateral_inputs"
      , "coins_per_utxo_size", "committee_min_size", "committee_max_term_length"
      , "gov_action_lifetime", "gov_action_deposit", "drep_deposit", "drep_activity"
      ]
    sharedRatioParams =
      [ "influence", "monetary_expand_rate", "treasury_growth_rate", "decentralisation"
      , "price_mem", "price_step"
      , "pvt_motion_no_confidence", "pvt_committee_normal", "pvt_committee_no_confidence"
      , "pvt_hard_fork_initiation", "pvtpp_security_group"
      , "dvt_motion_no_confidence", "dvt_committee_normal", "dvt_committee_no_confidence"
      , "dvt_update_to_constitution", "dvt_hard_fork_initiation", "dvt_treasury_withdrawal"
      , "min_fee_ref_script_cost_per_byte"
      ]
    -- A ratio compared as double precision so old (float) and new (numeric) agree.
    ratio col = both col (floatText ("t." <> col))
    renamedRatio newCol oldCol = FieldSpec newCol (floatText ("t." <> oldCol)) (floatText ("t." <> newCol))

-- ---------------------------------------------------------------------------
-- * SQL and fetching
-- ---------------------------------------------------------------------------

-- One query per (table, side) covering all scattered clusters. The spec's
-- 'tsScope' folds the clusters into an index-friendly predicate (see
-- 'TableSpec').
selectSql :: Side -> TableSpec -> [Scope] -> Text
selectSql side ts scopes =
  "SELECT "
    <> T.intercalate ", " (map (exprFor side) (tsKeyExprs ts <> tsFields ts))
    <> " FROM "
    <> tsTable ts
    <> " t WHERE "
    <> tsScope ts scopes

fetchRows :: DbConn -> Side -> TableSpec -> [Scope] -> IO [KeyedRow]
fetchRows _ _ _ [] = pure []
fetchRows conn side ts scopes = do
  let keyCount = length (tsKeyExprs ts)
      labels = map fsLabel (tsFields ts)
  rows <- queryRows conn (keyCount + length labels) (selectSql side ts scopes)
  pure (map (toKeyedRow keyCount labels) rows)

-- ---------------------------------------------------------------------------
-- * Check construction
-- ---------------------------------------------------------------------------

-- The windows and their per-side id ranges for one era. Sampled specs use the
-- scattered clusters; Wide specs use the single whole-era window. Resolving
-- them scans the block heap, so it is memoised and shared by every table check
-- of that era.
data EraScope = EraScope
  { esWindows :: ![Window]
  , esOld :: ![Scope]
  , esNew :: ![Scope]
  , esFullWindows :: ![Window]
  , esFullOld :: ![Scope]
  , esFullNew :: ![Scope]
  }

-- The windows and per-side scopes a spec uses, by its coverage.
coverageScopes :: Coverage -> EraScope -> ([Window], [Scope], [Scope])
coverageScopes Sampled es = (esWindows es, esOld es, esNew es)
coverageScopes Wide es = (esFullWindows es, esFullOld es, esFullNew es)

-- Build every (era, table) check below the epoch ceiling, grouped under a
-- top-level section and one sub-section per era. The per-era window and id
-- ranges are resolved once (first table check of the era) and cached; each
-- table check then runs exactly one query per side and diffs in memory.
spotCheckChecks :: SpotCheckConfig -> Int64 -> IO [CheckItem]
spotCheckChecks cfg epochCeiling = do
  caches <-
    forM activeEras $ \entry -> do
      ref <- newIORef Nothing
      pure (entry, ref)
  pure $
    Section "spot checks"
      : concat
        [ Section ("era " <> eraName era)
            : [Item (mkCheck entry ref ts) | ts <- tableSpecs]
        | (entry@(era, _, _), ref) <- caches
        ]
  where
    activeEras = filterEras (scEraFilter cfg) (eraRanges epochCeiling)

    mkCheck (era, loEpoch, hiEpoch) ref ts =
      Check
        { chkLabel = tsTable ts
        , chkRun = \oldDb newDb -> do
            mEra <- resolveEraScope oldDb newDb ref era loEpoch hiEpoch
            case mEra of
              Nothing -> pure (Skipped "no blocks in era")
              Just es -> do
                let (windows, oldScopes, newScopes) = coverageScopes (tsCoverage ts) es
                oldRows <- fetchRows oldDb OldSide ts oldScopes
                newRows <- fetchRows newDb NewSide ts newScopes
                let diffs = compareRowSets oldRows newRows
                pure (summarise windows (length oldRows) (length newRows) diffs)
        }

    -- Resolve once per era and memoise; later table checks reuse the cache.
    -- The ref's outer Maybe records whether resolution has run; the inner
    -- Maybe is the result (Nothing = era has no blocks below the ceiling).
    resolveEraScope oldDb newDb ref era loEpoch hiEpoch = do
      cached <- readIORef ref
      case cached of
        Just memo -> pure memo
        Nothing -> do
          windows <- resolveWindows oldDb cfg era loEpoch hiEpoch
          memo <- case windows of
            [] -> pure Nothing
            ws -> do
              oldScopes <- mapM (resolveScope oldDb) ws
              newScopes <- mapM (resolveScope newDb) ws
              fullWs <- resolveFullWindow oldDb era loEpoch hiEpoch
              fullOld <- mapM (resolveScope oldDb) fullWs
              fullNew <- mapM (resolveScope newDb) fullWs
              pure (Just (EraScope ws oldScopes newScopes fullWs fullOld fullNew))
          writeIORef ref (Just memo)
          pure memo

-- An empty window on both sides proves nothing, so it is reported as info (a
-- dim '--' line) rather than a green pass. A green 'ok' therefore always means
-- at least one row was actually compared and matched. Every line carries both
-- counts so emptiness is shown on both sides, never inferred from one.
summarise :: [Window] -> Int -> Int -> [RowDiff] -> CheckOutcome
summarise ws oldCount newCount diffs
  | oldCount == 0 && newCount == 0 = Info (windowTag ws <> "empty " <> counts)
  | null diffs = Ok (windowTag ws <> counts)
  | otherwise =
      Diff (windowTag ws <> show (length diffs) <> " diffs " <> counts : map describeDiff (take 10 diffs))
  where
    counts = "(old=" <> show oldCount <> " new=" <> show newCount <> ")"

-- Show the overall block span plus how many scattered clusters cover it.
windowTag :: [Window] -> Text
windowTag [] = "no blocks: "
windowTag ws =
  "blocks " <> show (minimum (map wLo ws)) <> "-" <> show (maximum (map wHi ws))
    <> " (" <> show (length ws) <> " clusters): "

describeDiff :: RowDiff -> Text
describeDiff = \case
  MissingInNew k -> "missing in new: " <> keyText k
  MissingInOld k -> "missing in old: " <> keyText k
  FieldMismatch k f o n ->
    keyText k <> " " <> f <> ": old=" <> valText o <> " new=" <> valText n
  where
    keyText = T.intercalate "/"
    valText = fromMaybe "NULL"
