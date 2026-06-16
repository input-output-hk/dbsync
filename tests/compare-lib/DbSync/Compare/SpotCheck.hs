module DbSync.Compare.SpotCheck
  ( SpotCheckConfig (..)
  , SpotCheckResult (..)
  , EraResult (..)
  , runSpotCheck
  , renderSpotCheck
  , spotCheckHasMismatch
  , eraRanges
  ) where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import DbSync.Compare.Connect (DbConn, queryMaybeInt, queryRows)
import DbSync.Compare.Report (dim, green, header, padRight, putLine, red)
import DbSync.Compare.RowCompare (KeyedRow (..), RowDiff (..), compareRowSets, toKeyedRow)
import System.Random (mkStdGen, randomR)

-- ---------------------------------------------------------------------------
-- * Configuration and results
-- ---------------------------------------------------------------------------

data SpotCheckConfig = SpotCheckConfig
  { scSeed :: !Int
  , scSamples :: !Int
  , scEraFilter :: !(Maybe [Text]) -- ^ Restrict to these eras; Nothing means all.
  }

data SpotCheckResult = SpotCheckResult
  { scCeiling :: !Int64
  , scSkippedTables :: ![Text] -- ^ Child tables skipped because the new DB lacks a scope-column index.
  , scEras :: ![EraResult]
  }
  deriving stock (Eq, Show)

data EraResult = EraResult
  { erEra :: !Text
  , erSampled :: !Int -- ^ Distinct block_nos drawn from the old DB.
  , erMatched :: !Int -- ^ Sampled blocks found by hash in both DBs.
  , erBlockDiffs :: ![RowDiff]
  , erTableDiffs :: ![(Text, [RowDiff])]
  }
  deriving stock (Eq, Show)

totalEraDiffs :: EraResult -> Int
totalEraDiffs er =
  length (erBlockDiffs er) + sum (map (length . snd) (erTableDiffs er))

spotCheckHasMismatch :: SpotCheckResult -> Bool
spotCheckHasMismatch = any ((> 0) . totalEraDiffs) . scEras

-- ---------------------------------------------------------------------------
-- * Era boundaries
-- ---------------------------------------------------------------------------

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

-- Each era clamped to the ceiling; eras starting above it drop out.
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
-- * Sampling
-- ---------------------------------------------------------------------------

eraSeed :: Int -> Era -> Int
eraSeed seed era = seed * 1000003 + fromIntegral (eraStart era)

-- Distinct block_nos in [lo, hi]; the whole range when it is no larger than n.
sampleBlockNos :: Int -> Int -> Int64 -> Int64 -> [Int64]
sampleBlockNos seed n lo hi
  | hi < lo = []
  | hi - lo + 1 <= fromIntegral n = [lo .. hi]
  | otherwise = Set.toList (go Set.empty (mkStdGen seed))
  where
    go acc g
      | Set.size acc >= n = acc
      | otherwise =
          let (x, g') = randomR (lo, hi) g
           in go (Set.insert x acc) g'

sampleEra :: DbConn -> Int -> Int -> Era -> Int64 -> Int64 -> IO [Int64]
sampleEra conn seed n era lo hi = do
  mMin <- queryMaybeInt conn (rangeSql "min" lo hi)
  mMax <- queryMaybeInt conn (rangeSql "max" lo hi)
  pure $ case (mMin, mMax) of
    (Just a, Just b) -> sampleBlockNos (eraSeed seed era) n a b
    _ -> []
  where
    rangeSql agg loE hiE =
      "SELECT " <> agg <> "(block_no)::bigint FROM block WHERE epoch_no BETWEEN "
        <> show loE <> " AND " <> show hiE

-- ---------------------------------------------------------------------------
-- * Field specifications
-- ---------------------------------------------------------------------------

-- (label, old-side SQL over alias t, new-side SQL over alias t)
type FieldSpec = (Text, Text, Text)

data Side = OldSide | NewSide

exprFor :: Side -> FieldSpec -> Text
exprFor OldSide (_, o, _) = o
exprFor NewSide (_, _, n) = n

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- A column compared identically on both sides.
both :: Text -> Text -> FieldSpec
both label expr = (label, expr, expr)

plain :: Text -> FieldSpec
plain col = both col ("t." <> col <> "::text")

hexed :: Text -> FieldSpec
hexed col = both col (hexExpr col)

hexExpr :: Text -> Text
hexExpr col = "encode(t." <> col <> ", 'hex')"

-- FK translations to the parent's natural key (identical SQL on both sides).
stakeRef :: Text -> Text
stakeRef col = "encode((SELECT sa.hash_raw FROM stake_address sa WHERE sa.id = t." <> col <> "), 'hex')"

poolRef :: Text -> Text
poolRef col = "encode((SELECT ph.hash_raw FROM pool_hash ph WHERE ph.id = t." <> col <> "), 'hex')"

assetPolicy :: Text
assetPolicy = "encode((SELECT ma.policy FROM multi_asset ma WHERE ma.id = t.ident), 'hex')"

assetName :: Text
assetName = "encode((SELECT ma.name FROM multi_asset ma WHERE ma.id = t.ident), 'hex')"

blockFields :: [FieldSpec]
blockFields =
  [ plain "epoch_no"
  , plain "slot_no"
  , plain "epoch_slot_no"
  , plain "block_no"
  , plain "size"
  , plain "time"
  , plain "tx_count"
  , plain "proto_major"
  , plain "proto_minor"
  , plain "vrf_key"
  , hexed "op_cert"
  , plain "op_cert_counter"
  , both "previous_hash" "encode((SELECT b2.hash FROM block b2 WHERE b2.id = t.previous_id), 'hex')"
  , both "slot_leader_hash" "encode((SELECT sl.hash FROM slot_leader sl WHERE sl.id = t.slot_leader_id), 'hex')"
  , both "slot_leader_desc" "(SELECT sl.description FROM slot_leader sl WHERE sl.id = t.slot_leader_id)::text"
  ]

txFields :: [FieldSpec]
txFields =
  map
    plain
    [ "block_index"
    , "out_sum"
    , "fee"
    , "deposit"
    , "size"
    , "invalid_before"
    , "invalid_hereafter"
    , "valid_contract"
    , "script_size"
    , "treasury_donation"
    ]

-- Address columns are inline on the old side and normalised behind address_id
-- on the new side; stake_address joins identically on both.
txOutKeys :: [FieldSpec]
txOutKeys = [plain "index"]

txOutFields :: [FieldSpec]
txOutFields =
  [ ("address", "t.address::text", "(SELECT a.address FROM address a WHERE a.id = t.address_id)::text")
  , ("has_script", "t.address_has_script::text", "(SELECT a.has_script FROM address a WHERE a.id = t.address_id)::text")
  , ("payment_cred", "encode(t.payment_cred, 'hex')", "encode((SELECT a.payment_cred FROM address a WHERE a.id = t.address_id), 'hex')")
  , both "stake_addr" (stakeRef "stake_address_id")
  , plain "value"
  , hexed "data_hash"
  ]

-- Collateral outputs share tx_out's shape and add the multi-asset summary.
collateralTxOutFields :: [FieldSpec]
collateralTxOutFields = txOutFields <> [plain "multi_assets_descr"]

-- The consumed output is named by tx hash + index on both sides; old resolves
-- it through tx_out_id, new carries the hash directly.
txInKeys :: [FieldSpec]
txInKeys =
  [ ("consumed_tx", "encode((SELECT t2.hash FROM tx t2 WHERE t2.id = t.tx_out_id), 'hex')", "encode(t.tx_out_hash, 'hex')")
  , plain "tx_out_index"
  ]

-- ---------------------------------------------------------------------------
-- * Child-table specifications
-- ---------------------------------------------------------------------------

data ChildSpec = ChildSpec
  { csTable :: !Text
  , csScopeCol :: !Text -- ^ Column the per-tx query filters on; must be indexed on new.
  , csKeys :: ![FieldSpec]
  , csFields :: ![FieldSpec]
  , csScopePred :: !(Text -> Text) -- ^ Parent id (as text) to a WHERE predicate over alias t.
  }

eqPred :: Text -> Text -> Text
eqPred col pid = col <> " = " <> pid

childSpecs :: [ChildSpec]
childSpecs =
  [ ChildSpec "tx_out" "tx_id" txOutKeys txOutFields (eqPred "t.tx_id")
  , ChildSpec "tx_in" "tx_in_id" txInKeys [] (eqPred "t.tx_in_id")
  , ChildSpec "collateral_tx_in" "tx_in_id" txInKeys [] (eqPred "t.tx_in_id")
  , ChildSpec "reference_tx_in" "tx_in_id" txInKeys [] (eqPred "t.tx_in_id")
  , ChildSpec "collateral_tx_out" "tx_id" txOutKeys collateralTxOutFields (eqPred "t.tx_id")
  , ChildSpec "withdrawal" "tx_id" [both "addr" (stakeRef "addr_id")] [plain "amount"] (eqPred "t.tx_id")
  , ChildSpec "tx_metadata" "tx_id" [plain "key"] [plain "json", hexed "bytes"] (eqPred "t.tx_id")
  , ChildSpec "ma_tx_mint" "tx_id" [both "policy" assetPolicy, both "name" assetName] [plain "quantity"] (eqPred "t.tx_id")
  , ChildSpec
      "ma_tx_out"
      "tx_out_id"
      [ both "out_index" "(SELECT o.index FROM tx_out o WHERE o.id = t.tx_out_id)::text"
      , both "policy" assetPolicy
      , both "name" assetName
      ]
      [plain "quantity"]
      (\pid -> "t.tx_out_id IN (SELECT id FROM tx_out WHERE tx_id = " <> pid <> ")")
  , ChildSpec
      "redeemer"
      "tx_id"
      [plain "purpose", plain "index"]
      [ plain "unit_mem"
      , plain "unit_steps"
      , both "redeemer_data" "encode((SELECT rd.hash FROM redeemer_data rd WHERE rd.id = t.redeemer_data_id), 'hex')"
      ]
      (eqPred "t.tx_id")
  , ChildSpec "redeemer_data" "tx_id" [hexed "hash"] [plain "value", hexed "bytes"] (eqPred "t.tx_id")
  , ChildSpec "datum" "tx_id" [hexed "hash"] [plain "value", hexed "bytes"] (eqPred "t.tx_id")
  , ChildSpec "script" "tx_id" [hexed "hash"] [plain "type", plain "json", hexed "bytes", plain "serialised_size"] (eqPred "t.tx_id")
  , ChildSpec "extra_key_witness" "tx_id" [hexed "hash"] [] (eqPred "t.tx_id")
  , ChildSpec "stake_registration" "tx_id" [plain "cert_index"] [both "addr" (stakeRef "addr_id"), plain "epoch_no", plain "deposit"] (eqPred "t.tx_id")
  , ChildSpec "stake_deregistration" "tx_id" [plain "cert_index"] [both "addr" (stakeRef "addr_id"), plain "epoch_no"] (eqPred "t.tx_id")
  , ChildSpec
      "delegation"
      "tx_id"
      [plain "cert_index"]
      [both "addr" (stakeRef "addr_id"), both "pool" (poolRef "pool_hash_id"), plain "active_epoch_no", plain "slot_no"]
      (eqPred "t.tx_id")
  , ChildSpec
      "pool_update"
      "registered_tx_id"
      [plain "cert_index"]
      [ both "pool" (poolRef "hash_id")
      , hexed "vrf_key_hash"
      , plain "pledge"
      , plain "active_epoch_no"
      , both "margin" "t.margin::double precision::text"
      , plain "fixed_cost"
      , both "reward_addr" (stakeRef "reward_addr_id")
      , plain "deposit"
      ]
      (eqPred "t.registered_tx_id")
  , ChildSpec "pool_retire" "announced_tx_id" [plain "cert_index"] [both "pool" (poolRef "hash_id"), plain "retiring_epoch"] (eqPred "t.announced_tx_id")
  ]

tableOrder :: [Text]
tableOrder = "tx" : map csTable childSpecs

-- ---------------------------------------------------------------------------
-- * Row fetching
-- ---------------------------------------------------------------------------

-- A block or tx reduced to its hash, this DB's surrogate id (for scoping
-- children), and the fields worth comparing.
data Probe = Probe
  { prHashHex :: !Text
  , prId :: !Text
  , prRow :: !KeyedRow
  }

fetchProbes :: DbConn -> [FieldSpec] -> Text -> IO [Probe]
fetchProbes conn fields sql = do
  rows <- queryRows conn (2 + length fields) sql
  pure (mapMaybe mk rows)
  where
    labels = map fst3 fields
    mk (h : i : rest) =
      Just
        Probe
          { prHashHex = fromMaybe "" h
          , prId = fromMaybe "" i
          , prRow = KeyedRow [fromMaybe "" h] (zip labels rest)
          }
    mk _ = Nothing

fetchKeyed :: DbConn -> Int -> [Text] -> Text -> IO [KeyedRow]
fetchKeyed conn keyCount labels sql = do
  rows <- queryRows conn (keyCount + length labels) sql
  pure (map (toKeyedRow keyCount labels) rows)

probeSql :: Side -> Text -> [FieldSpec] -> Text -> Text
probeSql side table fields predicate =
  "SELECT encode(t.hash, 'hex'), t.id::text, "
    <> T.intercalate ", " (map (exprFor side) fields)
    <> " FROM "
    <> table
    <> " t WHERE "
    <> predicate

childSql :: Side -> Text -> [FieldSpec] -> [FieldSpec] -> Text -> Text
childSql side table keys fields predicate =
  "SELECT "
    <> T.intercalate ", " (map (exprFor side) (keys <> fields))
    <> " FROM "
    <> table
    <> " t WHERE "
    <> predicate

inIntPred :: Text -> [Int64] -> Text
inIntPred col xs = col <> " IN (" <> T.intercalate "," (map show xs) <> ")"

inHashPred :: Text -> [Text] -> Text
inHashPred col hs = col <> " IN (" <> T.intercalate "," (map hexLit hs) <> ")"

hexLit :: Text -> Text
hexLit h = "'\\x" <> h <> "'::bytea"

-- Leading index columns present in a DB, as (table, column). A per-tx child
-- query must filter on a column indexed in BOTH DBs or it seq-scans one side.
indexedScopeCols :: DbConn -> IO (Set.Set (Text, Text))
indexedScopeCols conn = do
  rows <- queryRows conn 2 sql
  pure (Set.fromList [(tbl, col) | [Just tbl, Just col] <- rows])
  where
    sql =
      "SELECT c.relname::text, a.attname::text FROM pg_index i "
        <> "JOIN pg_class c ON c.oid = i.indrelid "
        <> "JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = i.indkey[0] "
        <> "WHERE c.relnamespace = 'public'::regnamespace"

-- ---------------------------------------------------------------------------
-- * Traversal
-- ---------------------------------------------------------------------------

runSpotCheck :: DbConn -> DbConn -> SpotCheckConfig -> Int64 -> IO SpotCheckResult
runSpotCheck oldConn newConn cfg epochCeiling = do
  idxSet <- Set.intersection <$> indexedScopeCols oldConn <*> indexedScopeCols newConn
  let isIndexed cs = Set.member (csTable cs, csScopeCol cs) idxSet
      activeSpecs = filter isIndexed childSpecs
      skipped = [csTable cs | cs <- childSpecs, not (isIndexed cs)]
      activeEras = filterEras (scEraFilter cfg) (eraRanges epochCeiling)

  perEra <- for activeEras $ \(era, lo, hi) -> do
    nos <- sampleEra oldConn (scSeed cfg) (scSamples cfg) era lo hi
    oldBlocks <-
      if null nos
        then pure []
        else fetchProbes oldConn blockFields (probeSql OldSide "block" blockFields (inIntPred "t.block_no" nos))
    pure (era, nos, oldBlocks)

  let allHashes = concatMap (\(_, _, obs) -> map prHashHex obs) perEra
  newBlockMap <-
    if null allHashes
      then pure Map.empty
      else do
        nbs <- fetchProbes newConn blockFields (probeSql NewSide "block" blockFields (inHashPred "t.hash" allHashes))
        pure (Map.fromList [(prHashHex p, p) | p <- nbs])

  eraResults <- for perEra $ \(era, nos, oldBlocks) ->
    processEra oldConn newConn activeSpecs newBlockMap era nos oldBlocks
  pure SpotCheckResult {scCeiling = epochCeiling, scSkippedTables = skipped, scEras = eraResults}

processEra :: DbConn -> DbConn -> [ChildSpec] -> Map.Map Text Probe -> Era -> [Int64] -> [Probe] -> IO EraResult
processEra oldConn newConn specs newBlockMap era nos oldBlocks = do
  let pairs = [(ob, nb) | ob <- oldBlocks, Just nb <- [Map.lookup (prHashHex ob) newBlockMap]]
      blockDiffs =
        compareRowSets
          (map prRow oldBlocks)
          (mapMaybe (\ob -> prRow <$> Map.lookup (prHashHex ob) newBlockMap) oldBlocks)
  perBlock <- for pairs $ \(ob, _nb) -> processBlockTxs oldConn newConn specs ob
  let tableMap = Map.fromListWith (<>) (concat perBlock)
      ordered = [(tbl, diffs) | tbl <- tableOrder, Just diffs <- [Map.lookup tbl tableMap]]
  pure
    EraResult
      { erEra = eraName era
      , erSampled = length nos
      , erMatched = length pairs
      , erBlockDiffs = blockDiffs
      , erTableDiffs = ordered
      }

processBlockTxs :: DbConn -> DbConn -> [ChildSpec] -> Probe -> IO [(Text, [RowDiff])]
processBlockTxs oldConn newConn specs ob = do
  oldTxs <- fetchProbes oldConn txFields (probeSql OldSide "tx" txFields ("t.block_id = " <> prId ob))
  let txHashes = map prHashHex oldTxs
  newTxs <-
    if null txHashes
      then pure []
      else fetchProbes newConn txFields (probeSql NewSide "tx" txFields (inHashPred "t.hash" txHashes))
  let newTxMap = Map.fromList [(prHashHex p, p) | p <- newTxs]
      txDiffs =
        compareRowSets
          (map prRow oldTxs)
          (mapMaybe (\o -> prRow <$> Map.lookup (prHashHex o) newTxMap) oldTxs)
      txPairs = [(o, n) | o <- oldTxs, Just n <- [Map.lookup (prHashHex o) newTxMap]]
  childResults <- for txPairs $ \(o, n) ->
    for specs $ \cs -> do
      diffs <- compareChildSpec oldConn newConn cs (prId o) (prId n)
      pure (csTable cs, diffs)
  pure (("tx", txDiffs) : concat childResults)

compareChildSpec :: DbConn -> DbConn -> ChildSpec -> Text -> Text -> IO [RowDiff]
compareChildSpec oldConn newConn cs oldParentId newParentId = do
  let keyCount = length (csKeys cs)
      labels = map fst3 (csFields cs)
      sqlFor side pid = childSql side (csTable cs) (csKeys cs) (csFields cs) (csScopePred cs pid)
  oldRows <- fetchKeyed oldConn keyCount labels (sqlFor OldSide oldParentId)
  newRows <- fetchKeyed newConn keyCount labels (sqlFor NewSide newParentId)
  pure (compareRowSets oldRows newRows)

-- ---------------------------------------------------------------------------
-- * Rendering
-- ---------------------------------------------------------------------------

renderSpotCheck :: SpotCheckResult -> IO ()
renderSpotCheck result = do
  header "Spot check - per-era content"
  unless (null (scSkippedTables result)) $
    putLine (dim ("skipped (no index on new): " <> T.intercalate ", " (scSkippedTables result)))
  for_ (scEras result) renderEra
  putLine ""
  let total = sum (map totalEraDiffs (scEras result))
  if total == 0
    then putLine (green "content match: no field or row mismatches below the ceiling")
    else putLine (red ("content mismatches: " <> show total))

renderEra :: EraResult -> IO ()
renderEra er = do
  let mark = if totalEraDiffs er == 0 then green (padRight 6 "OK") else red (padRight 6 "DIFF")
  putLine
    ( mark
        <> padRight 10 (erEra er)
        <> dim ("sampled " <> show (erSampled er) <> ", matched " <> show (erMatched er))
    )
  renderTableDiffs "block" (erBlockDiffs er)
  for_ (erTableDiffs er) $ \(tbl, diffs) -> renderTableDiffs tbl diffs

renderTableDiffs :: Text -> [RowDiff] -> IO ()
renderTableDiffs label diffs =
  for_ (take 20 diffs) $ \d -> putLine ("    " <> red (label <> " " <> describeDiff d))

describeDiff :: RowDiff -> Text
describeDiff = \case
  MissingInNew k -> "missing in new: " <> keyText k
  MissingInOld k -> "missing in old: " <> keyText k
  FieldMismatch k f o n ->
    keyText k <> " " <> f <> ": old=" <> valText o <> " new=" <> valText n

keyText :: [Text] -> Text
keyText = T.intercalate "/"

valText :: Maybe Text -> Text
valText = fromMaybe "NULL"
