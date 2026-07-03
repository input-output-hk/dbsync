-- | Physical schema-structure comparison: per-table column sets and
-- types, foreign keys, and unique constraints.
--
-- Columns are compared between the two live databases via
-- @information_schema@, with types collapsed to coarse buckets so a
-- domain or width difference (@lovelace@ vs @bigint@) does not
-- register while a real shape change (@bytea@ vs @text@) does.
--
-- The new schema never emits physical @REFERENCES@ constraints and
-- builds its unique indexes only in @PreparingForVolatileTail@, so on
-- the new side foreign keys and uniques are the declared 'TableDef'
-- metadata unioned with whatever physically exists; on the old side
-- they are read from the catalogs.
module DbSync.Compare.Structure
  ( structureChecks
  ) where

import Cardano.Prelude
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import DbSync.Compare.Check (Check (..), CheckItem (..), CheckOutcome (..))
import DbSync.Compare.Connect (DbConn, queryTextList)
import DbSync.Compare.Schema (renamedNewName, renamedOldName)
import DbSync.Db.Schema.Types (ForeignKey (..), TableDef (..))
import DbSync.Extractor.Registry (allDeclaredTables)

-- ---------------------------------------------------------------------------
-- * Check construction
-- ---------------------------------------------------------------------------

-- Fetch both sides' catalogs once, compute every outcome purely, and
-- wrap each as a constant check so the runner prints them like any
-- other check line.
structureChecks :: DbConn -> DbConn -> IO [CheckItem]
structureChecks oldConn newConn = do
  oldCols <- fetchColumns oldConn
  newCols <- fetchColumns newConn
  oldFks <- fetchFks oldConn
  newFks <- fetchFks newConn
  oldUniques <- fetchUniques oldConn
  newUniques <- fetchUniques newConn
  let shared = sort [t | t <- Map.keys newCols, renamedOldName t `Map.member` oldCols]
      colFor side t = fromMaybe Map.empty (Map.lookup t side)
      -- Old-side FKs and uniques on tables with no new counterpart are
      -- dropped here: the missing table itself is already reported by
      -- the schema coverage, and repeating it per constraint is noise.
      newTables = Set.fromList (Map.keys newCols)
      inNew t = renamedNewName t `Set.member` newTables
      oldFksShared = Set.filter (\(c, _, p) -> inNew c && inNew p) oldFks
      oldUniquesShared = Set.filter (\(t, _) -> inNew t) oldUniques
      items =
        [ constCheck ("columns " <> t) (columnsOutcome t (colFor oldCols (renamedOldName t)) (colFor newCols t))
        | t <- shared
        ]
          <> [ constCheck "foreign keys" (fkOutcome oldFksShared newFks)
             , constCheck "unique constraints" (uniqueOutcome oldUniquesShared newUniques)
             ]
  pure (Section "schema structure" : items)

constCheck :: Text -> CheckOutcome -> CheckItem
constCheck label outcome = Item (Check label (\_ _ -> pure outcome))

-- ---------------------------------------------------------------------------
-- * Columns
-- ---------------------------------------------------------------------------

-- table -> column -> type name (domain name when the column uses one).
fetchColumns :: DbConn -> IO (Map Text (Map Text Text))
fetchColumns conn = do
  rows <- queryTextList conn sql
  pure $ Map.fromListWith Map.union (mapMaybe parse rows)
  where
    sql =
      "SELECT c.table_name || '|' || c.column_name || '|' || COALESCE(c.domain_name, c.udt_name) "
        <> "FROM information_schema.columns c "
        <> "JOIN information_schema.tables t ON t.table_schema = c.table_schema AND t.table_name = c.table_name "
        <> "WHERE c.table_schema = 'public' AND t.table_type = 'BASE TABLE'"
    parse row = case T.splitOn "|" row of
      [tbl, col, typ] -> Just (tbl, Map.singleton col typ)
      _ -> Nothing

columnsOutcome :: Text -> Map Text Text -> Map Text Text -> CheckOutcome
columnsOutcome table olds news
  | null problems = Ok (show (Map.size news) <> " columns" <> newOnlyNote)
  | otherwise = Diff problems
  where
    problems = missing <> mismatched
    missing =
      [ "missing in new: " <> oc <> " (" <> ot <> ")"
      | (oc, ot) <- Map.toList olds
      , newColumnName table oc `Map.notMember` news
      , oc `notElem` expectedOldOnly table
      ]
    mismatched =
      [ "type " <> oc <> ": old=" <> ot <> " new=" <> nt
      | (oc, ot) <- Map.toList olds
      , Just nt <- [Map.lookup (newColumnName table oc) news]
      , canonicalType ot /= canonicalType nt
      , not (allowedTypeDiff table oc (canonicalType ot) (canonicalType nt))
      ]
    mappedNew = Set.fromList (map (newColumnName table) (Map.keys olds))
    newOnly = filter (`Set.notMember` mappedNew) (Map.keys news)
    newOnlyNote
      | null newOnly = ""
      | otherwise = " (+" <> show (length newOnly) <> " new-only: " <> T.intercalate ", " (take 5 newOnly) <> ")"

-- Collapse a type (or domain) name to a coarse bucket so intentional
-- width/domain differences between the schemas do not register.
canonicalType :: Text -> Text
canonicalType t
  | t `elem` ints = "num"
  | t `elem` numerics = "num"
  | t `elem` byteas = "bytea"
  | t `elem` texts = "text"
  | t `elem` ["timestamp", "timestamptz"] = "timestamp"
  | t `elem` ["json", "jsonb"] = "jsonb"
  | t `elem` ["float4", "float8"] = "float"
  | otherwise = t
  where
    ints = ["int2", "int4", "int8", "word31type", "word63type", "txindex"]
    numerics = ["numeric", "lovelace", "outsum", "word64type", "word128type", "int65type"]
    byteas = ["bytea", "hash28type", "hash32type", "asset32type", "addr29type"]
    -- Old-schema enums are compared against plain text on the new side.
    texts =
      [ "text", "varchar"
      , "rewardtype", "scripttype", "scriptpurposetype", "syncstatetype"
      , "vote", "voterrole", "govactiontype", "anchortype"
      ]

-- Ratios the old schema stores as double precision and the new one as
-- text (protocol parameters, pool margin). The content checks compare
-- them through a double-precision cast.
allowedTypeDiff :: Text -> Text -> Text -> Text -> Bool
allowedTypeDiff table col oldBucket newBucket =
  oldBucket == "float"
    && newBucket == "text"
    && (table `elem` ["param_proposal", "epoch_param"] || (table, col) == ("pool_update", "margin"))

-- Old columns replaced by the address-normalisation FK on the new side.
expectedOldOnly :: Text -> [Text]
expectedOldOnly table
  | table `elem` ["tx_out", "collateral_tx_out"] = ["address", "address_has_script", "payment_cred"]
  | otherwise = []

-- Renamed columns as (new table, [(new column, old column)]).
renamedColumns :: [(Text, [(Text, Text)])]
renamedColumns =
  [ ("param_proposal", dvtGroups)
  , ("epoch_param", dvtGroups)
  ]
  where
    dvtGroups =
      [ ("dvt_pp_network_group", "dvt_p_p_network_group")
      , ("dvt_pp_economic_group", "dvt_p_p_economic_group")
      , ("dvt_pp_technical_group", "dvt_p_p_technical_group")
      , ("dvt_pp_gov_group", "dvt_p_p_gov_group")
      ]

-- Map an old column name to its new-schema name within a (new-named) table.
newColumnName :: Text -> Text -> Text
newColumnName table oldCol =
  fromMaybe oldCol $ do
    renames <- lookup table renamedColumns
    lookup oldCol [(o, n) | (n, o) <- renames]

-- ---------------------------------------------------------------------------
-- * Foreign keys
-- ---------------------------------------------------------------------------

-- Single-column FK constraints as (child table, column, parent table).
-- Both schemas only ever reference the parent's surrogate id.
fetchFks :: DbConn -> IO (Set (Text, Text, Text))
fetchFks conn = do
  rows <- queryTextList conn sql
  pure $ Set.fromList (mapMaybe parse rows)
  where
    sql =
      "SELECT c.conrelid::regclass::text || '|' || a.attname || '|' || c.confrelid::regclass::text "
        <> "FROM pg_constraint c "
        <> "JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = c.conkey[1] "
        <> "WHERE c.contype = 'f' AND c.connamespace = 'public'::regnamespace "
        <> "AND array_length(c.conkey, 1) = 1"
    parse row = case T.splitOn "|" row of
      [child, col, parent] -> Just (child, col, parent)
      _ -> Nothing

declaredFks :: Set (Text, Text, Text)
declaredFks =
  Set.fromList
    [ (tdName td, fkColumn fk, fkParentTable fk)
    | td <- allDeclaredTables
    , fk <- tdForeignKeys td
    ]

-- Translate an old-side FK edge into new-schema naming.
translateFk :: (Text, Text, Text) -> (Text, Text, Text)
translateFk (child, col, parent) =
  let childNew = renamedNewName child
   in (childNew, newColumnName childNew col, renamedNewName parent)

fkOutcome :: Set (Text, Text, Text) -> Set (Text, Text, Text) -> CheckOutcome
fkOutcome oldFks newPhysical
  | Set.null oldFks =
      Info ("old database carries no FK constraints; new declares " <> show (Set.size newAll) <> " - nothing to compare")
  | null missing = Ok (show (Set.size oldFks) <> " FKs covered" <> extraNote)
  | otherwise = Diff (map render missing)
  where
    newAll = declaredFks <> newPhysical
    oldTranslated = Set.map translateFk oldFks
    missing = Set.toList (oldTranslated `Set.difference` newAll)
    extras = Set.size (newAll `Set.difference` oldTranslated)
    extraNote
      | extras == 0 = ""
      | otherwise = " (+" <> show extras <> " new-only)"
    render (child, col, parent) = "missing in new: " <> child <> "." <> col <> " -> " <> parent

-- ---------------------------------------------------------------------------
-- * Unique constraints
-- ---------------------------------------------------------------------------

-- Unique indexes and constraints as (table, sorted column list). Sorted
-- because column order changes uniqueness semantics not at all.
fetchUniques :: DbConn -> IO (Set (Text, [Text]))
fetchUniques conn = do
  rows <- queryTextList conn sql
  pure $ Set.fromList (mapMaybe parse rows)
  where
    sql =
      "SELECT t.relname || '|' || array_to_string(array_agg(a.attname ORDER BY x.ord), ',') "
        <> "FROM pg_index i "
        <> "JOIN pg_class t ON t.oid = i.indrelid "
        <> "JOIN pg_namespace n ON n.oid = t.relnamespace "
        <> "CROSS JOIN LATERAL unnest(i.indkey) WITH ORDINALITY AS x(attnum, ord) "
        <> "JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = x.attnum "
        <> "WHERE i.indisunique AND NOT i.indisprimary AND n.nspname = 'public' AND t.relkind = 'r' "
        <> "GROUP BY t.relname, i.indexrelid"
    parse row = case T.splitOn "|" row of
      [tbl, cols] -> Just (tbl, sort (T.splitOn "," cols))
      _ -> Nothing

declaredUniques :: Set (Text, [Text])
declaredUniques =
  Set.fromList
    [ (tdName td, sort (toList cols))
    | td <- allDeclaredTables
    , cols <- tdUniqueConstraints td
    ]

translateUnique :: (Text, [Text]) -> (Text, [Text])
translateUnique (table, cols) =
  let tableNew = renamedNewName table
   in (tableNew, sort (map (newColumnName tableNew) cols))

uniqueOutcome :: Set (Text, [Text]) -> Set (Text, [Text]) -> CheckOutcome
uniqueOutcome oldUniques newPhysical
  | Set.null oldUniques =
      Info ("old database carries no unique constraints; new declares " <> show (Set.size newAll) <> " - nothing to compare")
  | null missing = Ok (show (Set.size oldUniques) <> " uniques covered" <> extraNote)
  | otherwise = Diff (map render missing)
  where
    newAll = declaredUniques <> newPhysical
    oldTranslated = Set.map translateUnique oldUniques
    missing = Set.toList (oldTranslated `Set.difference` newAll)
    extras = Set.size (newAll `Set.difference` oldTranslated)
    extraNote
      | extras == 0 = ""
      | otherwise = " (+" <> show extras <> " new-only)"
    render (table, cols) = "missing in new: " <> table <> "(" <> T.intercalate ", " cols <> ")"
