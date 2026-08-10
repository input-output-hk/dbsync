---
id: schema-layer
title: Schema layer (dbsync-db)
sidebar_position: 6
---

# Schema layer

The [`dbsync-db`](https://github.com/input-output-hk/dbsync/tree/main/dbsync-db)
package owns everything between Haskell row types and the PostgreSQL
wire protocol. It's deliberately split out from `dbsync` so the schema
is self-contained: schema types and DDL generation depend on nothing
in the sync engine.

## Vocabulary

Three top-level concepts in
[`DbSync.Db.Schema.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Types.hs):

```haskell
data TableDef = TableDef
  { tdName              :: !Text
  , tdColumns           :: ![ColumnDef]
  , tdMode              :: !TableMode  -- LOGGED | UNLOGGED
  , tdPrimaryKey        :: !(Maybe [Text])
  , tdChecks            :: ![Text]
  , tdColumnDefaults    :: ![(Text, Text)]
  , tdUniqueConstraints :: ![NonEmpty Text]
  , tdGeneratedColumns  :: ![(Text, Text)]
  , tdIdentityColumns   :: ![Text]
  , tdParentRefs        :: ![ParentRef]
  }

data ColumnDef = ColumnDef
  { cdName     :: !Text
  , cdType     :: !PgType
  , cdNullable :: !Bool
  }

data ParentRef = ParentRef
  { prColumn       :: !Text
  , prParentTable  :: !Text
  , prParentColumn :: !Text
  }
```

One `TableDef` describes one extractor data table.

The optional-shaped fields — primary key, checks, defaults, unique
constraints, generated columns — are empty for most extractor tables.
Those run UNLOGGED and constraint-free during Ingest, and
[`PreparingForVolatileTail`](phases/preparing) retrofits the
constraints. The fields exist for the three tables created LOGGED from
the start — `dbsync_sync_state`, `epoch_param_pending`, and
`epoch_finalized` — and to carry metadata consumed later.

`tdParentRefs` declares **ownership**: rows of this table belong to a
row of `prParentTable` and must die with it. It drives three things —
the resume trim, the Follow rollback cascade, and the `FOREIGN KEY`
constraints created during Prep.

A reference to a deduplicated row is deliberately *not* an ownership
edge. `tx_out.inline_datum_id`, `tx_in.redeemer_id`, and
`ma_tx_out.ident` are shared between transactions rather than owned by
one, so they are neither cascaded nor constrained.

## Layout

```
dbsync-db/src/DbSync/Db/
├── Types.hs              # domain newtypes (DbLovelace, enums, Word128 codecs)
├── Sql.hs                # quoteIdent, quoteLiteral
├── Sql/Refs.hs           # column-reference DSL for hand-written SQL
├── Schema/
│   ├── Types.hs          # TableDef, ColumnDef, PgType
│   ├── Ids.hs            # one newtype per ID column
│   ├── Entity.hs         # Key type family
│   ├── Generate.hs       # CREATE TABLE DDL from TableDef
│   ├── Init.hs           # initSchema + schema-mode flip DDL
│   ├── Core.hs           # block, tx, slot_leader, stake_address, pool_hash
│   ├── UTxO.hs           # tx_out, tx_in, collateral, reference_tx_in
│   ├── Pool.hs           # pool_update, pool_owner, pool_relay, …
│   ├── … (one per domain)
│   ├── SyncState.hs      # dbsync_sync_state singleton metadata
│   └── EpochParamPending.hs  # Ingest → Prep bridge
├── Statement/            # one module per domain, mirroring Schema/
│   ├── Core.hs           # block, tx, slot_leader, stake_address, pool_hash
│   ├── UTxO.hs
│   ├── …
│   ├── IdAllocator.hs    # sequence-batch allocation for Follow
│   ├── Indexes.hs        # pre-resolve + production index DDL
│   ├── Sequences.hs      # attach + setval after the LOGGED flip
│   ├── Constraints.hs    # ownership FOREIGN KEY DDL
│   ├── Loader.hs         # libpq COPY stream primitives
│   └── Worker/
│       ├── Resolve.hs           # FK resolves (CTAS rebuilds)
│       ├── Backfill.hs          # tx column backfills
│       ├── EpochParamPending.hs # the Ingest → Prep bridge table
│       └── Rollback.hs          # Follow-phase rollback cascade
└── Loader/
    └── Encoder.hs        # Builder-based COPY-text encoding
```

`Schema/` and `Statement/` group by **domain**, not by table. One
module covers a family of related tables.

`Schema/<Domain>.hs` is the canonical place for a domain. Each one
exposes:

- The row types (`Block`, `Tx`, ...).
- A `<table>TableDef` value used by DDL generation.
- An `encode<Table>Copy` function for the Ingest writer.
- `<table>Encoder` / `<table>Decoder` hasql codecs used by the Follow
  writer and any control-plane queries.

`Statement/<Domain>.hs` provides the hasql `Statement`s the Follow loop
and the Preparing phase need.

## DDL generation

[`DbSync.Db.Schema.Generate.generateCreateTable`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Generate.hs)
takes a `TableDef` and returns the matching `CREATE TABLE` statement.
For most extractor tables (UNLOGGED, no constraints) the output is:

```sql
CREATE UNLOGGED TABLE "block" (
  "id" BIGINT NOT NULL,
  "hash" BYTEA NOT NULL,
  "epoch_no" BIGINT
);
```

For tables with a primary key, checks, or defaults
(`dbsync_sync_state`):

```sql
CREATE TABLE "dbsync_sync_state" (
  "id" SMALLINT NOT NULL DEFAULT 1,
  "block_id_counter" BIGINT NOT NULL DEFAULT 1,
  ...,
  PRIMARY KEY ("id"),
  CHECK ("id" = 1)
);
```

Neither indexes nor foreign keys are emitted here. Both arrive during
[`PreparingForVolatileTail`](phases/preparing), after the bulk load:

- Indexes come from
  [`Phase.Preparing.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Preparing/Indexes.hs).
- Foreign keys come from `Phase.Preparing.Constraints`, which turns
  each `tdParentRefs` entry into an `ALTER TABLE … ADD CONSTRAINT …
  FOREIGN KEY … NOT VALID`, then validates them in a second step.

`tdParentRefs` therefore does double duty: it is both the SQL
constraint source and the graph the Follow rollback cascade walks.

[`initSchema`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Init.hs)
takes the `TableDef` list from its caller, generates the DDL, and pipes
it through `psql`. An extractor left out of the config never gets its
tables created, so there is no empty-table residue.

## COPY encoders

The Ingest write path produces PostgreSQL COPY-text rows. The Builder
pipeline in
[`DbSync.Db.Loader.Encoder`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Loader/Encoder.hs)
exposes:

```haskell
type CopyField = Maybe Builder  -- Nothing → \N

buildCopyRow :: [CopyField] -> ByteString
bInt64       :: Int64 -> Builder
bWord64      :: Word64 -> Builder
bBool        :: Bool -> Builder
bHex         :: ByteString -> Builder       -- \\x prefix
bUTCTime     :: UTCTime -> Builder
bText        :: Text -> Builder
bEscapeText  :: ByteString -> Builder       -- escapes tab/newline/backslash
```

Each per-table `encode<Table>Copy` builds a list of `CopyField`s and
joins them via `buildCopyRow`. The whole row materialises as one
strict `ByteString` ready to hand to the loader stream.

Why Builders: naive hex encoding and multi-pass COPY-escaping allocate
a swarm of tiny pinned ByteStrings per row. The Builder pipeline
allocates one buffer per row and emits everything in-place.

## Hasql statements

The Follow path and the Preparing pass both speak hasql. Per-table
modules under
[`DbSync.Db.Statement.*`](https://github.com/input-output-hk/dbsync/tree/main/dbsync-db/src/DbSync/Db/Statement)
expose preparable statements for the common operations:

- `insert<Table>RowStmt :: Statement (<Id>, <Row>) ()` — Follow-time
  row insert with caller-chosen ID.
- `insert<Table>Stmt :: Statement <Row> <Id>` — variant where PG picks
  the ID via `RETURNING id`. Used at boot for one-shot inserts.
- `next<Table>IdStmt :: Statement () <Id>` — allocate the next ID
  from the table's sequence.
- `query<Table>IdStmt :: Statement <Key> (Maybe <Id>)` — dedup lookups
  for the Follow resolver. The block and tx hash lookups use the
  longer `query<Table>By<Column>Stmt` form instead.

Cross-cutting statements live in dedicated modules:

| Module | Role |
|---|---|
| [`Statement.IdAllocator`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/IdAllocator.hs) | Bulk `setval` / batch `nextval` for the Follow per-block ID allocator. |
| [`Statement.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Indexes.hs) | Pre-resolve + production index builders. |
| [`Statement.Sequences`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Sequences.hs) | Attach sequences after the LOGGED flip; advance via `setval`. |
| [`Statement.Constraints`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Constraints.hs) | Ownership `FOREIGN KEY` DDL built from `tdParentRefs`. |
| [`Statement.Loader`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Loader.hs) | Helpers for the libpq COPY streams. |
| [`Statement.Worker.Resolve`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Worker/Resolve.hs) | The `tx_in.tx_out_id` / collateral / reference FK resolves. |
| [`Statement.Worker.Backfill`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Worker/Backfill.hs) | The Preparing-pass `tx` column backfills. |
| [`Statement.Worker.EpochParamPending`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Worker/EpochParamPending.hs) | The `epoch_param_pending` Ingest → Prep bridge. |
| [`Statement.Worker.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Worker/Rollback.hs) | The Follow-phase rollback cascade. |

`hasql` was chosen over `postgresql-simple` for two reasons:
prepared-statement reuse (Follow runs the same statements every block,
so prepared-statement caching matters) and `Pipeline` (the Follow
writer drains a block's writes as one round-trip).

## Config-driven schema

The config selects which extractors are enabled, and each extractor
declares the tables it owns through `pdTables`.

The two packages meet at `initSchema`'s argument list, not inside it:

```haskell
initSchema :: [TableDef] -> Text -> IO ()
```

`dbsync-db` never sees an `ExtractorDef`. `dbsync` depends on
`dbsync-db`, so the dependency cannot run the other way. The caller in
`DbSync.App.Run` flattens the enabled extractors' `pdTables` and hands
the resulting list down.

`initSchemaStatements` then emits, in order:

1. `dbsync_sync_state`.
2. `epoch_param_pending`, always, even with the ledger disabled.
3. Every table in the list, as one batch.
4. The `epoch_current` and `epoch` views, if `epoch_finalized` is in
   the list.

The migration baseline file is generated from this same list, so it
cannot drift from what `initSchema` creates.

The database's shape is fixed for its lifetime. There is no JSON-RPC,
no admin endpoint, and no row that toggles extractors at runtime — see
[`extractors` is fixed per database](/users/config/overview#extractors-is-fixed-per-database).

## Domain types

[`DbSync.Db.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Types.hs)
groups the cross-domain newtypes and enums:

- `DbLovelace` (Word64 wrapped for Lovelace amounts) and `DbWord64`
  (generic Word64 wrapper). Both stored in PG `numeric` and encoded
  via `Scientific` to avoid the silent overflow that `int8` produces
  on aggregations past `maxBound :: Int64`.
- `DbInt65` for the signed pot-transfer amounts.
- `Word128` codecs for asset quantities that exceed 64 bits.
- Per-enum types: `ScriptPurpose`, `ScriptType`, `RewardSource`,
  `SyncState`, `Vote`, `VoterRole`, `GovActionType`, `AnchorType`.

The encoding rule for `numeric` columns is the load-bearing part: a
SUM of `tx.out_sum` exceeds `maxBound :: Int64` very quickly on
mainnet, and going via `Scientific` is the only safe path.

## Reading order

:::tip Starting from scratch
For someone new to the layer, this order tends to work:

1. [`Schema.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Types.hs)
   — the vocabulary.
2. [`Schema.Core`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Core.hs)
   — the representative example: row types, `TableDef`, COPY encoder,
   hasql codecs all in one module.
3. [`Schema.Generate`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Generate.hs)
   — how DDL falls out of `TableDef`.
4. [`Schema.Init`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Init.hs)
   — the entry point that ties it together.
5. [`Loader.Encoder`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Loader/Encoder.hs)
   — the COPY-text wire format.

After that the per-domain `Schema/*` and `Statement/*` files are
small and follow the same template.
:::
