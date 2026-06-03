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
  , tdForeignKeys       :: ![ForeignKey]
  }

data ColumnDef = ColumnDef
  { cdName     :: !Text
  , cdType     :: !PgType
  , cdNullable :: !Bool
  }

data ForeignKey = ForeignKey
  { fkColumn       :: !Text
  , fkParentTable  :: !Text
  , fkParentColumn :: !Text
  }
```

Every extractor data table is described by one `TableDef`. The
optional-shaped fields (primary key, checks, defaults, unique
constraints, generated columns) are empty for most extractor tables —
those run UNLOGGED with no constraints during Ingest and get
constraints retrofitted by [`PreparingForVolatileTail`](phases/preparing).
They exist for the small number of tables that need LOGGED-from-day-one
semantics (currently `dbsync_sync_state`) and to carry per-table
metadata that is consumed later.

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
│   ├── Core.hs           # block, tx, slot_leader, meta, reverse_index
│   ├── UTxO.hs           # tx_out, tx_in, collateral, reference_tx_in
│   ├── Pool.hs           # pool_hash, pool_update, etc.
│   ├── … (one per feature)
│   ├── SyncState.hs      # dbsync_sync_state singleton metadata
│   └── EpochParamPending.hs  # Ingest → Prep bridge
├── Statement/            # one module per table + cross-cutting ones
│   ├── Block.hs
│   ├── Tx.hs
│   ├── …
│   ├── IdAllocator.hs    # sequence-batch allocation for Follow
│   ├── Indexes.hs        # pre-resolve + production index DDL
│   ├── Sequences.hs      # attach + setval after the LOGGED flip
│   ├── Resolve.hs        # FK resolves (CTAS rebuilds)
│   ├── Backfill.hs       # tx column backfills + epoch_param_pending
│   ├── Loader.hs         # libpq COPY stream primitives
│   └── Rollback.hs       # Follow-phase rollback cascade
└── Loader/
    └── Encoder.hs        # Builder-based COPY-text encoding
```

`Schema/<Domain>.hs` is the canonical place for a domain. Each one
exposes:

- The row types (`Block`, `Tx`, ...).
- A `<table>TableDef` value used by DDL generation.
- A `encode<Table>Copy` function for the Ingest writer.
- `<table>Encoder` / `<table>Decoder` hasql codecs used by the Follow
  writer and any control-plane queries.

`Statement/<Table>.hs` provides hasql `Statement`s the Follow loop and
the Preparing phase need.

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

Indexes and foreign keys are never emitted here. Indexes are built
post-load by
[`Phase.Preparing.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Preparing/Indexes.hs);
foreign keys aren't emitted at all — `tdForeignKeys` is metadata
consumed by the Follow rollback cascade, not a SQL constraint. That
keeps cascading deletes off the hot path; the cascade walks the
schema graph itself.

[`initSchema`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Init.hs)
walks every enabled extractor's `pdTables`, runs `generateCreateTable`
over each, and pipes the result through `psql`. Disabling an extractor
in the profile means its tables never get created — there's no
empty-table residue.

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
bEscapeText  :: Text -> Builder             -- escapes tab/newline/backslash
```

Each per-table `encode<Table>Copy` builds a list of `CopyField`s and
joins them via `buildCopyRow`. The whole row materialises as one
strict `ByteString` ready to hand to the loader stream.

Why Builders: the previous implementation used `BS8.concatMap` for
hex encoding and a three-pass replace for COPY-escaping, allocating
~50K tiny pinned ByteStrings per `tx_cbor` row. The Builder pipeline
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
- `query<Table>By<Column>Stmt :: Statement <Key> (Maybe <Id>)` —
  dedup lookups for the Follow resolver.

Cross-cutting statements live in dedicated modules:

| Module | Role |
|---|---|
| [`Statement.IdAllocator`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/IdAllocator.hs) | Bulk `setval` / batch `nextval` for the Follow per-block ID allocator. |
| [`Statement.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Indexes.hs) | Pre-resolve + production index builders. |
| [`Statement.Sequences`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Sequences.hs) | Attach sequences after the LOGGED flip; advance via `setval`. |
| [`Statement.Resolve`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Resolve.hs) | The `tx_in.tx_out_id` / collateral / reference FK resolves. |
| [`Statement.Backfill`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Backfill.hs) | The Preparing-pass `tx` column backfills + `epoch_param_pending`. |
| [`Statement.Loader`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Loader.hs) | Helpers for the libpq COPY streams. |
| [`Statement.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Rollback.hs) | The Follow-phase rollback cascade. |

`hasql` was chosen over `postgresql-simple` for two reasons:
prepared-statement reuse (Follow runs the same statements every block,
so prepared-statement caching matters) and `Pipeline` (the Follow
writer drains a block's writes as one round-trip).

## Profile-driven schema

A profile selects which extractors are enabled, and each extractor
declares the tables it owns. The schema layer is the consumer of that:

```haskell
-- in DbSync.Db.Schema.Init.initSchema
allTables :: [TableDef]
allTables = syncStateTableDef : concatMap pdTables enabledExtractors
```

`initSchema` walks `allTables` once at boot, generates the DDL, and
pipes through `psql`. The shape of the database is decided by the
profile and is fixed for the lifetime of that database — see
[Profiles](/users/profiles/overview) on the user side.

No JSON-RPC, no admin endpoints, no schema_version row that lets you
toggle extractors at runtime. A profile change means a fresh sync.

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
