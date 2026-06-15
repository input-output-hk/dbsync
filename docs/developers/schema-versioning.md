---
id: schema-versioning
title: Schema versioning and migrations
sidebar_position: 7
---

# Schema versioning and migrations

dbsync stamps every database with a schema version, a fingerprint of the
exact schema shape, and the set of extractors that built it. This page
explains how that machinery is wired, where each piece lives, and how to
evolve the schema without breaking the guarantees it provides.

The split is deliberate: **identity** — which version, which
fingerprint, which extractors — lives in the `dbsync` binary, and
**mechanism** — how tables are built and how state is read and written —
lives in the `dbsync-db` library.

:::note Status
All of this is implemented: the version and fingerprint bookkeeping, the
ordered migration files and the `gen-migration` tool that writes them,
the version-gated boot runner that applies them, and the CI, runtime, and
round-trip drift checks. It is described here in the present tense.
:::

## The four pieces of bookkeeping

| Piece | Lives in | Source |
|---|---|---|
| **Target version** — the version this binary produces | `currentSchemaVersion :: Int` | [`DbSync.Schema.Version`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Schema/Version.hs) |
| **Applied version** — the version a database was built at | `dbsync_sync_state.schema_version_applied` | [`DbSync.Db.Schema.SyncState`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/SyncState.hs) |
| **Fingerprint** — a hash of the exact schema shape | `dbsync_sync_state.schema_fingerprint`, pinned in `releasedSchemaFingerprints` | [`DbSync.Schema.Version`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Schema/Version.hs) |
| **Applied extractor set** — which extractors built a database | `dbsync_sync_state.extractors` (`text[]`) | [`DbSync.Db.Schema.SyncState`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/SyncState.hs) |

## Identity: the binary's view

[`DbSync.Schema.Version`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Schema/Version.hs)
declares what this binary targets:

```haskell
-- Bump on any change to the declared schema.
currentSchemaVersion :: Int
currentSchemaVersion = 1

-- A pin per released version. CI asserts the declared schema still
-- hashes to the pin for currentSchemaVersion.
releasedSchemaFingerprints :: [(Int, Fingerprint)]

-- Hex-encoded Blake2b-256 of the canonical schema rendering.
newtype Fingerprint = Fingerprint { unFingerprint :: Text }

-- Hash a set of tables plus any raw DDL that lives outside a TableDef
-- (e.g. the epoch views).
schemaFingerprint :: [TableDef] -> [Text] -> Fingerprint
```

`schemaFingerprint` sorts tables by name and renders every `TableDef`
field explicitly, so neither input order nor a renamed Haskell field can
silently change the hash — only a real change to the SQL shape moves it.

[`DbSync.Extractor.Registry`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/Registry.hs)
is the single source of truth for "what extractors exist" and turns that
into the fingerprint of the whole declared schema:

```haskell
-- Every extractor the binary can build, in declaration order.
allKnownExtractors :: [ExtractorDef]

-- Fingerprint of the full declared schema: every known extractor's
-- tables plus the bookkeeping/system tables and the epoch view DDL —
-- exactly what a full initSchema creates on a fresh database.
declaredSchemaFingerprint :: Fingerprint
```

Because `declaredSchemaFingerprint` covers the same objects a fresh
`initSchema` builds, a fingerprint taken from a live, fully-built
database can be compared against it directly.

## Mechanism: the library's view

The `dbsync-db` schema layer (see [Schema layer](schema-layer)) owns the
table definitions and the boot operations:

- [`initSchema`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Init.hs)
  creates the `dbsync_sync_state` singleton, the system tables, and
  every enabled extractor's tables.
- `dropSchema` tears all of that down (used by `--resync-from-genesis`
  and by tests).
- The boot path seeds `dbsync_sync_state` with `currentSchemaVersion`,
  `declaredSchemaFingerprint`, and the enabled-extractor set via
  [`seedSyncState`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/SyncState/Row.hs).

## Migrations

Migrations are ordered SQL files under
[`dbsync-db/migrations/`](https://github.com/input-output-hk/dbsync/tree/main/dbsync-db/migrations).
They are compiled into the binary at build time with `file-embed`,
quarantined in the single module
[`DbSync.Db.Schema.Migration.Files`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Migration/Files.hs)
— the only Template Haskell in the codebase — so the runner needs no
files on disk.

- **`0001-baseline-v1.sql`** is the frozen baseline: byte-for-byte the
  DDL a fresh `initSchema` runs for v1. It records the v1 shape and feeds
  the round-trip test, and is **never replayed against a live database**.
- **`NNNN-vN.sql`** files carry the incremental upgrade for each version
  past v1. The runner concatenates the files between a database's applied
  version and the binary's target.

On a fresh database `initSchema` builds the schema directly and
`seedSyncState` stamps the current version, fingerprint, and extractor
set — the migration files only ever upgrade an existing database.

### Generating migration files

The `gen-migration` executable (in the `dbsync` package) writes these
files and never touches a database:

- `gen-migration baseline` rewrites `0001-baseline-v1.sql` from the
  current `initSchema` output.
- `gen-migration generate` emits a draft
  `NNNN-v<currentSchemaVersion>.sql` from the typed schema diff, for you
  to review and complete by hand.

Headers carry no wall-clock timestamp, so the output is byte-reproducible.

## The boot check

Two gates run at boot, in order.

### Extractor-presence gate

The first gate is **presence-based**: it compares the extractor set the
profile enables against the set recorded on `dbsync_sync_state`.

```
checkExtractorPresence enabledNames connStr   -- IO probe of dbsync_sync_state
  → SchemaFresh        -- no dbsync_sync_state table
  | SchemaUnseeded     -- table present, no seeded row
  | analyzeExtractorState enabledNames observed  -- pure, when seeded
      → SchemaMatches | SchemaMismatched (NonEmpty MissingExtractor)

decideSchemaAction resyncFromGenesis schemaState
  → ActionRunInit | ActionSkipInit | ActionForceReinit | ActionAbort errs
```

- **`SchemaFresh`** (no `dbsync_sync_state` table) → `ActionRunInit`:
  build the schema from scratch.
- **`SchemaUnseeded`** (table present but never seeded — a crash landed
  between init and the seed write) → `ActionSkipInit`; boot then aborts
  with a resync hint.
- **`SchemaMatches`** (every enabled extractor present) →
  `ActionSkipInit`: resume.
- **`SchemaMismatched`** (an enabled extractor is missing) →
  `ActionAbort`: refuse to start and render each `MissingExtractor`.
- `--resync-from-genesis` short-circuits to `ActionForceReinit`:
  `dropSchema` then `initSchema`.

Extractors present in the database but absent from the running profile
are ignored — removing an extractor from a profile does not require a
re-sync. The pure core (`analyzeExtractorState`, `decideSchemaAction`)
is covered by unit tests; the IO wrapper and DDL by the PG-backed
`InitSpec`.

### Version and fingerprint gate

Once the schema is settled and the sync-state row is readable,
[`runMigrationGate`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Run.hs)
calls
[`runMigrations`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Migration.hs),
which reads `schema_version_applied` and `schema_fingerprint` and
dispatches on `decideMigrations`:

- **applied < current** — apply the embedded migration files for versions
  `(applied+1 .. current)` in a single transaction
  (`Hasql.Session.script`), then stamp the new `schema_version_applied`,
  `schema_fingerprint`, and `extractors`.
- **applied == current** — compare the stored fingerprint to
  `declaredSchemaFingerprint`. Equal resumes; different aborts
  (`SchemaDriftUncovered`): a `TableDef` changed with no migration or
  version bump.
- **applied > current** — abort (`DbNewerThanBinary`): the database was
  built by a newer binary than this one.

This gate runs automatically and is version-gated; there is no opt-in
apply or preview step. It is a no-op on a fresh database (the row was
just stamped at the current version) and never replays the baseline.

## Evolving the schema

Any change to a `TableDef`, or to the set of tables an extractor owns,
is a schema change. The workflow:

1. **Make the change** in the relevant `Schema/<Domain>.hs` (`dbsync-db`)
   — add a column, a table, a constraint.
2. **Run the unit suite.** The L1 pin test
   ([`VersionSpec`](https://github.com/input-output-hk/dbsync/blob/main/tests/main/unit/DbSync/Schema/VersionSpec.hs))
   fails: the declared fingerprint has moved away from the pin, proving
   the schema changed. That failure is the signal to account for it.
3. **Bump `currentSchemaVersion`** in
   [`DbSync.Schema.Version`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Schema/Version.hs)
   and add a `releasedSchemaFingerprints` entry for the new version. The
   failing test prints the new fingerprint; paste that in. Entries for
   already-released versions are frozen.
4. **Generate and complete the migration.** Run `gen-migration generate`,
   then review the draft `NNNN-vN.sql` and augment it with the renames,
   backfills, or recomputes the mechanical diff cannot infer.
5. **Verify.** The L3 ladder test
   ([`LadderSpec`](https://github.com/input-output-hk/dbsync/blob/main/tests/main/integration/DbSync/Schema/Migration/LadderSpec.hs))
   proves the baseline plus your file reconstructs the declared schema,
   and the boot gate applies it version-gated on existing databases.

:::caution Keep the fingerprint honest
Never edit a frozen pin to make a test pass. A moved fingerprint at a
released version means the schema changed without a version bump — fix
the version, not the pin.
:::

## Drift detection layers

The version and fingerprint are checked at three points, all implemented:

| Layer | When | Check | Where |
|---|---|---|---|
| **L1 — CI pin** | unit suite | `declaredSchemaFingerprint == releasedSchemaFingerprints[currentSchemaVersion]`. Any `TableDef` change moves the fingerprint and fails the test until the version is bumped and the pin refreshed. Pure, no DB. | `tests/main/unit/DbSync/Schema/VersionSpec.hs` |
| **L2 — runtime boot gate** | boot | `runMigrations` compares the stored fingerprint to the declared one at equal versions, and refuses to start on uncovered drift or a database newer than the binary. | [`DbSync.Db.Schema.Migration`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Migration.hs) |
| **L3 — round-trip ladder** | integration | A normalized `pg_dump --schema-only` of an `initSchema`-built database equals that of a baseline+ladder-built one, proving the committed files reconstruct the declared schema. Needs a live `dbsync_test`. | `tests/main/integration/DbSync/Schema/Migration/LadderSpec.hs` |

L1 is the layer that catches "I changed the schema and forgot to account
for it": it is pure and fast and reddens the unit suite the moment a
`TableDef` moves.

### Recompute invariants

Fingerprints catch shape drift; they cannot catch a stored *value*
drifting from what its source rows imply (the cardano-db-sync #2118
class). A separate harness
([`tests/lib/DbSync/Test/RecomputeInvariants.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/RecomputeInvariants.hs),
driven by
[`tests/main/e2e/DbSync/Phase/RecomputeInvariantsSpec.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/main/e2e/DbSync/Phase/RecomputeInvariantsSpec.hs))
asserts, after a real sync, that `epoch_finalized` equals the
`block`+`tx` aggregate, `block.tx_count` equals `COUNT(tx)`, and
`tx.out_sum` equals `SUM(tx_out.value)` for valid txs.

Ledger-state-sourced values — `reward`/`pot_reward` amounts,
`epoch_stake`, `pool_stat`, `ada_pots`, `drep_distr`,
`epoch_param`/`epoch_state`/`cost_model`, `gov_action_proposal` status,
`epoch_sync_stats` — are not recomputable from PostgreSQL alone. The
harness covers only the three PG-derivable invariants; validating
ledger-sourced values needs a ledger or golden oracle, which it does not
attempt.
