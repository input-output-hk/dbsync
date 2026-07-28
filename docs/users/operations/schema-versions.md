---
id: schema-versions
title: Schema versions and upgrades
sidebar_position: 4
---

# Schema versions and upgrades

Every dbsync database is stamped with a **schema version**, a **schema
fingerprint**, and a record of **which extractors built it**. dbsync
checks that stamp on every boot so a binary never runs against a
database whose shape it doesn't understand. This page explains what is
recorded, how the boot check behaves, and what to do when an upgrade or
profile change trips it.

:::note How the stamp is used
On every boot dbsync checks the stamp and acts on it. It **applies** any
pending schema migrations automatically, in a single transaction, to
bring a behind database up to the version this binary targets. If the
database was built by a *newer* binary, or its shape has drifted with no
migration to cover it, dbsync **refuses to start** rather than risk
damage. Adding or removing an extractor (a profile change) is a separate
matter and still needs a re-sync — see
[Changing a profile](#changing-a-profile).
:::

## What gets recorded

Three pieces of bookkeeping live in the database itself:

| Where | Column / table | Meaning |
|---|---|---|
| `dbsync_sync_state` | `schema_version_applied` | The schema version this database was built at. |
| `dbsync_sync_state` | `schema_fingerprint` | A hash of the exact schema shape it was built with. |
| `dbsync_sync_state` | `extractors` (`text[]`) | Which extractors built the database. |

The `extractors` column on `dbsync_sync_state` is the source of truth
for the boot check — a `text[]` of the enabled extractor names:

```
                 extractors
---------------------------------------------
 {core,utxo,multi_asset,metadata,...}
```

## The boot check

On startup, dbsync compares the extractors your profile enables against
the set recorded on `dbsync_sync_state` and picks one of three paths:

| Observed state | What dbsync does |
|---|---|
| No `dbsync_sync_state` table (empty database) | Treats it as a fresh database and creates the schema. |
| Every enabled extractor is present | Skips init and resumes normally. |
| An enabled extractor is missing | **Refuses to start** and prints which extractors are missing. |

The third case is the one you'll see after an upgrade or profile change.
The message names the exact problem:

```
Schema mismatch — refusing to start. Use --resync-from-genesis to wipe and re-sync.
  - Extractor 'governance' is enabled in the profile but missing from the database.
```

Read the message — it tells you which extractor is enabled in your
profile but was never built into this database.

:::tip Removing an extractor is safe
Extractors recorded in the database but **not** in your current profile
are ignored — you can drop an extractor from your profile and keep
running against the same database without a re-sync. Only *adding* an
extractor (or any other shape change) needs a rebuild.
:::

## Upgrading dbsync

When you upgrade the dbsync binary, the schema version it targets may be
higher than the one stamped in your database.

- **No schema change** (most upgrades) — the version is unchanged, the
  extractor set still matches, and dbsync resumes against your existing
  database with no action needed.
- **Schema change** — the new binary targets a higher schema version. On
  the next boot dbsync applies the pending migrations automatically, in a
  single transaction, updates the stamp, and resumes. There is no flag
  and no manual step.

dbsync refuses to start in two cases instead:

- The database was built by a **newer** binary than the one you're
  running — its `schema_version_applied` is ahead of what this binary
  targets. Run a build that targets that version, or re-sync.
- The schema has **drifted** from what the binary expects with no
  migration to cover it. This points to a mis-packaged or hand-edited
  build; the message prints the stored and expected fingerprints.

Some changes still imply a re-sync even with migrations in place —
notably enabling a new extractor (a [profile change](#changing-a-profile))
or a change to data derived from ledger state, which dbsync cannot
backfill from the database alone.

Always read the release notes for an upgrade to know what it carries.

## Changing a profile

Adding an extractor changes the set of tables the database needs. On the
next boot the new extractor's tables are absent, the presence check
fails, and dbsync refuses to start. This is the same
[profile immutability](../profiles/overview#profile-immutability) rule
seen from the schema side: a database is fixed to the profile that built
it.

The fix is a re-sync with the new profile in place. The cleanest way is
the `--resync-from-genesis` flag, which drops the schema, wipes the
on-disk ledger state, and rebuilds from genesis in a single boot:

```bash
dbsync --config my-config.json --resync-from-genesis ...
```

:::danger Destructive
`--resync-from-genesis` drops the entire database and the on-disk ledger
state. Back up anything you've built on top of the dbsync schema first.
Full details and a backup checklist are in
[Recovery](recovery#when-re-sync-is-unavoidable).
:::

## Inspecting the stamp

You can read the current stamp directly from the database:

```sql
-- Version, fingerprint, and extractor set this database was built at
SELECT schema_version_applied, schema_fingerprint, extractors
FROM dbsync_sync_state;

-- Which extractors built it, one per row
SELECT unnest(extractors) AS extractor
FROM dbsync_sync_state
ORDER BY extractor;
```

If you ever file a bug about a boot mismatch, the output of these two
queries plus the binary version is the most useful thing to include.

## The schema fingerprint vs the network fingerprint

These are two different guards — don't confuse them:

- The **schema fingerprint** (`dbsync_sync_state.schema_fingerprint`)
  describes the *shape* of the tables.
- The **network/ledger fingerprint** (stored under the ledger state
  directory) describes *which chain* the database belongs to. Pointing
  dbsync at a different network's genesis is caught by that check — see
  [Recovery](recovery#when-re-sync-is-unavoidable).
