---
id: faq
title: FAQ
sidebar_position: 6
---

# Frequently asked questions

## How much disk space does each profile need?

Approximate `pg_database_size` after a full mainnet sync. The
`everything` row is measured; the smaller profiles are approximations
derived from the `everything`-profile per-table breakdown:

| Profile | Size |
|---|---|
| `minimal` | ~30 GB |
| `utxo-only` | ~160 GB |
| `spo` | ~185 GB |
| `dapp` | ~265 GB |
| `everything-no-ledger` | ~475 GB |
| `everything` | ~480 GB |

Plus ~220 GB for the cardano-node database (mostly the ImmutableDB
chain history at mainnet tip). Allow 50% headroom over these figures
— the Preparing pass needs working space for the index builds and
the LOGGED-flip table rewrite.

The on-disk ledger state (with `ledger.enabled = true`) is an
additional ~11 GB at mainnet tip.

Note: `tx_cbor` alone is ~218 GB and dominates the `everything` and
`everything-no-ledger` totals. If you don't need raw transaction
CBOR, disabling `cbor` in a custom profile derived from one of those
presets nearly halves the database. See
[Preset profiles](profiles/presets) for the breakdown.

## Do I need ledger state for my use case?

Enable `ledger` if you need:

- Reward amounts per epoch (`reward`).
- Per-epoch stake distribution (`epoch_stake`).
- Protocol parameter snapshots per epoch (`epoch_param`).
- ADA pot balances (`ada_pots`).
- Per-tx deposit values populated on the `tx` row.

Skip it (use `everything-no-ledger`) if you only need block-derived
data — UTxO, certificates, metadata, native tokens, governance
actions as recorded on-chain. The `ledger` cost is roughly 8 GB of
resident RAM and ~11 GB of disk at mainnet tip.

## Can I change profile after sync?

No. A profile is fixed once the database is created — see
[profile immutability](profiles/overview#profile-immutability).
Changing means a fresh sync against a fresh database.

Schema migrations handle structural schema changes across dbsync
versions, but they don't turn an extractor on or off in an existing
database — enabling one needs its tables back-filled from genesis. Pick
the profile carefully before committing to a multi-day sync.

## What hardware do I need?

The target is **4 cores / 16 GB RAM** for an `everything-no-ledger`
mainnet sync. Bigger machines sync faster but the architecture
doesn't need them.

| Component | Minimum | Comfortable |
|---|---|---|
| CPU cores | 4 | 6–8 |
| RAM | 16 GB | 32 GB (or more if ledger is on) |
| Disk | as per profile + 50% headroom | NVMe / local SSD |
| Network | enough to keep up with mainnet block production | any modern broadband |

Disk speed matters more than CPU. The Ingest path is bandwidth-bound
to PostgreSQL; on a slow disk you'll see PG's WAL writes dominate.
Local NVMe is the sweet spot; network-mounted PG data directories
hurt a lot.

## How does this compare to the original cardano-db-sync?

Same schema (mostly), different engine. Headline differences:

- **Faster initial sync.** The bulk-load path uses parallel COPY
  streams, pre-assigned IDs, and UNLOGGED tables; the original is
  closer to per-tx INSERT.
- **Modular extractors.** You pick which projections run via a
  profile JSON. The original is monolithic.
- **In-process ledger.** The optional ledger worker is V2 LSM-backed
  rather than the in-memory state the original used.

Workloads that worked against the original mostly work against this one
with a matching profile. The schema is close but not identical, so
validate your queries against a sample sync before switching over.

## Can I run multiple dbsync instances against one node?

Yes, as long as each has its own PostgreSQL database and its own
`--ledger-state-dir`. The node's n2c socket supports multiple
clients concurrently.

This is occasionally useful — running an `everything` instance and a
`minimal` instance side-by-side, for example, lets you query a
small fast index while the full one catches up.

## Can I run dbsync against a remote PostgreSQL?

Yes. Set the `database.host` / `database.port` in the profile to
point at your PG instance. The COPY streams open ~30 simultaneous
connections during Ingest, so make sure `max_connections` is high
enough.

Latency to PG matters during Ingest. A few-millisecond round-trip
adds little; 50+ ms (i.e. cross-region) noticeably slows the
bulk-load. Co-locating dbsync and PG is recommended.

## What about Mithril for dbsync itself?

dbsync has no equivalent of Mithril snapshots. Each sync runs the
extractor pipeline against the blocks the node delivers — there's no
"download a pre-built PG dump" path. PG-side `pg_basebackup` or
filesystem snapshots can serve a similar role if you control both
ends; see [Recovery / Backup strategy](operations/recovery#backup-strategy).

## What logs should I keep?

At `info` level, the `epoch_sync_stats` table captures everything
useful retrospectively: per-epoch block count, tx count, timing.
Stdout/stderr logs are mostly useful for live monitoring and
post-mortems on crashes.

Production deployments tend to ship the JSON logs to a centralised
log aggregator and keep the PG `epoch_sync_stats` table for long-term
analysis.

## Where do I report bugs?

[GitHub issues](https://github.com/input-output-hk/dbsync/issues).
Include:

- The profile JSON (redact credentials).
- The dbsync CLI invocation.
- Relevant log lines (set `logging.level = "debug"` and reproduce if
  you can).
- The Cardano network you're on (mainnet / preprod / preview /
  custom).
- The OS and `cardano-node` version.

## How stable is the schema?

The schema is versioned and fingerprinted. When an upgrade changes it,
dbsync migrates a behind database in place at boot rather than forcing a
re-sync; a database built by a newer binary, or one whose shape drifted
with no migration, refuses to start. Profile changes — enabling or
disabling an extractor — are the exception and still need a fresh sync.
