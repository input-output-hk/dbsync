---
id: faq
title: FAQ
sidebar_position: 6
---

# Frequently asked questions

## How much disk space do I need?

It depends on which extractors you enable. `cbor` is by far the
largest, and it dominates the total in any config that enables it.
Disabling it roughly halves the database.

[Prerequisites](installation/prerequisites#disk-space) holds the
sizing table and the caveats that go with it.

## Do I need ledger state for my use case?

Enable `ledger` if you need:

- Reward amounts per epoch (`reward`).
- Per-epoch stake distribution (`epoch_stake`).
- Protocol parameter snapshots per epoch (`epoch_param`).
- ADA pot balances (`ada_pots`).
- Per-tx deposit values on the `tx` row.

Skip it — use `everything-no-ledger` — if you only need block-derived
data: UTxO, certificates, metadata, native tokens, and governance
actions as recorded on-chain.

The ledger replay is the largest RAM consumer in a dbsync run, and it
adds a full replay to the catch-up time. Enable it only if you need
one of the tables above.

## Can I change the extractor set after a sync?

No. The set is fixed once the database is created. See
[`extractors` is fixed per database](config/overview#extractors-is-fixed-per-database).

Schema migrations handle structural changes across dbsync versions.
They do not turn an extractor on or off, because enabling one needs
its tables back-filled from genesis. Choose the extractors before you
commit to a long sync.

## What hardware do I need?

dbsync targets **4 cores / 16 GB RAM**, which is what
`scripts/postgres-tuning.conf` is written for. Bigger machines sync
faster, but the architecture does not require them.

| Component | Minimum | Comfortable |
|---|---|---|
| CPU cores | 4 | 6–8 |
| RAM | 16 GB | 32 GB, more with `ledger` enabled |
| Disk | see [Prerequisites](installation/prerequisites#disk-space) | NVMe / local SSD |
| Network | enough to keep up with block production | any modern broadband |

Disk speed matters more than CPU. The Ingest path is bound by write
bandwidth to PostgreSQL. On a slow disk, PG's WAL writes dominate.
Network-mounted PG data directories cost a lot.

## How does this compare to the original cardano-db-sync?

Same schema in most places, different engine:

- **Faster initial sync.** The bulk-load path uses parallel COPY
  streams, pre-assigned ids, and UNLOGGED tables. The original is
  closer to per-transaction `INSERT`.
- **Modular extractors.** You choose which extractors run. The
  original is monolithic.
- **On-disk ledger.** The optional ledger worker keeps its state in an
  LSM-tree on disk rather than in memory.

Most queries that worked against the original work against this one
with a matching extractor set. The schema is close but not identical,
so validate your queries against a sample sync before you switch.

## Can I run several dbsync instances against one node?

Yes, if each one has its own PostgreSQL database and its own
`--ledger-state-dir`. The node's n2c socket accepts several clients at
once.

This is useful for running a small fast index beside a large one that
is still catching up.

## Can I run dbsync against a remote PostgreSQL?

Yes, with one caveat.

Set `host` and `port` in the `--pg-config` file. Ingest opens **one
connection per table**, so with every extractor enabled that is more
than 70 connections. Raise `max_connections` to match.

:::caution
dbsync shells out to `psql` to create and drop the schema, and those
calls do **not** read the `--pg-config` file. They use `PGHOST`,
`PGPORT`, `PGUSER`, and `PGPASSWORD` instead. Set those environment
variables to match, or the schema lands on the wrong server. See
[Environment](running#environment).
:::

Latency to PG matters during Ingest. A few milliseconds costs little.
Fifty milliseconds or more, such as cross-region, slows the bulk-load
noticeably. Co-locate dbsync and PostgreSQL.

## Is there a Mithril-style snapshot for dbsync?

No. Every sync runs the extractor pipeline over the blocks the node
delivers. There is no pre-built PG dump to download.

`pg_basebackup` and filesystem snapshots serve the same purpose if you
control both ends. See
[Recovery / Backup strategy](operations/recovery#backup-strategy).

## What should I keep for later analysis?

The `epoch_sync_stats` table. It records one row per finalised epoch
with the block count, the throughput, and the elapsed time. See
[Metrics](operations/metrics).

Keep stderr logs for live monitoring and for crash post-mortems.

## Where do I report bugs?

[GitHub issues](https://github.com/input-output-hk/dbsync/issues).
Include:

- The config JSON. Redact credentials.
- The dbsync command line.
- The relevant log lines. Set `logging.level = "debug"` and reproduce
  if you can.
- The Cardano network: mainnet, preprod, preview, or custom.
- The OS and the `cardano-node` version.

## How stable is the schema?

dbsync versions and fingerprints the schema. When an upgrade changes
it, dbsync migrates the database in place at boot. No re-sync is
needed.

dbsync refuses to start in three cases:

- A newer binary built the database.
- The schema drifted with no migration to cover it.
- The extractor set in the config differs from the recorded one.

Only the third case needs a fresh sync.
