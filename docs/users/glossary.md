---
id: glossary
title: Glossary
sidebar_position: 7
---

# Glossary

Terms that come up across the documentation.

## Cardano concepts

**Block** — A signed batch of transactions, produced by the slot
leader of a given slot. The fundamental unit dbsync indexes; one
`block` row per block.

**Transaction** (tx) — A signed batch of inputs, outputs,
certificates, and metadata. One `tx` row per transaction; the
`utxo`, `multi_asset`, `metadata`, and other extractors hang their
rows off `tx`.

**Slot** — A 1-second window in which a block may or may not be
produced. Mainnet has ~432,000 slots per epoch.

**Epoch** — A fixed-length window of slots — 5 days on mainnet
(432,000 slots). The unit at which dbsync commits Ingest progress
and at which the ledger worker produces snapshots.

**UTxO** — Unspent Transaction Output. The Cardano ledger model:
every transaction consumes UTxOs (as inputs) and produces UTxOs
(as outputs). The `utxo` extractor records both sides.

**Multi-asset** — Native (non-ADA) tokens. Stored in
`multi_asset` and referenced from `ma_tx_out` rows.

**Datum** / **Redeemer** / **Script** — Plutus smart-contract
ingredients. Recorded by the `scripts_datums` extractor.

**Slot leader** — The pool that produced a given block. Resolved via
the VRF key in the block header; dbsync writes one `slot_leader` row
per pool, deduped through the dedup table.

**HFC** — Hard Fork Combinator. The Cardano consensus layer that
sequences the protocol's era transitions (Byron → Shelley → Allegra
→ Mary → Alonzo → Babbage → Conway → ...). dbsync's parser
collapses era-specific block layouts into a single `GenericBlock`
type before extractors run.

## Protocols

**n2c** — Node-to-Client. The Unix-socket protocol dbsync uses to
follow the node. ChainSync, LocalStateQuery, and LocalTxSubmission
are the three sub-protocols spoken over it; dbsync uses ChainSync
and LocalStateQuery.

**ChainSync** — The Ouroboros mini-protocol for streaming blocks
from a node. Each `MsgForward` carries a new block; each
`MsgRollback` carries a rollback target.

**LocalStateQuery** — The Ouroboros mini-protocol for one-shot
queries against the node's ledger state. dbsync uses it to compute
per-block `SlotDetails` (wall-clock time from a slot number).

## dbsync concepts

**Config** — The JSON file you pass via `--config`. Decides which
projections run and how dbsync behaves. Contains no connection
details — those live in the pg-config file (`--pg-config`). See
[The config file](profiles/overview).

**Profile** — The `db_profile` section of the config: the set of
enabled projections. The presets shipped in `config-examples/` are
named after their profile.

**Projection** / **Extractor** — A pure mapping from a parsed block
to a set of database rows. Each owns a set of tables and is enabled
independently. `core` is unconditional; everything else is opt-in.

**Phase** — One of four lifecycle stages dbsync moves through:
`IngestChainHistory` (bulk-load), `PreparingForVolatileTail`
(post-load setup), `FollowingVolatileTail` (steady-state),
`FollowingChainTip` (idle at tip). See
[Phases overview](/developers/phases/overview) on the developer side.

**Ingest** / **Follow** — Shorthand for the bulk-load and
steady-state phases respectively. They differ in writer
implementation (COPY vs hasql) and ID strategy (counter+dedup vs
sequence-allocator) but share the same extractor bodies.

**Rollback boundary** — `nodeTip − k`. The slot below which the
chain is immune to rollback. The Ingest phase exits when the
consumer crosses this boundary, because beyond it dbsync needs the
per-block transactional path Follow provides.

**`k`** — The protocol security parameter, 2160 on mainnet. The
maximum rollback depth. Rollbacks deeper than `k` violate
protocol-level guarantees; dbsync panics rather than silently
corrupting the database.

## Storage

**COPY** — PostgreSQL's bulk-load wire protocol. Tab-separated
text rows streamed over a libpq connection in `COPY ... FROM
STDIN` mode. The fastest way to bulk-insert into PG; dbsync's
Ingest path uses one COPY connection per table.

**hasql** — Haskell PostgreSQL client library. Backs the Follow
path's `INSERT` statements and the Preparing pass. Chosen for its
prepared-statement caching and `Pipeline` support (one-round-trip
batched query execution).

**UNLOGGED** — A PostgreSQL table mode that skips WAL writes. Used
during Ingest; tables are flipped to `LOGGED` at the end of the
Preparing pass.

**WAL** — Write-Ahead Log. PostgreSQL's durability mechanism. dbsync
intentionally skips it during Ingest (UNLOGGED tables) and benefits
from `wal_level = minimal` during the LOGGED flip.

**LSM-tree** — Log-Structured Merge tree. The on-disk key-value
store backing the Ingest dedup stores, the UTxO scratch state, and
(when enabled) the Cardano LedgerDB. Optimised for write-heavy
workloads with bursty traffic and periodic compaction.

**LedgerDB** — The V2 ledger state from `ouroboros-consensus`. An
LSM-tree-backed on-disk UTxO set with in-memory caches. Optional —
controlled by the profile's `ledger.enabled` flag.

## ID assignment

**DedupStore** — An LSM-tree mapping a natural key (hash) to the
assigned database ID. Used during Ingest for `stake_address`,
`multi_asset`, `pool_hash`, `slot_leader`, `cost_model`.

**Counter** — A per-table in-process monotonic counter that hands
out the next ID. Used during Ingest because COPY has no return
channel for `RETURNING id`.

**Sequence** — A PostgreSQL `SERIAL`-style ID source. Attached to
each table at the end of the Preparing pass and used by Follow's
ID allocator.

## Side channels

**Ledger worker** — The optional in-RAM ledger-state worker. When
on, applies blocks to the LedgerDB and produces per-block deposit
maps and per-epoch reward / stake / protocol-param data.

**TxOut worker** — A per-epoch worker active during Ingest that
back-fills `tx_out.address_id` and `tx_out.consumed_by_tx_id`.

**OffChain fetcher** — Background workers that fetch pool and
governance-vote metadata over HTTP, off the hot path. Enabled by the
`off_chain_pools` / `off_chain_votes` options.

## State files

**`<state-dir>/dbsync-ledger/`** — The on-disk LedgerDB and
snapshots when ledger is enabled. Survives across restarts.

**`<state-dir>/dbsync-ledger/ingest-lsm/`** — The Ingest LSM session:
DedupStore tables + UtxoStore. Wiped at the Ingest → Prep handoff.

**`dbsync_sync_state`** — The singleton PG table that records sync
progress. The truth source for resume: `last_committed_slot` is
where dbsync picks up after a restart.
