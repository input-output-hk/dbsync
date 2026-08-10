---
id: intro
title: Introduction
slug: /intro
sidebar_position: 1
---

# dbsync

A fast, modular indexer for the Cardano blockchain. dbsync follows a
running `cardano-node` over its Unix socket and writes on-chain data
into a PostgreSQL schema you control table by table.

This section is for **operators** who want to run dbsync against a
node. If you work on dbsync itself — extractors, the phase machinery,
the schema layer — read the [Developers section](/developers/intro)
instead.

## What it does

For every block the node hands it, dbsync:

1. Parses the block once into an era-independent representation.
2. Runs the enabled **extractors** over it. Each one covers a domain:
   UTxO, multi-asset, governance, and so on.
3. Writes the resulting rows into your PostgreSQL database.

Each extractor is independent. You choose which ones run when you
create the database. Disabling one skips both the work and the tables
it would own.

## What's in this section

- [Installation](installation/prerequisites) — prerequisites, then
  platform-specific instructions for Linux and macOS, then the build.
- [Cardano node setup](node-setup) — running the `cardano-node`
  dbsync follows.
- [Configuration](config/overview) — the config file, the six
  presets that ship, and how to write your own.
- [Running dbsync](running) — CLI flags, environment, first-run
  expectations.
- [Operations](operations/metrics) — metrics, troubleshooting,
  recovery and restart semantics.
- [FAQ](faq) and [Glossary](glossary).

## Status

:::warning Pre-release
A dbsync upgrade migrates the database at boot. A schema change does
not force a re-sync.

**A change to the extractor set does.** If you enable or disable an
extractor, you must sync a new database from genesis. Choose the
extractors once, per database, before you start.
:::
