---
id: intro
title: Introduction
slug: /intro
sidebar_position: 1
---

# dbsync

A fast, modular indexer for the Cardano blockchain. dbsync follows a
running `cardano-node` over its Unix socket and projects on-chain data
into a PostgreSQL schema you control table by table via **profiles**.

This section is for **operators and users** who want to run dbsync to
populate a Postgres database. If you're working on dbsync itself —
extractors, the phase machinery, the schema layer — head to the
[Developers section](/developers/intro).

## What it does

For every block the node hands it, dbsync:

1. Parses the block once into an era-independent representation.
2. Runs the enabled projections (UTxO, multi-asset, governance, …)
   over it.
3. Writes the resulting rows into your PostgreSQL database.

Each projection is independent — you decide which ones run when you
create the database, and that decision is fixed for the lifetime of
that database. Disabling a projection skips both the work *and* the
tables it would own.

## What's in this section

- [Installation](installation/prerequisites) — prerequisites, then
  platform-specific instructions for Linux and macOS, then the build.
- [Cardano node setup](node-setup) — running the `cardano-node`
  dbsync follows.
- [Configuration](profiles/overview) — the config file, the six
  presets that ship, and how to write your own.
- [Running dbsync](running) — CLI flags, environment, first-run
  expectations.
- [Operations](operations/metrics) — metrics, troubleshooting,
  recovery and restart semantics.
- [FAQ](faq) and [Glossary](glossary).

## Status

:::warning Pre-release
Upgrading dbsync migrates a behind database in place at boot, so a schema
change no longer forces a re-sync. **Changing a profile** — enabling or
disabling an extractor — is different and still requires a fresh sync, as
can a change to ledger-derived data. Treat the profile as a decision made
once per database.
:::
