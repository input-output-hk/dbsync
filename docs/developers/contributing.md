---
id: contributing
title: Contributing
sidebar_position: 10
---

# Contributing

Workflow, conventions, and code style for working on dbsync.

## Branching and PRs

- Branch off `main`. PRs target `main`.
- Keep PRs focused. A new extractor is one PR; a refactor of the
  Follow loop is another.
- Use the commit history to tell a coherent story. Squash noisy
  WIP commits before opening the PR; keep the meaningful ones.
- The PR description should call out anything an operator would
  care about — schema changes, config-shape changes, performance
  changes — even if the code change is small.

## Code comments

Comments should give the next developer a brief understanding of what
a function or module does. They're not:

- A history of how the code got here. The repo is greenfield and
  pre-release; there's no backwards-compatibility story worth
  narrating.
- A full explanation of every detail. If a comment is becoming
  convoluted, the code probably needs a clearer name or a smaller
  function.
- Cross-references to plans, design docs, or prior iterations.

## Formatting and linting

The repo doesn't enforce a specific formatter via CI — running
`fourmolu` or `hlint` is a developer-side convenience, not a
requirement. Match the surrounding file's style; the per-module
defaults (`NoImplicitPrelude`, `OverloadedStrings`,
`DerivingStrategies`, `GeneralizedNewtypeDeriving`, `LambdaCase`)
are declared once in each cabal file's `common defaults`.

:::warning `-Werror` is on
The GHC warnings listed in the `common warnings` block of each
`.cabal` file are enforced as errors. `-Wall`, `-Wcompat`,
`-Wincomplete-record-updates`, `-Wincomplete-uni-patterns`,
`-Wmissing-deriving-strategies`, `-Wpartial-fields`,
`-Wunused-packages`, etc. A PR that adds warnings won't compile.

For local iteration where you want to ignore them temporarily, see
the `-Wwarn` override in
[Building from source](/users/installation/building#faster-rebuilds).
:::

## Adding a new module

1. Create the file under the right directory in `dbsync/src/`,
   `dbsync-db/src/`, or `tests/lib/`.
2. Add the module name to the `exposed-modules` list in the matching
   `.cabal` file. Tests go under `other-modules` in
   `tests/dbsync-tests.cabal`.
3. If the module needs a new dependency, add it to the same `.cabal`
   file's `build-depends`. The `-Wunused-packages` warning will
   catch unused entries on rebuild.
4. For test modules, register the spec in
   [`tests/main/Main.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/main/Main.hs)
   under the appropriate tier `describe` block.

## Adding a dependency

- Internal: just import it. The two internal libraries (`dbsync-db`,
  `dbsync-testlib`) are wired through `build-depends`.
- External: add it to the relevant `.cabal` and run `cabal build`.
  Use the version constraints already in place where possible; CHaP
  pins (cardano-* packages) and Hackage pins (everything else)
  are resolved through `cabal.project`'s `index-state` rather than
  individual upper bounds.
- New transitive bounds: if you hit a version conflict, prefer
  bumping the index state in `cabal.project` over adding an
  `allow-newer` clause.

## Repository layout reminder

```
.
├── dbsync/        # the sync engine
├── dbsync-db/     # schema layer
├── dbsync-smash/  # SMASH server (stub)
├── tests/         # tests + dbsync-testlib + dbsync-mock
├── profiles/      # shipped profile JSON
├── scripts/       # operator scripts
├── docs/          # this documentation site
└── cabal.project  # workspace + CHaP pin
```

See [Repository layout](repository-layout) for the per-directory
breakdown.

## Running the test suite locally

Before opening a PR:

```bash
cabal build all
cabal test all
```

PG-touching tests need `PostgreSQL ≥ 16` running locally with the
current user holding `CREATEDB`. See [Testing](testing) for tier
selection.

## Releases

Pre-release; no formal release process yet. A release that changes the
schema carries a migration: bump `currentSchemaVersion`, generate the
migration file with `gen-migration`, and let the pin test, boot gate,
and ladder test enforce the rest. See
[Schema versioning and migrations](schema-versioning).
