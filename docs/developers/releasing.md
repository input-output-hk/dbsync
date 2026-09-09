---
id: releasing
title: Releasing
sidebar_position: 15
---

# Releasing

How a dbsync release is built, verified, and published. Binaries are built
by nix from `flake.nix` (the single build definition); docker images are a
plain `Dockerfile` that copies a prebuilt binary in — nix never builds
images. The pipeline is `.github/workflows/release.yml`.

Builds run against `cache.iog.io`: when IOG's Hydra
([ci.iog.io](https://ci.iog.io), jobset `input-output-hk-dbsync`) has
already built the revision, every `nix build` is a cache substitution and
finishes in minutes. When it hasn't, the runner builds cold (hours) — slow,
but never blocked on IOG infra.

## What a release produces

| Artifact | Where |
|---|---|
| Multi-arch docker image (linux/amd64 + linux/arm64), `dbsync` executable only | `ghcr.io/input-output-hk/dbsync:X.Y.Z` |
| `:latest` tag | applied only when the GitHub Release is published |
| Static binary tarballs `dbsync-X.Y.Z-linux-{x86_64,aarch64}.tar.gz` | GitHub Release assets |
| `dbsync-X.Y.Z-macos-aarch64.tar.gz` (native, dylibs bundled relocatable) | GitHub Release assets |
| `SHA256SUMS` | GitHub Release assets |
| "Tested with cardano-node N" statement | release notes, from the `tag:` in `cabal.project` |

The linux binaries are fully static (musl): no glibc floor, no runtime
packages, they run on any distro — including bare alpine. The macOS binary
is dynamically linked but ships its non-system dylibs alongside, rewritten
to `@executable_path`, so the tarball is self-contained.

## The release cycle

A tag builds everything into a **draft**; a human publishes it. Nothing
is announced, and `:latest` never moves, until the publish.

1. **Version bump PR** — set `version:` in `dbsync/dbsync.cabal`.
   Merge to `main`.
2. **Tag** — on the merge commit:

   ```bash
   git checkout main && git pull
   git tag vX.Y.Z          # must equal the cabal version exactly
   git push origin vX.Y.Z
   ```

   This triggers the `Release` workflow:
   - `check` — tag matches the cabal version; flake.lock's crypto-lib
     pins match cardano-node's.
   - `build-linux` (amd64 + arm64, native runners) — `nix build
     .#dbsync-static`, sanity-check the binary in a bare alpine
     container, upload the tarball.
   - `build-macos` — `nix build .#dbsync-macos`, smoke test, upload the
     tarball.
   - `docker` — one buildx run assembles and pushes the multi-arch
     `X.Y.Z` image from the two static binaries, then smoke-tests the
     amd64 leg; `smoke-arm64` pulls the pushed image on an arm runner to
     catch a wrong-architecture binary.
   - `release` — attaches tarballs + `SHA256SUMS` to a **draft** GitHub
     Release with auto-generated notes.
3. **Verify the draft** — assets present, notes sensible,
   `docker run --rm ghcr.io/input-output-hk/dbsync:X.Y.Z --help` works.
4. **Publish** — the `Promote release` workflow points `:latest` at
   `X.Y.Z`. Done.

A published release is immutable history: never delete or re-tag it.
A broken draft, on the other hand, is disposable — delete the draft and
the tag, fix, re-tag.

## Dry-running the pipeline

`Release` also has a `workflow_dispatch` trigger: Actions → Release →
Run workflow. A dispatch run is identical to a tag run except no draft
release is created (and the tag==version check is skipped). Use it to
rehearse the pipeline after workflow changes without touching tags.

Timings are dominated by one question: has Hydra (or a previous run)
already populated `cache.iog.io` for this revision? Substituted: each
build job is minutes. Cold: ~3 h per platform (full GHC bootstrap
included). The docker job itself is ~2 min either way.

## When a run fails

- **Infra flake** (download blip, evicted runner): *Re-run failed jobs*
  on the run page.
- **Real fix**: re-running is useless — it reuses the same SHA. Fix on
  a branch, merge, dispatch again. For a failed *tag* run, also move
  the tag:

  ```bash
  git push origin :refs/tags/vX.Y.Z
  git tag -f vX.Y.Z && git push origin vX.Y.Z
  ```

  and delete the stale draft release.
- **Reproducing locally**: `nix build .#dbsync-static` on any linux
  machine (or `.#dbsync-macos` on Apple Silicon) is bit-for-bit the same
  build the workflow runs, with the same cache.

## Crypto library pins

The IOG crypto libraries (libsodium VRF fork, secp256k1, blst) come from
the `iohkNix` flake input and are recorded in `flake.lock`. They must
match the revisions cardano-node itself builds against at the tag
`cabal.project` pins — the `check` job runs
`scripts/release/check-crypto-pins.sh`, which diffs our `flake.lock`
against cardano-node's (both plain JSON, no nix involved) and fails the
release on drift.

When bumping the cardano-node tag in `cabal.project`, refresh and
re-check:

```bash
nix flake update iohkNix
scripts/release/check-crypto-pins.sh
git add flake.lock
```

## One-time repository setup

- **Tag ruleset** — Settings → Rules → New tag ruleset, target `v*`,
  restrict creation to maintainers. Tags trigger builds, so tag
  creation is the permission that gates the pipeline.
- **Package visibility** — the first push creates the ghcr `dbsync`
  package as private; make it public so users can pull anonymously.
