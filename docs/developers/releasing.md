---
id: releasing
title: Releasing
sidebar_position: 15
---

# Releasing

How a dbsync release is built, verified, and published. Everything is
packaging — no nix, no external build farm; the whole pipeline is
`.github/workflows/release.yml` plus a stock multi-stage `Dockerfile`
that any contributor can build locally with `docker build`.

## What a release produces

| Artifact | Where |
|---|---|
| Multi-arch docker image (linux/amd64 + linux/arm64), `dbsync` executable only | `ghcr.io/input-output-hk/dbsync:X.Y.Z` |
| `:latest` tag | applied only when the GitHub Release is published |
| Binary tarballs `dbsync-X.Y.Z-linux-{x86_64,aarch64}.tar.gz` + `SHA256SUMS` | GitHub Release assets |
| "Tested with cardano-node N" statement | release notes, from the `tag:` in `cabal.project` |

The linux binaries are dynamically linked against glibc ≥ 2.35
(Ubuntu 22.04+) and need `libpq5 liblmdb0 libsnappy1v5 liburing2
libgmp10` at runtime. The IOG crypto libraries (libsodium VRF fork,
secp256k1, blst) are statically linked — see
[IOG library pins](#iog-library-pins).

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
   - `check` — tag matches the cabal version; IOG lib pins are fresh.
   - `linux/amd64` + `linux/arm64` — native runners, never QEMU. Build
     the image, push it by digest, extract the binary for the tarball.
   - `docker-manifest` — stitches the multi-arch `X.Y.Z` tag and
     smoke-tests `docker run --help` in the runtime image.
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

Expected timings: `check` seconds; each linux build **~1–1.5 h cold**,
~10–20 min once the per-arch `buildcache-*` registry cache is warm;
manifest + smoke ~1 min.

## When a run fails

- **Infra flake** (download blip, evicted runner): *Re-run failed jobs*
  on the run page. Same commit, warm cache.
- **Real fix**: re-running is useless — it reuses the same SHA. Fix on
  a branch, merge, dispatch again. For a failed *tag* run, also move
  the tag:

  ```bash
  git push origin :refs/tags/vX.Y.Z
  git tag -f vX.Y.Z && git push origin vX.Y.Z
  ```

  and delete the stale draft release.
- **Cost of iteration** depends on where it failed: anything before or
  after the `cabal build --only-dependencies` layer is minutes; a
  failure *inside* that layer costs a full cold rebuild (buildx only
  exports the registry cache on success). Early steps (GHC/cabal
  bindists, protoc, `build-iog-libs.sh`) reproduce locally in minutes with
  `docker build --target builder .` — the failing step aborts the build
  long before the expensive layer.

## IOG library pins

`scripts/release/iog-lib-pins.env` vendors the git revisions of the
libsodium VRF fork, secp256k1, and blst. They are not hand-picked: they
are read out of **cardano-node's own `flake.lock`** (plain JSON, no nix
involved) at the exact tag `cabal.project` pins, so release builds link
the same crypto library revisions the node itself ships with.

When bumping the cardano-node tag in `cabal.project`:

```bash
scripts/release/update-iog-lib-pins.sh   # regenerates the pins file
git add scripts/release/iog-lib-pins.env
```

Forgetting this cannot slip through: the `check` job runs
`update-iog-lib-pins.sh --check` and fails the release if the vendored
pins are stale.

## One-time repository setup

- **Tag ruleset** — Settings → Rules → New tag ruleset, target `v*`,
  restrict creation to maintainers. Tags trigger builds, so tag
  creation is the permission that gates the pipeline.
- **Package visibility** — the first push creates the ghcr `dbsync`
  package as private; make it public so users can pull anonymously.
