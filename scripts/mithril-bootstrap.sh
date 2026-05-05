#!/bin/bash

# mithril-bootstrap.sh
#
# Bootstrap cardano-node's database directory from a Mithril snapshot so we
# don't have to sync mainnet from genesis at the network layer. The script
# downloads the verified ImmutableDB + VolatileDB; cardano-node then replays
# the LedgerDB from genesis using that local immutable chain (which is
# orders of magnitude faster than fetching blocks from peers).
#
# Why no ledger-state download / conversion?
#   Earlier versions of this script downloaded the Mithril ancillary archive
#   too (a Mem-format ledger snapshot at the immutable tip) and converted it
#   to the UTxO-HD flavor configured in the node (V2LSM / V1LMDB / V2InMemory)
#   via cardano-node's `snapshot-converter`. That path was unreliable across
#   node versions: cardano-node 10.7 in particular would silently reject the
#   converted snapshot (missing 'tablesCodecVersion' metadata) and then
#   replay from genesis anyway.
#
#   We removed the ancillary download and the conversion phase entirely.
#   With the 'early-n2c-socket' consensus changes (two-phase ChainDB.openDB,
#   lazy NodeKernel, forked Node.lateInit thread), the n2c Unix socket is
#   bindable within seconds of node startup and dbsync can start consuming
#   immutable-chain blocks via NtC ChainSync while the LedgerDB replay
#   continues in the background. The "skip the replay" optimisation is no
#   longer worth its operational cost.
#
# Phases:
#   1. Install mithril-client (if missing).
#   2. Download a verified Cardano DB snapshot from the release-mainnet
#      Mithril aggregator (immutable + volatile DBs only).
#
# Re-runs:
#   The script refuses to overwrite an existing db/. If <node-dir>/db
#   already exists you must remove or rename it before re-running.
#
# Prerequisites:
#   - cardano-node STOPPED before running this script.
#   - curl and tar on PATH.
#
# mithril-client:
#   The script checks for mithril-client on PATH. If it is missing, you will
#   be prompted to download a pre-built binary from the official Mithril
#   GitHub releases into $HOME/.local/bin. Pass -y / --auto-install to skip
#   the prompt, or --no-install to keep the old "error if missing" behaviour.
#
#   Note: the official pre-built binaries do not support Intel Macs. On
#   Intel macOS install with `cargo install mithril-client-cli` instead.
#
# Usage:
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir>                       # REQUIRED
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir> --dry-run             # list available snapshots
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir> --digest <d>          # pin to a specific snapshot
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir> -y                    # auto-install mithril-client
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir> --no-install          # never auto-install
#   ./scripts/mithril-bootstrap.sh --db-dir <node-dir> --mithril-version <v> # pin mithril-client version
#
# --db-dir is mandatory. It points at the cardano-node directory (the folder
# containing config.json, topology.json, etc; could be named 'testnet',
# 'mainnet', 'preprod', whatever - the name is irrelevant). The script will:
#   - create that directory (and any missing parents) if it does not exist
#   - then create a fresh db/ subdirectory inside it for the Mithril download
#   - or fail if <node-dir>/db already exists, to avoid clobbering data
# mithril-client extracts into a fixed db/ subdirectory of its --download-dir,
# so we hand it <node-dir> and let it create db/ for us.

set -euo pipefail

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

# Mainnet Mithril network parameters (release-mainnet)
# Source: https://mithril.network/doc/manual/getting-started/network-configurations
# Verification key is sourced from:
#   https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey
# Hex-encoded ASCII of a JSON byte array (Ed25519 vkey, 32 bytes).
# Override via environment variable if IOG rotates keys.
AGGREGATOR_ENDPOINT="${AGGREGATOR_ENDPOINT:-https://aggregator.release-mainnet.api.mithril.network/aggregator}"

# genesis.vkey  -> decoded prefix: [191,66,140,185,138,11,237,207,...]
GENESIS_VERIFICATION_KEY="${GENESIS_VERIFICATION_KEY:-5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d}"

# Defaults that can be overridden via flags
NODE_DIR=""         # REQUIRED via --db-dir; no default. cardano-node dir.
DB_DIR=""           # Computed as $NODE_DIR/db after arg parsing.
DIGEST="latest"
DRY_RUN=0
AUTO_INSTALL=0
NO_INSTALL=0

# mithril-client install settings (only used if mithril-client is missing)
MITHRIL_INSTALL_DIR="${MITHRIL_INSTALL_DIR:-$HOME/.local/bin}"
MITHRIL_CLIENT_VERSION="${MITHRIL_CLIENT_VERSION:-}"     # empty => resolve "latest"
MITHRIL_CLIENT_VERSION_FALLBACK="2603.1"                 # used if GitHub API fails

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

usage() {
  # Print the leading block comment (everything from line 3 up to the first
  # non-comment line). Avoids hard-coded line ranges so the help block stays
  # in sync as the comment evolves.
  awk 'NR>=3 { if ($0 !~ /^#/ && $0 != "") exit; sub(/^# ?/, ""); print }' "$0"
  exit "${1:-0}"
}

while [ $# -gt 0 ]; do
  case "$1" in
    --db-dir)              NODE_DIR="$2"; shift 2 ;;
    --digest)              DIGEST="$2"; shift 2 ;;
    --dry-run)             DRY_RUN=1; shift ;;
    -y|--auto-install)     AUTO_INSTALL=1; shift ;;
    --no-install)          NO_INSTALL=1; shift ;;
    --mithril-version)     MITHRIL_CLIENT_VERSION="$2"; shift 2 ;;
    -h|--help)             usage 0 ;;
    *)
      echo "ERROR: unknown argument: $1" >&2
      usage 1
      ;;
  esac
done

if [ -z "$NODE_DIR" ]; then
  echo "ERROR: --db-dir is required (path to the cardano-node directory" >&2
  echo "       containing config.json/topology.json; e.g. /usr/code/iog/testnet)." >&2
  echo "" >&2
  usage 1
fi

# Strip trailing slash; compute the actual db dir.
NODE_DIR="${NODE_DIR%/}"
DB_DIR="$NODE_DIR/db"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

log() {
  printf '[mithril-bootstrap] %s\n' "$*"
}

err() {
  printf '[mithril-bootstrap] ERROR: %s\n' "$*" >&2
}

confirm() {
  # Prompt user with yes/no question; default no. Honours AUTO_INSTALL=1.
  local prompt="$1"
  if [ "$AUTO_INSTALL" = "1" ]; then
    return 0
  fi
  local reply
  read -r -p "$prompt [y/N]: " reply
  case "$reply" in
    [yY]|[yY][eE][sS]) return 0 ;;
    *) return 1 ;;
  esac
}

detect_platform() {
  local s m
  s="$(uname -s)"
  m="$(uname -m)"
  case "$s" in
    Darwin) PLATFORM_OS="macos" ;;
    Linux)  PLATFORM_OS="linux" ;;
    *)
      err "Unsupported OS: $s. Pre-built binaries cover macOS (arm64) and Linux only."
      err "Install manually: cargo install mithril-client-cli"
      exit 1
      ;;
  esac
  case "$m" in
    x86_64|amd64)  PLATFORM_ARCH="x64" ;;
    arm64|aarch64) PLATFORM_ARCH="arm64" ;;
    *)
      err "Unsupported CPU architecture: $m"
      exit 1
      ;;
  esac
  if [ "$PLATFORM_OS" = "macos" ] && [ "$PLATFORM_ARCH" = "x64" ]; then
    err "Pre-built mithril-client binaries do not support Intel Macs."
    err "On Intel macOS, install with:"
    err "  cargo install mithril-client-cli"
    err "or run this script on an Apple Silicon Mac."
    exit 1
  fi
}

resolve_mithril_version() {
  if [ -n "$MITHRIL_CLIENT_VERSION" ]; then
    printf '%s\n' "$MITHRIL_CLIENT_VERSION"
    return 0
  fi
  local v
  # `|| true` defuses set -e/pipefail when curl or grep fail (no network, rate limited).
  v="$(curl -sSfL --max-time 10 \
        https://api.github.com/repos/input-output-hk/mithril/releases/latest 2>/dev/null \
        | grep '"tag_name"' \
        | head -1 \
        | cut -d'"' -f4 || true)"
  if [ -z "$v" ]; then
    err "Could not resolve latest mithril release from GitHub API."
    err "Falling back to hard-coded version: $MITHRIL_CLIENT_VERSION_FALLBACK"
    v="$MITHRIL_CLIENT_VERSION_FALLBACK"
  fi
  printf '%s\n' "$v"
}

install_mithril_client() {
  detect_platform

  local version url tmpdir tarball binary_path target had_install_dir_in_path
  version="$(resolve_mithril_version)"
  url="https://github.com/input-output-hk/mithril/releases/download/${version}/mithril-${version}-${PLATFORM_OS}-${PLATFORM_ARCH}.tar.gz"
  target="$MITHRIL_INSTALL_DIR/mithril-client"

  log "Detected platform: ${PLATFORM_OS}/${PLATFORM_ARCH}"
  log "Mithril version:   ${version}"
  log "Download URL:      ${url}"
  log "Install path:      ${target}"

  if [ -e "$target" ]; then
    local existing_version
    existing_version="$("$target" --version 2>/dev/null || echo unknown)"
    log "Existing binary found: $existing_version"
    if ! confirm "Overwrite existing mithril-client?"; then
      err "Aborted by user."
      exit 1
    fi
  else
    if ! confirm "Install mithril-client to ${target}?"; then
      err "Aborted by user."
      exit 1
    fi
  fi

  mkdir -p "$MITHRIL_INSTALL_DIR"
  tmpdir="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpdir'" EXIT
  tarball="$tmpdir/mithril.tar.gz"

  log "Downloading..."
  if ! curl -fL --progress-bar -o "$tarball" "$url"; then
    err "Download failed: $url"
    exit 1
  fi

  log "Extracting..."
  tar -xzf "$tarball" -C "$tmpdir"
  binary_path="$(find "$tmpdir" -type f -name 'mithril-client' | head -1)"
  if [ -z "$binary_path" ]; then
    err "mithril-client binary not found inside tarball."
    exit 1
  fi

  mv "$binary_path" "$target"
  chmod +x "$target"

  if [ "$PLATFORM_OS" = "macos" ]; then
    # Newer macOS adds a quarantine xattr to curl-downloaded binaries; clear it.
    xattr -d com.apple.quarantine "$target" 2>/dev/null || true
  fi

  # Was the install dir on PATH *before* we extended it?
  case ":$PATH:" in
    *":$MITHRIL_INSTALL_DIR:"*) had_install_dir_in_path=1 ;;
    *)                          had_install_dir_in_path=0 ;;
  esac
  export PATH="$MITHRIL_INSTALL_DIR:$PATH"

  if [ "$had_install_dir_in_path" = "0" ]; then
    log ""
    log "NOTE: $MITHRIL_INSTALL_DIR is not on your persistent PATH."
    log "Add this line to your shell rc (~/.zshrc on macOS, ~/.bashrc on Linux):"
    log "  export PATH=\"$MITHRIL_INSTALL_DIR:\$PATH\""
    log ""
  fi

  if ! "$target" --version >/dev/null 2>&1; then
    err "Installed mithril-client but verification failed (cannot run --version)."
    exit 1
  fi

  log "mithril-client installed: $("$target" --version 2>/dev/null)"
}

ensure_mithril_client() {
  if command -v mithril-client >/dev/null 2>&1; then
    return 0
  fi
  log "mithril-client not found on PATH."
  if [ "$NO_INSTALL" = "1" ]; then
    cat >&2 <<'EOF'

--no-install was passed; refusing to install automatically.

Install with one of:

  # macOS (Apple Silicon) / Linux release binary:
  curl -sSfL \
    "https://github.com/input-output-hk/mithril/releases/latest" \
    >/dev/null  # check assets, then:
  # download the matching mithril-* tarball, extract, place mithril-client on PATH.

  # Or with cargo:
  cargo install mithril-client-cli

EOF
    exit 1
  fi
  install_mithril_client
}

# ---------------------------------------------------------------------------
# Pre-flight checks
# ---------------------------------------------------------------------------

# Ensure the cardano-node directory exists. cardano-node would create this on
# startup; doing it here lets users point --db-dir at a brand-new path.
if [ ! -d "$NODE_DIR" ]; then
  log "Creating cardano-node dir (and missing parents): $NODE_DIR"
  mkdir -p "$NODE_DIR"
fi

# Make sure mithril-client is available (needed for both download and dry-run).
ensure_mithril_client

if command -v mithril-client >/dev/null 2>&1; then
  log "mithril-client:    $(command -v mithril-client)"
  log "  version:         $(mithril-client --version 2>/dev/null || echo unknown)"
fi
log "Aggregator:        $AGGREGATOR_ENDPOINT"
log "Cardano-node dir:  $NODE_DIR"
log "Target db dir:     $DB_DIR  (mithril will create db/ here)"

export AGGREGATOR_ENDPOINT
export GENESIS_VERIFICATION_KEY

# ---------------------------------------------------------------------------
# Dry run: just list snapshots and exit (does not touch $DB_DIR)
# ---------------------------------------------------------------------------

if [ "$DRY_RUN" = "1" ]; then
  log "Listing available cardano-db snapshots..."
  mithril-client cardano-db snapshot list
  exit 0
fi

# ---------------------------------------------------------------------------
# Refuse to clobber an existing db/
# ---------------------------------------------------------------------------

if [ -e "$DB_DIR" ]; then
  err "$DB_DIR already exists."
  err "This script will not overwrite an existing db directory. Either:"
  err "  - remove it:        rm -rf '$DB_DIR'"
  err "  - move it aside:    mv '$DB_DIR' '$DB_DIR.bak.\$(date +%Y%m%d-%H%M%S)'"
  err "  - point --db-dir at a different cardano-node directory"
  exit 1
fi

# ---------------------------------------------------------------------------
# Download
# ---------------------------------------------------------------------------

log "Downloading Mithril cardano-db snapshot ($DIGEST)..."
log "This will take a while (snapshot is multi-GB)."
log ""
log "NOTE: ancillary archive (Mem-format ledger snapshot) is NOT requested."
log "      cardano-node will replay the LedgerDB from genesis using the"
log "      downloaded immutable chain. With the early-n2c-socket consensus"
log "      changes, dbsync can begin consuming blocks during that replay."

# mithril-client always extracts into a fixed `db/` subdirectory of
# --download-dir, so handing it $NODE_DIR lands the result at $NODE_DIR/db
# == $DB_DIR.
mithril-client cardano-db download "$DIGEST" \
  --download-dir "$NODE_DIR"

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------

log ""
log "============================================================"
log "Snapshot ready at $DB_DIR"
log "Format: ImmutableDB + VolatileDB only (no ledger snapshot)."
log "============================================================"
log ""
log "Next steps:"
log "  1. Start cardano-node. It will replay the LedgerDB from genesis"
log "     using the local immutable chain. With the 'early-n2c-socket'"
log "     consensus changes, the n2c socket binds within seconds:"
log "       cardano-node run \\"
log "         --config        $NODE_DIR/config.json \\"
log "         --database-path $DB_DIR \\"
log "         --socket-path   $DB_DIR/node.socket \\"
log "         --topology      $NODE_DIR/topology.json"
log ""
log "  2. Within seconds you should see the 'OpenedDBImmutableReady'"
log "     trace event and the socket file should appear at"
log "     $DB_DIR/node.socket. 'cardano-cli query tip' may block until"
log "     replay completes, but it will NOT fail with 'connect: does not"
log "     exist'."
log ""
log "  3. Start dbsync immediately. The Ingest: log line should show"
log "     ~3000-5000 blk/s with 'drain 80-100/100' and status 'HEALTHY'"
log "     for the bulk of the historic chain, in parallel with the node's"
log "     ledger replay."
