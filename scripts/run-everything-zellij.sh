#!/bin/bash

set -e

# Determine project root directory (parent of scripts/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Set default paths (can be overridden by environment variables)
HOMEIOG="${HOMEIOG:-$HOME/Code/IOG}"
CARDANO_NODE_DIR="${CARDANO_NODE_DIR:-$HOMEIOG/cardano-node}"
TESTNET_DIR="${TESTNET_DIR:-$HOMEIOG/testnet}"

# Verify required directories exist
if [ ! -d "$CARDANO_NODE_DIR" ]; then
    echo "ERROR: cardano-node directory not found at: $CARDANO_NODE_DIR"
    echo "Set CARDANO_NODE_DIR environment variable or update HOMEIOG path"
    exit 1
fi

if [ ! -d "$TESTNET_DIR" ]; then
    echo "ERROR: testnet directory not found at: $TESTNET_DIR"
    echo "Set TESTNET_DIR environment variable or update HOMEIOG path"
    exit 1
fi

# Find the dbsync binary. DBSYNC_BIN overrides autodetection: when more
# than one build variant exists under dist-newstyle, `find | head -1`
# picks an arbitrary one; the boot log's "binary … (linked …)" line
# confirms which one actually ran.
dbsync="${DBSYNC_BIN:-$(find "$PROJECT_DIR"/dist-newstyle -name dbsync -type f | head -1)}"

if [ -z "$dbsync" ]; then
    echo "ERROR: dbsync binary not found in: $PROJECT_DIR/dist-newstyle"
    echo "Build the project first with: cabal build dbsync"
    exit 1
fi

echo "Using dbsync binary: $dbsync"

# Default configs (override via CONFIG / PG_CONFIG)
CONFIG="${CONFIG:-$PROJECT_DIR/config-examples/everything.json}"
PG_CONFIG="${PG_CONFIG:-$PROJECT_DIR/config-examples/pg-config.example.json}"

# Kill any previous instances
echo "Cleaning up previous instances..."
pkill -f cardano-node || true
pkill -f dbsync || true
sleep 1

echo "Cleanup complete. Starting services..."

# Layout:
#  ┌──────────────────┬──────────────────┐
#  │   cardano-node   │  cardano-db-sync │
#  └──────────────────┴──────────────────┘
# cardano-node is launched with `+RTS -N -A64m -RTS`:
#   -N      use all 10 cores (8 P + 2 E on this Apple Silicon Mac) for the GHC
#           work-stealing runtime; parallelises sig-verification, Plutus eval, GC.
#   -A64m   bump GC young-gen nursery from 4 MB to 64 MB; cuts minor-GC frequency
#           by ~16x during the heavy-allocation ledger replay.
#
# When $dbsync was built with -f ghc-debug, the stub serves a heap-inspection
# socket in the XDG data dir, which ghc-debug-brick auto-discovers. Export
# GHC_DEBUG_SOCKET before running to pin an explicit path instead; unset it
# for auto-discovery.
zellij --layout <(cat <<EOF
layout {
    pane split_direction="vertical" {
        pane name="cardano-node" focus=true {
            command "bash"
            args "-c" "cd $CARDANO_NODE_DIR/ && cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json +RTS -N -A64m -RTS"
        }
        pane name="cardano-db-sync" {
            command "bash"
            args "-c" "cd $PROJECT_DIR/ && echo 'Starting DbSync...' && ${GHC_DEBUG_SOCKET:+GHC_DEBUG_SOCKET=$GHC_DEBUG_SOCKET }$dbsync --config $CONFIG --pg-config $PG_CONFIG --node-config $TESTNET_DIR/config.json --socket-path $TESTNET_DIR/db/node.socket --ledger-state-dir $TESTNET_DIR "
      }
    }
}
EOF
)
