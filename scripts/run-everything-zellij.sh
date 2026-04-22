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

# Find dbsync binary
dbsync="$(find "$PROJECT_DIR"/dist-newstyle -name dbsync -type f | head -1)"

if [ -z "$dbsync" ]; then
    echo "ERROR: dbsync binary not found in: $PROJECT_DIR/dist-newstyle"
    echo "Build the project first with: cabal build dbsync"
    exit 1
fi

echo "Using dbsync binary: $dbsync"

# Default profile (uses the full-config test fixture)
PROFILE="${PROFILE:-$PROJECT_DIR/profiles/everything-profile.json}"

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
zellij --layout <(cat <<EOF
layout {
    pane split_direction="vertical" {
        pane name="cardano-node" focus=true {
            command "bash"
            args "-c" "cd $CARDANO_NODE_DIR/ && cardano-node run --config $TESTNET_DIR/config.json --database-path $TESTNET_DIR/db/ --socket-path $TESTNET_DIR/db/node.socket --host-addr 0.0.0.0 --port 1337 --topology $TESTNET_DIR/topology.json"
        }
        pane name="cardano-db-sync" {
            command "bash"
            args "-c" "cd $PROJECT_DIR/ && echo 'Waiting for node socket...' && sleep 5 && $dbsync --db-sync-config $TESTNET_DIR/db-sync-config.json --socket-path $TESTNET_DIR/db/node.socket --state-dir $TESTNET_DIR/ledger-state --profile $PROFILE"
        }
    }
}
EOF
)
