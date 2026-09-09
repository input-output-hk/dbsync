#!/usr/bin/env bash
# Fails if flake.lock's sodium/secp256k1/blst revisions (via the iohkNix
# input) have drifted from the revisions cardano-node itself pins, at the
# tag cabal.project's source-repository-package names. No nix involved:
# both lock files are plain JSON.
#
# Usage: scripts/release/check-crypto-pins.sh
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
our_lock=$repo_root/flake.lock

command -v jq >/dev/null || { echo "error: jq is required" >&2; exit 1; }

node_tag=$(awk '
  /location:.*IntersectMBO\/cardano-node$/ { found = 1; next }
  found && /tag:/ { print $2; exit }
' "$repo_root/cabal.project")
[ -n "$node_tag" ] || {
  echo "error: cardano-node tag not found in cabal.project" >&2
  exit 1
}

node_lock_url="https://raw.githubusercontent.com/IntersectMBO/cardano-node/${node_tag}/flake.lock"
node_lock=$(curl -sfL "$node_lock_url") || {
  echo "error: failed to fetch $node_lock_url" >&2
  exit 1
}

# $1: lib name (matches the node name in both lock files). Prints
# "owner/repo@rev" from the given lock's JSON, or fails loudly if the node
# is missing (upstream restructured its flake) rather than diffing blank.
pin() {
  jq -er --arg n "$1" '.nodes[$n].locked | "\(.owner)/\(.repo)@\(.rev)"' <<<"$2" || {
    echo "error: node '$1' missing from $3 — flake restructured?" >&2
    exit 1
  }
}

status=0
for lib in sodium secp256k1 blst; do
  ours=$(pin "$lib" "$(cat "$our_lock")" "$our_lock")
  theirs=$(pin "$lib" "$node_lock" "$node_lock_url")
  if [ "$ours" != "$theirs" ]; then
    echo "error: $lib pin drifted from cardano-node $node_tag" >&2
    echo "  ours:  $ours" >&2
    echo "  theirs: $theirs" >&2
    status=1
  fi
done

if [ "$status" -eq 0 ]; then
  echo "crypto pins match cardano-node $node_tag"
else
  echo "Run 'nix flake update iohkNix' (or wait for iohk-nix to catch up upstream), commit flake.lock, and re-check." >&2
fi
exit $status
