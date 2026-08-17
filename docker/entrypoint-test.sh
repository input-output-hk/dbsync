#!/usr/bin/env bash
# Behaviour check for docker/entrypoint.sh: runs it in a bare ubuntu container
# against a stub dbsync that prints its argv, and asserts on the rendered
# command line and pg-config. Needs docker; no image build required.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Stub dbsync: one arg per line, then the rendered pg-config contents.
cat >"$tmp/dbsync" <<'EOF'
#!/bin/sh
for a in "$@"; do echo "ARG:$a"; done
while [ $# -gt 1 ]; do
  if [ "$1" = "--pg-config" ]; then echo "PGCONFIG:"; cat "$2"; fi
  shift
done
EOF
chmod +x "$tmp/dbsync"

mkdir -p "$tmp/cardano/mainnet" "$tmp/secrets"
echo '{}' >"$tmp/cardano/mainnet/config.json"
echo 'hunter2' >"$tmp/secrets/password"

run() { # env assignments as args, e.g. run PROFILE=utxo-only ...
  local envs=()
  for e in "$@"; do envs+=(-e "$e"); done
  docker run --rm \
    -v "$repo_root/docker/entrypoint.sh":/usr/local/bin/entrypoint.sh:ro \
    -v "$tmp/dbsync":/usr/local/bin/dbsync:ro \
    -v "$repo_root/config-examples":/opt/dbsync/profiles:ro \
    -v "$tmp/cardano":/opt/cardano:ro \
    -v "$tmp/secrets":/secrets:ro \
    "${envs[@]}" ubuntu:22.04 entrypoint.sh
}

fail() {
  echo "FAIL: $1" >&2
  exit 1
}

# 1. args mode passes through verbatim
docker run --rm \
  -v "$repo_root/docker/entrypoint.sh":/usr/local/bin/entrypoint.sh:ro \
  -v "$tmp/dbsync":/usr/local/bin/dbsync:ro \
  ubuntu:22.04 entrypoint.sh --help | grep -qx 'ARG:--help' \
  || fail "args mode did not pass through"

# 2. env mode renders the full command line and pg-config
out=$(run PROFILE=utxo-only NETWORK=mainnet POSTGRES_HOST=pg POSTGRES_DB=dbsync \
          POSTGRES_PASSWORD_FILE=/secrets/password \
          DBSYNC_EXTRA_ARGS='--rollback-to-slot 123')
grep -qx 'ARG:/opt/dbsync/profiles/utxo-only.json' <<<"$out" || fail "profile path"
grep -qx 'ARG:/opt/cardano/mainnet/config.json' <<<"$out" || fail "node config path"
grep -qx 'ARG:/ipc/node.socket' <<<"$out" || fail "socket default"
grep -qx 'ARG:/var/lib/dbsync' <<<"$out" || fail "state dir default"
grep -qx 'ARG:--rollback-to-slot' <<<"$out" || fail "extra args not word-split"
grep -qx 'ARG:123' <<<"$out" || fail "extra args value"
grep -q '"host": "pg"' <<<"$out" || fail "pg host"
grep -q '"port": 5432' <<<"$out" || fail "pg port default"
grep -q '"password_file": "/secrets/password"' <<<"$out" || fail "password_file"

# 3. required settings enforced
run PROFILE=utxo-only NETWORK=mainnet POSTGRES_DB=d 2>/dev/null \
  && fail "missing POSTGRES_HOST accepted" || true
run PROFILE=nope NETWORK=mainnet POSTGRES_HOST=h POSTGRES_DB=d 2>/dev/null \
  && fail "unknown PROFILE accepted" || true
run PROFILE=utxo-only NETWORK=nope POSTGRES_HOST=h POSTGRES_DB=d 2>/dev/null \
  && fail "unknown NETWORK accepted" || true
run PROFILE=utxo-only NETWORK=mainnet POSTGRES_HOST=h POSTGRES_DB=d \
    POSTGRES_PASSWORD_FILE=relative/path 2>/dev/null \
  && fail "relative POSTGRES_PASSWORD_FILE accepted" || true

echo "entrypoint-test: all checks passed"
