#!/usr/bin/env bash
# Entrypoint contract:
#   any CLI args  -> exec dbsync "$@" (manual mode, mounted configs)
#   no args       -> env-driven: baked profile/network + rendered pg-config
#
#   NETWORK                 baked node config dir (mainnet|preprod|preview)
#   NODE_CONFIG             mounted node config path, overrides NETWORK
#   PROFILE                 baked dbsync profile name (e.g. utxo-only)
#   DBSYNC_CONFIG           mounted dbsync config path, overrides PROFILE
#   POSTGRES_HOST/PORT/DB/USER  rendered into a pg-config at start
#   POSTGRES_PASSWORD_FILE  absolute path, referenced as password_file
#   SOCKET_PATH             n2c socket        (default /ipc/node.socket)
#   LEDGER_STATE_DIR        state dir         (default /var/lib/dbsync)
#   DBSYNC_EXTRA_ARGS       appended verbatim
set -euo pipefail

if [ "$#" -gt 0 ]; then
  exec dbsync "$@"
fi

profiles_dir=/opt/dbsync/profiles
networks_dir=/opt/cardano

die() {
  echo "entrypoint: $*" >&2
  exit 1
}

# dbsync config: explicit path wins over baked profile name.
if [ -n "${DBSYNC_CONFIG:-}" ]; then
  config=$DBSYNC_CONFIG
elif [ -n "${PROFILE:-}" ]; then
  config=$profiles_dir/$PROFILE.json
  [ -f "$config" ] \
    || die "unknown PROFILE '$PROFILE' (available: $(cd "$profiles_dir" && ls ./*.json | sed 's|^\./||; s|\.json$||' | tr '\n' ' '))"
else
  die "set PROFILE (baked dbsync config) or DBSYNC_CONFIG (mounted path)"
fi

# node config: explicit path wins over baked network bundle.
if [ -n "${NODE_CONFIG:-}" ]; then
  node_config=$NODE_CONFIG
elif [ -n "${NETWORK:-}" ]; then
  node_config=$networks_dir/$NETWORK/config.json
  [ -f "$node_config" ] \
    || die "unknown NETWORK '$NETWORK' (available: $(ls "$networks_dir" | tr '\n' ' '))"
else
  die "set NETWORK (mainnet|preprod|preview) or NODE_CONFIG (mounted path)"
fi

[ -n "${POSTGRES_HOST:-}" ] || die "POSTGRES_HOST is required"
[ -n "${POSTGRES_DB:-}" ] || die "POSTGRES_DB is required"

# A relative password_file would resolve against the rendered pg-config's
# directory (/tmp), never what the operator meant.
if [ -n "${POSTGRES_PASSWORD_FILE:-}" ]; then
  case $POSTGRES_PASSWORD_FILE in
    /*) ;;
    *) die "POSTGRES_PASSWORD_FILE must be an absolute path" ;;
  esac
fi

# Render the pg-config; the password only ever arrives via password_file.
# ponytail: env values land in the JSON verbatim — quotes/backslashes in
# hostnames or db names are on the operator.
pg_config=$(mktemp -t pg-config-XXXXXX.json)
{
  printf '{\n'
  printf '  "host": "%s",\n' "$POSTGRES_HOST"
  printf '  "port": %s,\n' "${POSTGRES_PORT:-5432}"
  printf '  "name": "%s",\n' "$POSTGRES_DB"
  printf '  "user": "%s"' "${POSTGRES_USER:-postgres}"
  if [ -n "${POSTGRES_PASSWORD_FILE:-}" ]; then
    printf ',\n  "password_file": "%s"\n' "$POSTGRES_PASSWORD_FILE"
  else
    printf '\n'
  fi
  printf '}\n'
} >"$pg_config"

# DBSYNC_EXTRA_ARGS stays unquoted on purpose: the shell splits it on
# whitespace so "--rollback-to-slot 123" arrives as two argv entries.
# shellcheck disable=SC2086
exec dbsync \
  --config "$config" \
  --pg-config "$pg_config" \
  --node-config "$node_config" \
  --socket-path "${SOCKET_PATH:-/ipc/node.socket}" \
  --ledger-state-dir "${LEDGER_STATE_DIR:-/var/lib/dbsync}" \
  ${DBSYNC_EXTRA_ARGS:-}
