#!/bin/bash

# Run an already-built profiled `dbsync` binary with GHC profiling enabled.
#
# Build:
#   cabal --project-file=cabal.project.profiling build dbsync
#   (or set BUILD=1 below to run this for you)
#
# Run (from any directory; output lands in $PWD):
#   ./scripts/profile-dbsync.sh
#
# Generated files (prefix `dbsync`):
#   dbsync.hp     heap profile samples (render with hp2pretty)
#   dbsync.prof   cost-centre tree (%time / %alloc per function)
#   dbsync.gc     GC stats summary on exit (max residency, alloc, productivity)
#   dbsync.eventlog  optional, when EVENTLOG=1 — feed to eventlog2html
#
# Reading dbsync.prof:
#   Flat table at top is sorted by %time; %alloc finds allocation hot spots.
#   cabal.project.profiling uses `profiling-detail: toplevel-functions` which
#   gives source-level names. Switch back to `late` for more faithful timings
#   at the cost of mangled names ($wgo, $j_sXYZ).
#
# Reading dbsync.gc (for memory growth investigation):
#   "maximum residency" = peak live heap at major GC time (RAM truth source).
#   "bytes allocated"   = transient churn (drives GC, not RSS).
#   "Productivity"      = MUT / Total; >85% is healthy on this workload.
#
# Reading dbsync.hp:
#   hp2pretty dbsync.hp                # static SVG
#   eventlog2html dbsync.eventlog      # interactive HTML (when EVENTLOG=1)
#
# Memory diagnosis recipe:
#   1. Check dbsync.gc for headline max residency.
#   2. -hT (default) shows WHICH TYPES accumulate.
#   3. HEAP_MODE=-hc isolates WHICH FUNCTIONS allocate retained heap.
#   4. HEAP_MODE=-hr (slow) attributes WHO retains the heap (retainer sets).
#   5. RTS A/B tests: CORES=2 / NURSERY=8m to see how runtime overhead scales.
#
# macOS RSS caveat:
#   `ps -o rss` and htop's MemB on macOS include shared memory mappings
#   (e.g. PG's shared_buffers, lib pages, file-backed mmap). To get a
#   private-memory view of dbsync alone, use:
#     vmmap <pid> | rg 'TOTAL'
#     top -l 1 -pid <pid> -stats pid,command,mem,vsize
#
# Output-file size: if `hp2pretty` OOMs, raise SAMPLE (longer interval =
# fewer samples = smaller file), or split with scripts/hp-split.sh.
#
# Clean shutdown:
#   .prof and .gc are written ONLY on a clean exit. SIGINT (Ctrl-C) is fine
#   for the application but the RTS skips profile flush. To get .prof + .gc:
#     kill <pid>           # SIGTERM, gives the RTS a chance to flush
#   Or just wait for the run to finish naturally.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

HOMEIOG="${HOMEIOG:-$HOME/Code/IOG}"
TESTNET_DIR="${TESTNET_DIR:-$HOMEIOG/testnet}"
CONFIG_JSON="${CONFIG:-$PROJECT_DIR/config-examples/everything.json}"
PG_CONFIG_JSON="${PG_CONFIG:-$PROJECT_DIR/config-examples/pg-config.example.json}"

# ----- Build (optional) ----------------------------------------------------
# Set BUILD=1 to rebuild before running. Default: assume already built.
if [ "${BUILD:-0}" = "1" ]; then
  echo "Building dbsync with cabal.project.profiling..."
  (cd "$PROJECT_DIR" && cabal --project-file=cabal.project.profiling build dbsync)
fi

# ----- RTS configuration ---------------------------------------------------
# Heap profile mode. Override via:
#   HEAP_MODE=-hc   cost-centre profile  (which function allocates retained)
#   HEAP_MODE=-hr   retainer profile     (who holds it alive — slow)
#   HEAP_MODE=-hd   closure description
#   HEAP_MODE=-hy   by type (more granular than -hT)
HEAP_MODE="${HEAP_MODE:--hT}"

# Heap sample interval (seconds). 60s is a sensible default for multi-hour
# runs; 10–30s for short focused runs.
SAMPLE="${SAMPLE:-60}"

# Parallelism and nursery size.
#   CORES=4   (default; mimics the 4-core target from PHASE2.md)
#   NURSERY=64m (default; original cardano-db-sync uses -A16m and is
#                noticeably slower at 4 cores. -A64m gives the GC head-
#                room each capability needs without blowing past PHASE2's
#                memory target — 4 × 64m = 256 MB of nursery total.)
CORES="${CORES:-4}"
NURSERY="${NURSERY:-64m}"

# Optional: hard-cap heap to catch leaks early (process is killed on OOM
# instead of swap-thrashing the machine). Unset by default.
#   MAX_HEAP=12G
MAX_HEAP_OPT=""
if [ -n "${MAX_HEAP:-}" ]; then MAX_HEAP_OPT="-M${MAX_HEAP}"; fi

# Optional: emit eventlog with heap events for eventlog2html. Files are
# large (~100 MB/hour) so off by default.
#   EVENTLOG=1 ./scripts/profile-dbsync.sh
EVENTLOG_OPT=""
if [ "${EVENTLOG:-0}" = "1" ]; then EVENTLOG_OPT="-l-au"; fi

# ----- Locate binary -------------------------------------------------------
dbsync="$(find "$PROJECT_DIR"/dist-newstyle -name dbsync -type f -path '*profiling*' | head -1)"
if [ -z "$dbsync" ]; then
  dbsync="$(find "$PROJECT_DIR"/dist-newstyle -name dbsync -type f | head -1)"
fi
if [ -z "$dbsync" ]; then
  echo "ERROR: dbsync binary not found. Build first:" >&2
  echo "  cabal --project-file=cabal.project.profiling build dbsync" >&2
  echo "  (or set BUILD=1 and re-run this script)"               >&2
  exit 1
fi

# ----- Assemble RTS opts ---------------------------------------------------
#  -p          cost-centre tree    -> dbsync.prof
#  $HEAP_MODE  heap profile mode   -> dbsync.hp
#  -L60        wider label width   (default 25 truncates Module.fn)
#  -i$SAMPLE   heap sample seconds
#  -podbsync   output prefix
#  -sdbsync.gc GC summary on exit  -> dbsync.gc
#  -A$NURSERY  per-capability nursery size
#  -N$CORES    parallel GHC threads
#  -T          enable GHC.Stats
#  -c          compacting old-gen GC: after Gen-1 GC, return megablocks
#              above the live watermark to the OS. This is GHC's only
#              real mechanism for shrinking the reserved heap. Without
#              it, transient peaks at epoch-commit time ratchet the
#              heap up to ~15 GB on mainnet without ever giving it back.
#  -F1.3       grow heap to 1.3x live before GC instead of the default
#              2.0x. Pairs with -c to bound the post-peak ceiling.
#              Trade-off: more frequent Gen-1 GCs, lower productivity
#              (~85% instead of ~91%). Worth it for ~10 GB RAM saved.
#  --disable-delayed-os-memory-return
#              GHC 9.4+ delays the madvise() that returns freed pages
#              to the OS, to avoid madvise thrashing on bursty
#              workloads. We want eager return for low RSS — matches
#              what the original cardano-db-sync does.
#  $MAX_HEAP_OPT  optional -M cap
#  $EVENTLOG_OPT  optional eventlog
RTS_OPTS="-p $HEAP_MODE -L60 -i$SAMPLE -podbsync -sdbsync.gc \
  -A$NURSERY -N$CORES -T -c -F1.3 --disable-delayed-os-memory-return \
  $MAX_HEAP_OPT $EVENTLOG_OPT"

# ----- Report what we're doing ---------------------------------------------
echo "Binary:     $dbsync"
echo "Config:     $CONFIG_JSON"
echo "Pg config:  $PG_CONFIG_JSON"
echo "Heap mode:  $HEAP_MODE  (override via HEAP_MODE=…)"
echo "Cores:      $CORES      (override via CORES=…)"
echo "Nursery:    $NURSERY    (override via NURSERY=…)"
echo "RTS opts:   +RTS $RTS_OPTS -RTS"
echo "Output:     ./dbsync.{prof,hp,gc${EVENTLOG_OPT:+,eventlog}} in $PWD"
echo
echo "Stop with kill <pid> (SIGTERM) to get .prof + .gc."
echo "Ctrl-C (SIGINT) writes only the incremental .hp."
echo

# ----- Run -----------------------------------------------------------------
exec "$dbsync" \
  --config         "$CONFIG_JSON" \
  --pg-config      "$PG_CONFIG_JSON" \
  --node-config    "$TESTNET_DIR/config.json" \
  --socket-path    "$TESTNET_DIR/db/node.socket" \
  --ledger-state-dir "$TESTNET_DIR" \
  +RTS $RTS_OPTS -RTS
