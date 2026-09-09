# syntax=docker/dockerfile:1

# Assembles the runtime image from a prebuilt static binary — no compiler
# here. release.yml builds the binaries with nix (`nix build .#dbsync-static`,
# see flake.nix) and stages them under docker/bin/<TARGETARCH>/dbsync before
# running buildx. To build locally, stage a binary the same way:
#
#   nix build .#dbsync-static
#   mkdir -p docker/bin/amd64 && cp result/bin/dbsync docker/bin/amd64/
#   docker build .

# ---------------------------------------------------------------------------
# Network config bundles. Genesis files resolve relative to config.json, so
# each baked network directory is self-contained.
# ---------------------------------------------------------------------------
FROM ubuntu:22.04 AS configs
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
RUN set -eu; \
    for net in mainnet preprod preview; do \
      mkdir -p "/opt/cardano/$net"; \
      for f in config.json byron-genesis.json shelley-genesis.json \
               alonzo-genesis.json conway-genesis.json; do \
        curl -sfL --retry 10 --retry-delay 6 \
          "https://book.play.dev.cardano.org/environments/$net/$f" \
          -o "/opt/cardano/$net/$f"; \
      done; \
    done

# ---------------------------------------------------------------------------
# Runtime. The dbsync executable only — the node runs in its own container,
# and the dev tools (gen-migration, smash) are not shipped. The binary is
# fully static (musl), so ca-certificates (TLS for off-chain pool metadata
# fetches) is the only runtime package.
# ---------------------------------------------------------------------------
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --create-home --uid 10001 dbsync \
    && install -d -o dbsync -g dbsync /ipc /var/lib/dbsync

ARG TARGETARCH
COPY docker/bin/${TARGETARCH}/dbsync /usr/local/bin/dbsync
COPY --from=configs /opt/cardano /opt/cardano
COPY config-examples/ /opt/dbsync/profiles/
RUN rm /opt/dbsync/profiles/pg-config.example.json
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh

USER dbsync
ENV LANG=C.UTF-8 \
    SOCKET_PATH=/ipc/node.socket \
    LEDGER_STATE_DIR=/var/lib/dbsync
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
