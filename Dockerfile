# syntax=docker/dockerfile:1

# ---------------------------------------------------------------------------
# Builder. ubuntu:22.04 pins the glibc floor (2.35): the binary built here is
# also the linux release tarball asset, so the base must stay the oldest
# supported distro, not the newest.
# ---------------------------------------------------------------------------
FROM ubuntu:22.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8
RUN apt-get update && apt-get install -y --no-install-recommends \
      autoconf automake build-essential ca-certificates curl git jq \
      libffi-dev libgmp-dev liblmdb-dev libncurses-dev libnuma-dev \
      libpq-dev libsnappy-dev libtinfo-dev libtool liburing-dev \
      pkg-config unzip zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# protoc from the official release: jammy's 3.12 predates proto3 optional,
# which the proto-lens packages cardano-node pulls in require.
ARG PROTOC_VERSION=25.1
RUN arch="$(uname -m)" \
    && case "$arch" in x86_64) parch=x86_64 ;; aarch64) parch=aarch_64 ;; *) echo "unsupported arch $arch" >&2; exit 1 ;; esac \
    && curl -sfL --retry 10 --retry-delay 6 "https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-${parch}.zip" -o /tmp/protoc.zip \
    && unzip -q /tmp/protoc.zip -d /usr/local bin/protoc 'include/*' \
    && rm /tmp/protoc.zip

# GHC + cabal bindists straight from downloads.haskell.org — ghcup's metadata
# lives on raw.githubusercontent.com, which 429s the shared runner IPs. URLs
# and sha256s are the ones ghcup's own metadata pins for this base per arch;
# re-read them from ghcup-0.1.0.yaml when bumping either version.
ARG GHC_VERSION=9.14.1
ARG CABAL_VERSION=3.16.1.0
RUN set -eu; arch="$(uname -m)"; \
    case "$arch" in \
      x86_64) \
        ghc_url="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-ubuntu22_04-linux.tar.xz"; \
        ghc_sha=29410b9856dcb47fe5038e69478fbcf96137166ced8a789e566440747c2b9393; \
        cabal_url="https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-x86_64-linux-glibc.tar.xz"; \
        cabal_sha=dbb9e9964a918602924cf9f3aa6e21962c449bfce9f7a1c00504b5d3787af41a ;; \
      aarch64) \
        ghc_url="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-aarch64-deb10-linux.tar.xz"; \
        ghc_sha=526c352cceddbf6c580e17ade7e782e3b21b4182d328b2d454c9f13ca7c08992; \
        cabal_url="https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-aarch64-linux-deb10.tar.xz"; \
        cabal_sha=f90264ff9503f638ada33353c0b39dd99d30f7849c5fa373d1abcbf0bc01945e ;; \
      *) echo "unsupported arch $arch" >&2; exit 1 ;; \
    esac; \
    curl -sfL --retry 10 --retry-delay 6 "$ghc_url" -o /tmp/ghc.tar.xz; \
    echo "$ghc_sha  /tmp/ghc.tar.xz" | sha256sum -c -; \
    tar -xJf /tmp/ghc.tar.xz -C /tmp; \
    ( cd "/tmp/ghc-${GHC_VERSION}-${arch}-unknown-linux" \
      && ./configure --prefix=/usr/local && make install ); \
    rm -rf /tmp/ghc.tar.xz "/tmp/ghc-${GHC_VERSION}-${arch}-unknown-linux"; \
    curl -sfL --retry 10 --retry-delay 6 "$cabal_url" -o /tmp/cabal.tar.xz; \
    echo "$cabal_sha  /tmp/cabal.tar.xz" | sha256sum -c -; \
    mkdir /tmp/cabal; tar -xJf /tmp/cabal.tar.xz -C /tmp/cabal; \
    install -m 755 /tmp/cabal/cabal /usr/local/bin/cabal; \
    rm -rf /tmp/cabal.tar.xz /tmp/cabal; \
    ghc --version; cabal --version

# IOG C libs (libsodium VRF fork, secp256k1, blst), static-only, revisions
# pinned to cardano-node's own flake.lock — see scripts/release/.
COPY scripts/release/iog-lib-pins.env scripts/release/build-iog-libs.sh /tmp/iog/
RUN /tmp/iog/build-iog-libs.sh

WORKDIR /src

# Dependency layer: only the build-plan inputs, so this expensive layer is
# cached until cabal.project or a .cabal file changes. BuildKit's registry
# cache backend caches layers, not RUN mounts — this split is the cache.
COPY cabal.project ./
COPY dbsync/dbsync.cabal dbsync/
COPY dbsync-db/dbsync-db.cabal dbsync-db/
COPY dbsync-smash/dbsync-smash.cabal dbsync-smash/
COPY tests/dbsync-tests.cabal tests/
COPY tests/dbsync-mock/dbsync-mock.cabal tests/dbsync-mock/
RUN cabal update
RUN cabal build dbsync:exe:dbsync --only-dependencies

COPY . .
RUN cabal build dbsync:exe:dbsync \
    && mkdir -p /out \
    && cp "$(cabal list-bin dbsync:exe:dbsync)" /out/dbsync \
    && strip /out/dbsync

# ---------------------------------------------------------------------------
# Artifact. `docker buildx build --target artifact --output type=local` pulls
# the bare binary out for the release tarballs.
# ---------------------------------------------------------------------------
FROM scratch AS artifact
COPY --from=builder /out/dbsync /dbsync

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
# and the dev tools (gen-migration, smash) are not shipped.
# ---------------------------------------------------------------------------
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates libffi8 libgmp10 liblmdb0 libnuma1 libpq5 \
      libsnappy1v5 libstdc++6 libtinfo6 liburing2 zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --create-home --uid 10001 dbsync \
    && install -d -o dbsync -g dbsync /ipc /var/lib/dbsync

COPY --from=builder /out/dbsync /usr/local/bin/dbsync
COPY --from=configs /opt/cardano /opt/cardano
COPY config-examples/ /opt/dbsync/profiles/
RUN rm /opt/dbsync/profiles/pg-config.example.json
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh

USER dbsync
ENV SOCKET_PATH=/ipc/node.socket \
    LEDGER_STATE_DIR=/var/lib/dbsync
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
