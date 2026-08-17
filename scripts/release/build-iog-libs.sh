#!/usr/bin/env bash
# Builds the IOG C libraries (libsodium VRF fork, secp256k1, blst) from source
# at the revisions vendored in iog-lib-pins.env, static-only, so the linker
# folds them into the dbsync binary.
#
# One recipe for every release artifact: the Dockerfile, both linux tarball
# builds, and the macOS release job all run this script.
#
# Env:
#   IOG_LIBS_PREFIX  install prefix (default /usr/local; must be writable)
set -euo pipefail

script_dir=$(cd "$(dirname "$0")" && pwd)
# shellcheck source=iog-lib-pins.env
. "$script_dir/iog-lib-pins.env"

prefix=${IOG_LIBS_PREFIX:-/usr/local}
jobs=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

fetch() { # owner/repo, rev, dest — full clone: blst needs tags for its version
  git -c advice.detachedHead=false clone --quiet --no-checkout \
    "https://github.com/$1" "$3"
  git -C "$3" checkout --quiet "$2"
}

# libsodium (VRF fork)
fetch "$SODIUM_REPO" "$SODIUM_REV" "$workdir/sodium"
(
  cd "$workdir/sodium"
  # Skip autogen's config.guess/config.sub refresh: savannah's gitweb serves
  # an anti-bot HTML page to curl, which then shadows the good scripts
  # autoreconf just installed. Those cover every target we build.
  DO_NOT_UPDATE_CONFIG_SCRIPTS=1 ./autogen.sh
  ./configure --prefix="$prefix" --disable-shared --enable-static --with-pic
  make -j"$jobs"
  make install
)

# secp256k1 — schnorrsig is what cardano-crypto-class binds against
fetch "$SECP256K1_REPO" "$SECP256K1_REV" "$workdir/secp256k1"
(
  cd "$workdir/secp256k1"
  ./autogen.sh
  ./configure --prefix="$prefix" --disable-shared --enable-static --with-pic \
    --enable-module-schnorrsig --enable-module-recovery
  make -j"$jobs"
  make install
)

# blst — no autotools; install the static lib, headers and a pc file by hand
fetch "$BLST_REPO" "$BLST_REV" "$workdir/blst"
(
  cd "$workdir/blst"
  ./build.sh -fPIC
  version=$(git describe --tags | sed 's/^v//')
  install -d "$prefix/lib/pkgconfig" "$prefix/include"
  install -m 644 libblst.a "$prefix/lib/"
  install -m 644 bindings/blst.h bindings/blst_aux.h "$prefix/include/"
  cat >"$prefix/lib/pkgconfig/libblst.pc" <<EOF
prefix=$prefix
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: $version
Libs: -L\${libdir} -lblst
Cflags: -I\${includedir}
EOF
)

echo "IOG libs installed to $prefix:"
echo "  libsodium  $SODIUM_REV"
echo "  secp256k1  $SECP256K1_REV"
echo "  blst       $BLST_REV"
