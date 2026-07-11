#!/usr/bin/env bash
# Builds vendor/libgit2 as a static library with no network transports
# (no SSH, no HTTPS -- this codebase only ever touches local repositories)
# and installs it under gitlib-effect/cbits/build/install, so Cabal has a
# stable lib/+include/ layout to point extra-lib-dirs/include-dirs at
# regardless of libgit2's own internal build-tree structure.
#
# Idempotent: skips the build entirely if the installed static archive
# already exists. Delete gitlib-effect/cbits/build to force a rebuild
# (e.g. after bumping the vendor/libgit2 submodule).
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"

src_dir="$repo_root/vendor/libgit2"
build_dir="$script_dir/build"
install_dir="$build_dir/install"

if [ -f "$install_dir/lib/libgit2.a" ]; then
  exit 0
fi

if [ ! -f "$src_dir/CMakeLists.txt" ]; then
  echo "build-libgit2.sh: $src_dir is not populated -- run 'git submodule update --init vendor/libgit2'" >&2
  exit 1
fi

cmake -S "$src_dir" -B "$build_dir" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$install_dir" \
  -DCMAKE_INSTALL_LIBDIR=lib \
  -DBUILD_SHARED_LIBS=OFF \
  -DUSE_SSH=OFF \
  -DUSE_HTTPS=OFF \
  -DREGEX_BACKEND=builtin \
  -DUSE_BUNDLED_ZLIB=OFF \
  -DUSE_THREADS=ON \
  -DBUILD_TESTS=OFF \
  -DBUILD_CLI=OFF \
  -DBUILD_EXAMPLES=OFF \
  -DBUILD_FUZZERS=OFF

cmake --build "$build_dir" --target libgit2package --parallel
cmake --install "$build_dir"
