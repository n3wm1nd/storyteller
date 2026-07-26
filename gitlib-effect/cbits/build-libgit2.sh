#!/usr/bin/env bash
# Builds libgit2 as a static library with no network transports (no SSH, no
# HTTPS -- this codebase only ever touches local repositories) and installs
# it under gitlib-effect/cbits/build/install, so Cabal has a stable
# lib/+include/ layout to point extra-lib-dirs/include-dirs at regardless of
# libgit2's own internal build-tree structure.
#
# Source: the superproject's vendor/libgit2 submodule when it is checked out
# -- the normal case in a development tree, and the pin a developer bumps.
# Otherwise this script fetches libgit2 itself, at $libgit2_commit below.
# That fallback is what makes gitlib-effect installable as a *package*
# rather than only as part of this repository: an sdist cannot carry
# vendor/libgit2 (it sits outside the package directory), so anything
# building from a tarball -- `cabal install`, which sdists local packages
# into the store, included -- has no submodule to find. The fetch lands
# inside cbits/build, i.e. within the package, so it works wherever the
# unpacked tarball happens to be.
#
# $libgit2_commit and the submodule therefore both pin libgit2, and must be
# bumped together; when both are available the script says so rather than
# letting them drift silently.
#
# Idempotent: skips the build entirely if the installed static archive
# already exists. Delete gitlib-effect/cbits/build to force a rebuild
# (e.g. after bumping the pin).
set -euo pipefail

libgit2_commit=f7164261c9bc0a7e0ebf767c584e5192810a8b24

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"

build_dir="$script_dir/build"
install_dir="$build_dir/install"
submodule_src="$repo_root/vendor/libgit2"

if [ -f "$install_dir/lib/libgit2.a" ]; then
  exit 0
fi

if [ -f "$submodule_src/CMakeLists.txt" ]; then
  src_dir="$submodule_src"
  # Tolerate there being no git repository above us at all (an exported or
  # copied tree, e.g. a container build context): then there is simply no
  # submodule pin to compare against.
  pinned="$(git -C "$repo_root" ls-tree HEAD vendor/libgit2 2>/dev/null | awk '{print $3}' || true)"
  if [ -n "$pinned" ] && [ "$pinned" != "$libgit2_commit" ]; then
    echo "build-libgit2.sh: warning: vendor/libgit2 is pinned at $pinned but" >&2
    echo "  \$libgit2_commit in this script says $libgit2_commit -- building the" >&2
    echo "  submodule's version. Bump both together." >&2
  fi
else
  src_dir="$build_dir/libgit2-src"
  if [ ! -f "$src_dir/CMakeLists.txt" ]; then
    echo "build-libgit2.sh: no vendor/libgit2 checkout -- fetching libgit2 $libgit2_commit" >&2
    rm -rf "$src_dir"
    git clone --filter=blob:none --no-checkout \
      https://github.com/libgit2/libgit2.git "$src_dir"
    git -C "$src_dir" checkout --detach "$libgit2_commit"
  fi
fi

cmake -S "$src_dir" -B "$build_dir" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$install_dir" \
  -DCMAKE_INSTALL_LIBDIR=lib \
  -DBUILD_SHARED_LIBS=OFF \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
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
