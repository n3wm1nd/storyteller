# Builds story-server (plus the pre-built frontend export it serves) into a
# small runtime image. Build from the repository root with podman:
#
#   git submodule update --init vendor/libgit2      # once
#   podman build -t storyteller .
#   podman run --rm -p 8090:8090 -v storyteller-data:/data \
#     -e ROLE_PROSE_MODEL=claude-opus-4-8 -e ANTHROPIC_API_KEY=... \
#     -e ROLE_AGENT_MODEL=deepseek-v4-pro -e OPENROUTER_API_KEY=... \
#     storyteller
#
# See the ENV block at the bottom for what story-server reads, which of it
# has to be supplied, and why the llama.cpp default endpoint needs
# overriding in a container.
#
# The four Haskell dependencies a developer gets from a sibling ../runix
# checkout are fetched from GitHub at the commits pinned below, so no such
# checkout is needed -- override any of them with --build-arg. libgit2 comes
# from the vendor/libgit2 submodule instead, keeping its commit pinned in
# one place.

########################################################################
# Stage 1 — build
########################################################################
FROM docker.io/library/haskell:9.10.3-bookworm AS build

# cmake/pkg-config/zlib are for the vendored libgit2; git is needed both to
# fetch the dependency sources and by gitlib-effect's own test/runtime path.
RUN apt-get update && apt-get install -y --no-install-recommends \
        cmake pkg-config zlib1g-dev git ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# --- the Hackage index -------------------------------------------------
# Its own layer, and above everything repo-shaped: it is a ~150MB download
# with nothing to do with our sources, so nothing below should be able to
# trigger it. Runs from / deliberately — with a project file already in the
# working directory cabal would try (and fail) to read the project first.
# Cached indefinitely as a result; `podman build --no-cache`, or an edit
# above this line, is what refreshes it, which also keeps successive builds
# solving against the same index snapshot.
RUN cabal update

WORKDIR /src

# --- vendored libgit2 source -------------------------------------------
# Nothing here builds it: gitlib-effect is build-type Custom, and its
# Setup.hs runs cbits/build-libgit2.sh when the library is configured. This
# only has to make the submodule's source available, in a layer of its own
# so the source COPYs below can't invalidate it.
#
# The submodule commit stays the single pin (flake.nix aside), which is why
# this is a COPY and not a clone -- so `git submodule update --init
# vendor/libgit2` is a prerequisite of building the image.
COPY vendor/libgit2 ./vendor/libgit2

# --- unpublished Haskell dependencies ---------------------------------
# Four packages a developer gets from a sibling ../runix checkout (see
# cabal.project.local) are not on Hackage. Three are standalone public
# GitHub repos, declared below as source-repository-package stanzas pinned
# by commit, which cabal fetches and builds like any other dependency.
#
# sse-parser is the exception: it is a subdirectory of runix-project, whose
# own submodules are registered with git@github.com: URLs. cabal always
# clones with --recurse-submodules, so pointing a stanza at it makes the
# fetch die on `cannot run ssh` -- and those submodules (runix, universal-llm,
# runix-tools, runix-code) are irrelevant to the one directory we want
# anyway. Hence a sparse, submodule-free checkout of just libs/sse-parser,
# added as a plain local package.
ARG RUNIX_REF=9f58648336600c9ada82ec4a324acfeba8af9147
ARG UNIVERSAL_LLM_REF=e159b2d1b52a429fd9eda7c4cafb7bb4480efc13
ARG RUNIX_TOOLS_REF=06fff697eb5e8818c3626594599beaa4e6d93fe7
ARG RUNIX_PROJECT_REF=64b836822144e4d8225ed5a8141d63bc9cfa7aa3

RUN git clone --filter=blob:none --no-checkout --no-recurse-submodules \
        https://github.com/n3wm1nd/runix-project.git /deps/runix-project \
    && git -C /deps/runix-project sparse-checkout set libs/sse-parser \
    && git -C /deps/runix-project checkout --detach ${RUNIX_PROJECT_REF}

# (.dockerignore keeps the host's cabal.project.local, whose paths point
# into ~/git/runix, out of the build context -- this one replaces it.)
RUN printf '%s\n' \
      'packages: /deps/runix-project/libs/sse-parser'              \
      ''                                                           \
      'source-repository-package'                                  \
      '  type: git'                                                \
      '  location: https://github.com/n3wm1nd/runix.git'           \
      "  tag: ${RUNIX_REF}"                                        \
      ''                                                           \
      'source-repository-package'                                  \
      '  type: git'                                                \
      '  location: https://github.com/n3wm1nd/universal-llm.git'   \
      "  tag: ${UNIVERSAL_LLM_REF}"                                \
      ''                                                           \
      'source-repository-package'                                  \
      '  type: git'                                                \
      '  location: https://github.com/n3wm1nd/runix-tools.git'     \
      "  tag: ${RUNIX_TOOLS_REF}"                                  \
      > cabal.project.local

# --- layer 1: the four unpublished packages and their closure ----------
# Driven by nothing but the package *descriptions* -- no module source from
# this repo at all -- so the bulk of the third-party build (polysemy, aeson,
# the http/tls stack, ...) survives every edit to storyteller or
# gitlib-effect. Naming the four as build targets rather than using
# --only-dependencies is what makes that possible: --only-dependencies for
# exe:story-server would drag in gitlib-effect, a *local* dependency, whose
# library cannot be built from its .cabal file alone.
#
# The solve happens against the real project (both local .cabal files are
# present), so the versions chosen here are the ones the later layers want
# too, and their build products are found rather than rebuilt.
COPY cabal.project storyteller.cabal ./
COPY gitlib-effect/gitlib-effect.cabal gitlib-effect/Setup.hs ./gitlib-effect/
# Cabal wants declared source/data locations to exist when it configures.
RUN mkdir -p src app bench test/fixtures && touch test/fixtures/minimal.yaml
RUN cabal build runix universal-llm runix-tools sse-parser

# --- layer 2: gitlib-effect and the rest of storyteller's deps ---------
# Everything left over (warp, wai, websockets, ...) plus gitlib-effect
# itself, which needs its real sources -- and which builds the vendored
# libgit2 on the way, via its Setup.hs. Invalidated by a gitlib-effect edit,
# but layer 1's products are still on disk when that happens.
#
# The target is the storyteller *package*, not exe:story-server: an
# executable depends on its own package's library, so asking for the exe's
# dependencies would include lib:storyteller and try to compile it from a
# src/ that isn't here yet.
COPY gitlib-effect ./gitlib-effect
RUN cabal build --only-dependencies storyteller

# --- layer 3: storyteller itself ---------------------------------------
# cabal install puts the binary at a path we choose, so nothing here has to
# guess at, or parse out, a location inside dist-newstyle. It gets there by
# sdisting each local package into the store and building it from the
# tarball, so it only works because gitlib-effect is a self-contained
# package: cbits/build-libgit2.sh fetches libgit2 itself when there is no
# vendor/libgit2 submodule to find (the situation inside an sdist), and the
# static archive is installed into the package's own libdir as a bundled
# library, so consumers still resolve -lgit2 after the scratch build tree is
# gone. See gitlib-effect.cabal's extra-bundled-libraries and its Setup.hs.
#
# Note this builds every executable in the package, not just story-server:
# a store install builds the whole package rather than one component. Only
# story-server is carried into the runtime image.
COPY . .
RUN cabal install exe:story-server --installdir=/out --install-method=copy \
    && strip /out/story-server

########################################################################
# Stage 2 — runtime
########################################################################
FROM docker.io/library/debian:bookworm-slim

# git: gitlib-effect shells out to it to init/inspect repositories (libgit2
# is linked in statically, so it needs nothing here).
# libgmp10/zlib1g/libffi8: GHC's runtime shared-library dependencies.
RUN apt-get update && apt-get install -y --no-install-recommends \
        git libgmp10 zlib1g libffi8 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/story-server /usr/local/bin/story-server
# Pre-built static export committed at frontend/out — served by story-server
# itself via STATIC_DIR, so the whole deployment is this one process.
COPY frontend/out /srv/frontend

RUN useradd --system --create-home --uid 10001 storyteller \
    && mkdir -p /data && chown storyteller:storyteller /data
USER storyteller

# Defaults for the three settings that describe the image's own layout. Only
# these are baked in; everything else story-server reads is deployment
# policy and belongs on the run command.
#
#   STORY_REPO   the story repository, created (bare) on first use if
#                absent. story-server *requires* it, hence a default -- but
#                it is the one piece of durable state here, so bind it to a
#                volume or host directory (the /data volume below) rather
#                than leaving it in the container's writable layer.
#   STATIC_DIR   the frontend export copied in above.
#   PORT         matches EXPOSE below.
#
# Run-time, none baked in:
#
#   ROLE_PROSE_MODEL / ROLE_AGENT_MODEL
#                which model backs each of the two LLM roles; both default
#                to qwen35-40b. Unknown names fail at startup listing the
#                valid ones, and the two tables differ -- the agent role
#                needs structured output, so the Claude entries are
#                prose-only and fail with that as the reason (see
#                Storyteller.Core.LLM.Registry's knownModels /
#                knownAgentModels).
#   LLAMACPP_ENDPOINT
#                needed by llama.cpp-routed models, which includes both
#                defaults above. Its own default, http://localhost:8080/v1,
#                resolves to *this container* and so is essentially never
#                right here: point it at the real host, e.g.
#                http://host.containers.internal:8080/v1, or run with
#                --network=host.
#   OPENROUTER_API_KEY / ANTHROPIC_API_KEY
#                required only when the selected model routes via that
#                backend, and then startup fails without it.
#   LLM_LOG_REQUESTS
#                set to anything non-empty to log LLM requests.
ENV STORY_REPO=/data/story \
    STATIC_DIR=/srv/frontend \
    PORT=8090
VOLUME ["/data"]
EXPOSE 8090

ENTRYPOINT ["story-server"]
