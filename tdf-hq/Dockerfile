# -------- Stage 1: build with Stack (no GHCR) ----------
ARG SOURCE_COMMIT=dev
ARG BUILD_TIME=unknown

# Use the GHC version required by the resolver to avoid Stack downloading a full toolchain.
FROM haskell:9.6-bullseye AS builder
ARG SOURCE_COMMIT
ARG BUILD_TIME
WORKDIR /app

# OS deps for building persistent-postgresql + basic tools
# Some CI networks inject MITM proxies that break apt signature checks; allow insecure
# repositories only during image build (no secrets involved).
RUN apt-get -o Acquire::AllowInsecureRepositories=true -o Acquire::Check-Valid-Until=false update && \
    apt-get -o Acquire::AllowInsecureRepositories=true --allow-unauthenticated install -y --no-install-recommends \
    libpq-dev pkg-config git curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV SOURCE_COMMIT=${SOURCE_COMMIT}
ENV GIT_SHA=${SOURCE_COMMIT}
ENV BUILD_TIME=${BUILD_TIME}
ENV STACK_ROOT=/root/.stack
ENV STACK_SETUP_PATH=/tmp/stack-setup-2.yaml
# Prefer raw.githubusercontent.com to avoid GitHub API 403 rate limits; fall back to the API if needed.
ENV STACK_SETUP_URL_PRIMARY=https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml
ENV STACK_SETUP_URL_FALLBACK=https://api.github.com/repos/commercialhaskell/stackage-content/contents/stack/stack-setup-2.yaml?ref=master

# Pre-fetch stack setup info with simple retries to avoid GitHub rate limits during stack setup
RUN set -e; \
    for i in $(seq 1 8); do \
      if curl -fsSL "$STACK_SETUP_URL_PRIMARY" -o "$STACK_SETUP_PATH"; then break; fi; \
      if curl -fsSL -H "Accept: application/vnd.github.v3.raw" "$STACK_SETUP_URL_FALLBACK" -o "$STACK_SETUP_PATH"; then break; fi; \
      echo "Retrying stack-setup-2.yaml download ($i/8)..." >&2; \
      sleep 10; \
    done; \
    test -s "$STACK_SETUP_PATH"

# Show toolchain versions (handy in Render logs)
RUN ghc --version || true
RUN stack --version || true

# Copy project definition first for better layer caching (context: repo root)
COPY tdf-hq/stack.yaml stack.yaml
COPY tdf-hq/stack-snapshot.yaml stack-snapshot.yaml
COPY tdf-hq/tdf-hq.cabal tdf-hq.cabal
# COPY tdf-hq/package.yaml ./   # uncomment if you have it

# Make sure Stack installs the correct compiler for the resolver
# Also, be explicit and avoid any global "system-ghc" config
RUN set -e; \
    retry() { \
      for i in 1 2 3; do \
        "$@" && return 0; \
        echo "Retrying ($i/3): $*" >&2; \
        sleep 10; \
      done; \
      return 1; \
    }; \
    # Update index with a clean pantry if hash verification fails (Hackage mirror flakiness)
    if ! retry stack --no-terminal --setup-info-yaml "$STACK_SETUP_PATH" --install-ghc update; then \
      echo "Stack update failed, clearing pantry cache and retrying..." >&2; \
      rm -rf /root/.stack/pantry/hackage; \
      retry stack --no-terminal --setup-info-yaml "$STACK_SETUP_PATH" --install-ghc update; \
    fi; \
    retry stack --no-terminal --setup-info-yaml "$STACK_SETUP_PATH" --install-ghc setup; \
    retry stack --no-terminal --setup-info-yaml "$STACK_SETUP_PATH" --install-ghc build --only-dependencies

# Copy sources (git metadata is provided via build args when available)
COPY tdf-hq/app app
COPY tdf-hq/src src
COPY tdf-hq/config/default.env.example config/default.env
COPY tdf-hq/docs docs

# Capture commit and build time inside the image for the /version endpoint.
# Prefer the actual repo metadata when it's available; otherwise, fall back to
# the provided build args (which default to dev/unknown).
RUN set -eu; \
    COMMIT_VAL="${SOURCE_COMMIT:-}"; \
    if [ -z "$COMMIT_VAL" ] || [ "$COMMIT_VAL" = "dev" ] || [ "$COMMIT_VAL" = "unknown" ]; then \
      if git rev-parse --verify HEAD >/dev/null 2>&1; then \
        COMMIT_VAL="$(git rev-parse --short=12 HEAD)"; \
      fi; \
    fi; \
    if [ -z "$COMMIT_VAL" ]; then COMMIT_VAL="dev"; fi; \
    echo "$COMMIT_VAL" > /app/COMMIT; \
    BUILD_TS="${BUILD_TIME:-}"; \
    if [ -z "$BUILD_TS" ] || [ "$BUILD_TS" = "unknown" ]; then \
      BUILD_TS="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"; \
    fi; \
    echo "$BUILD_TS" > /app/BUILD_TIME

# Build and export the binary
RUN stack --no-terminal --setup-info-yaml "$STACK_SETUP_PATH" --install-ghc build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
ARG SOURCE_COMMIT=dev
ARG BUILD_TIME=unknown
WORKDIR /app

# Runtime libs for Postgres + CA certs for Neon TLS
RUN apt-get -o Acquire::AllowInsecureRepositories=true -o Acquire::Check-Valid-Until=false update && \
    apt-get -o Acquire::AllowInsecureRepositories=true --allow-unauthenticated install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ARG SOURCE_COMMIT=UNKNOWN
ENV SOURCE_COMMIT=${SOURCE_COMMIT}
ENV GIT_SHA=${SOURCE_COMMIT}
ENV BUILD_TIME=${BUILD_TIME}
COPY --from=builder /out/tdf-hq-exe /app/tdf-hq-exe
COPY --from=builder /app/docs /app/docs
COPY --from=builder /app/COMMIT /app/COMMIT
COPY --from=builder /app/BUILD_TIME /app/BUILD_TIME

# Render provides PORT; map it to the app's APP_PORT
ENV APP_PORT=8080
CMD ["/bin/sh","-c","APP_PORT=${PORT:-8080} /app/tdf-hq-exe"]
