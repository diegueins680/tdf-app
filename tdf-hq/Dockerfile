# -------- Stage 1: build with Stack (no GHCR) ----------
ARG SOURCE_COMMIT=dev
ARG BUILD_TIME=unknown

FROM haskell:9.6-bullseye AS builder
ARG SOURCE_COMMIT
ARG BUILD_TIME
WORKDIR /app

# OS deps for building persistent-postgresql + basic tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config git curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV SOURCE_COMMIT=${SOURCE_COMMIT}
ENV GIT_SHA=${SOURCE_COMMIT}
ENV BUILD_TIME=${BUILD_TIME}
ENV STACK_ROOT=/root/.stack

# Show toolchain versions (handy in Render logs)
RUN ghc --version || true
RUN stack --version || true

# Copy project definition first for better layer caching
COPY stack.yaml stack.yaml
COPY tdf-hq.cabal tdf-hq.cabal
# COPY tdf-hq/package.yaml ./   # uncomment if you have it

# Make sure Stack installs the correct compiler for the resolver
# Also, be explicit and avoid any global "system-ghc" config
RUN stack --no-terminal --install-ghc setup && \
    stack --no-terminal --install-ghc build --only-dependencies

# Copy sources (git metadata is provided via build args when available)
COPY app app
COPY src src
COPY config/default.env.example config/default.env
COPY docs docs

# Capture commit and build time inside the image for version endpoint
# In build contexts without .git (e.g., Fly remote builds), fall back to SOURCE_COMMIT/BUILD_TIME args.
RUN echo "${SOURCE_COMMIT:-dev}" > /app/COMMIT && \
    echo "${BUILD_TIME:-unknown}" > /app/BUILD_TIME

# Refresh stack indices and clear any stale pantry cache to avoid hash issues
RUN rm -rf /root/.stack/pantry/hackage && \
    stack --no-terminal update

# Build and export the binary
RUN stack --no-terminal --install-ghc build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
ARG SOURCE_COMMIT=dev
ARG BUILD_TIME=unknown
WORKDIR /app

# Runtime libs for Postgres + CA certs for Neon TLS
RUN apt-get update && apt-get install -y --no-install-recommends \
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
