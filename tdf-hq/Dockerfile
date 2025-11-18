# -------- Stage 1: build with Stack (no GHCR) ----------
FROM haskell:9.6-bullseye AS builder
WORKDIR /app

# OS deps for building persistent-postgresql + basic tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config git curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Show toolchain versions (handy in Render logs)
RUN ghc --version || true
RUN stack --version || true

# Copy project definition first for better layer caching
COPY stack.yaml tdf-hq.cabal ./
# COPY package.yaml ./   # uncomment if you have it

# Make sure Stack installs the correct compiler for the resolver
# Also, be explicit and avoid any global "system-ghc" config
RUN stack --no-terminal --install-ghc setup && \
    stack --no-terminal --install-ghc build --only-dependencies

# Copy sources (git metadata is provided via build args when available)
COPY app app
COPY src src
COPY config config
COPY docs docs


# Build and export the binary
RUN stack --no-terminal --install-ghc build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
WORKDIR /app

# Runtime libs for Postgres + CA certs for Neon TLS
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ARG SOURCE_COMMIT=UNKNOWN
ENV GIT_SHA=${SOURCE_COMMIT}
COPY --from=builder /out/tdf-hq-exe /app/tdf-hq-exe
COPY --from=builder /app/docs /app/docs

# Render provides PORT; map it to the app's APP_PORT
ENV APP_PORT=8080
CMD ["/bin/sh","-c","APP_PORT=${PORT:-8080} /app/tdf-hq-exe"]
