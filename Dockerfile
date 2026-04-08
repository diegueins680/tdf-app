# syntax=docker/dockerfile:1

FROM haskell:9.8 as builder

WORKDIR /srv/tdf

# Copy git metadata separately so Development.GitRev.gitHash
# can resolve the commit during stack build. Without this, stack
# build fails when Version.hs calls $(gitHash) at compile time.
COPY .git .git

# Copy the rest of the application source
COPY . .

RUN stack setup --system-ghc \
 && stack build --copy-bins --local-bin-path /opt/tdf/bin

FROM debian:bookworm-slim as runtime

WORKDIR /opt/tdf

COPY --from=builder /opt/tdf/bin/ /usr/local/bin/

CMD ["tdf"]
