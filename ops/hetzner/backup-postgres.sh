#!/bin/sh
set -eu
umask 077

# Install the two reviewed Python companions together in this root-owned path.
# Fail if private storage has not already been provisioned; never chmod an
# unexpected existing directory or follow an operator-provided output path.
exec /usr/bin/env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 \
  /usr/bin/python3 /opt/tdf/production/backup-postgres.py
