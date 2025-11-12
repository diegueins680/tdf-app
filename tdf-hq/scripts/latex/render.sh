#!/usr/bin/env bash
set -euo pipefail
TEXDIR=$1
OUT=$2
mkdir -p $(dirname "$OUT")
tectonic --outdir $(dirname "$OUT") $(find "$TEXDIR" -maxdepth 1 -name '*.tex' -print | sort | head -n1)
