#!/bin/sh
set -eu
records_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
records_build_dir="$records_root/.tmp/records-runtime-probe"
mkdir -p "$records_build_dir"
cd "$records_root/tdf-hq"
stack exec -- ghc --make -O0 -threaded -isrc test/RecordsRuntimeProbe.hs \
  -outputdir "$records_build_dir" -o "$records_build_dir/records-runtime-probe"
cd "$records_root"
TDF_RECORDS_RUNTIME_PROBE="$records_build_dir/records-runtime-probe" \
  ./scripts/test-records-youtube-catalog-migration.sh
