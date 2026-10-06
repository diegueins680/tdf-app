#!/bin/sh
set -eu
cd /private/tmp/tdf-branch-audit-20260928/ingestion/tdf-hq
test_dist=$(stack path --dist-dir)
stack exec -- ghc -O0 -Wall -threaded -isrc -itest -i"$test_dist/build/autogen" -outputdir .stack-work/event-raci-browser /private/tmp/tdf-branch-audit-20260928/DiscoveryAuditMain.hs -o .stack-work/event-raci-browser/discovery-audit
.stack-work/event-raci-browser/discovery-audit
