#!/bin/sh
# Read the actual canonical tables through the compiled application helper.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_dataweb_cart_test
# Use the four unchanged authoritative CREATE TABLE definitions. This verifies
# the reader boundary, not the complete migration manifest or payment lifecycle.
checkout_schema=$(python3 - "$repo_root" <<'PY'
from pathlib import Path
import re
import sys
source = (Path(sys.argv[1])/'tdf-hq/sql/2026-08-13_unified_checkout_core.sql').read_text()
for table in ['commerce_quote', 'commerce_quote_line', 'commerce_checkout_session',
              'commerce_checkout_line_item']:
    matches = re.findall(r'CREATE TABLE IF NOT EXISTS ' + table + r' \(.*?^\);',
                         source, re.M | re.S)
    if len(matches) != 1:
        raise SystemExit('Canonical checkout table definition changed: ' + table)
    print(matches[0])
PY
)
printf '%s\n' "$checkout_schema" | tdf_test_psql >/dev/null
export TDF_DATAFAST_CART_TEST_DB="$TDF_TEST_DATABASE_URL"
cd "$repo_root/tdf-hq"
stack --no-terminal exec -- runghc -package=tdf-hq test/integration/DatafastCart.hs \
  --seed=20261110 --fail-on=empty
