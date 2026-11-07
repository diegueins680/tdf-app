#!/usr/bin/env python3
"""Emit finite PostgreSQL probes of the actual recovery time CHECK (not a proof)."""
import importlib.util
from pathlib import Path
import sys

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('recovery', ROOT/'scripts/verify-recovery-expiry.py')
recovery = importlib.util.module_from_spec(spec)
spec.loader.exec_module(recovery)


def sql():
    _, _, check = recovery.expressions((ROOT/recovery.SOURCE).read_text(),
                                       (ROOT/recovery.MIGRATION).read_text())
    limit = 2**63 - 1
    boundaries = (-2**63, -900, -1, 0, 1, 899, 900, 901,
                  limit - 901, limit - 900, limit - 899, limit)
    pairs = {(i, e) for i in boundaries for e in boundaries}
    pairs.update((i, (i + 900 + 2**63) % 2**64 - 2**63) for i in boundaries)
    rows = [(i, e, i >= 0 and e - i == 900) for i, e in sorted(pairs)]
    rows.extend([(None, 900, False), (0, None, False), (None, None, False)])
    def literal(value):
        return 'NULL::bigint' if value is None else f"'{value}'::bigint"
    values = ',\n'.join(f'({literal(i)}, {literal(e)}, {str(expected).lower()})'
                        for i, e, expected in rows)
    return f"""-- Generated from the repository CHECK, isolated temporary table only.
SELECT version();
BEGIN;
CREATE TEMP TABLE recovery_time_probe (
  issued_at_epoch BIGINT NOT NULL,
  expires_at_epoch BIGINT NOT NULL,
  CHECK ({check})
) ON COMMIT DROP;
DO $probe$
DECLARE
  sample record;
  admitted boolean;
  checked integer := 0;
BEGIN
  FOR sample IN SELECT * FROM (VALUES
{values}
  ) AS cases(issued, expires, expected) LOOP
    admitted := true;
    BEGIN
      INSERT INTO recovery_time_probe VALUES (sample.issued, sample.expires);
    EXCEPTION
      WHEN check_violation OR numeric_value_out_of_range OR not_null_violation THEN
        admitted := false;
    END;
    IF admitted IS DISTINCT FROM sample.expected THEN
      RAISE EXCEPTION 'Recovery time mismatch: issued=%, expires=%, expected=%',
        sample.issued, sample.expires, sample.expected;
    END IF;
    checked := checked + 1;
  END LOOP;
  -- Independently exercise the checked-bigint assumption, irrespective of
  -- PostgreSQL's permitted CHECK conjunct evaluation order.
  BEGIN
    PERFORM '9223372036854775807'::bigint - '-1'::bigint;
    RAISE EXCEPTION 'Expected signed bigint overflow';
  EXCEPTION WHEN numeric_value_out_of_range THEN NULL;
  END;
  RAISE NOTICE 'Recovery time CHECK: % finite boundary probes passed; no universal proof', checked;
END
$probe$;
ROLLBACK;
"""


if __name__ == '__main__':
    print(sql())
