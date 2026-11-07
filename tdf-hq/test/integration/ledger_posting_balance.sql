-- SYS-MONEY-003 at the runtime authority: commerce_validate_ledger_posting on
-- the production schema. Every case runs in a transaction that is rolled back.
-- After the posting-integrity migration, rejected postings use SQLSTATE 23514.
-- Require its exact ledger diagnostic, not an unrelated constraint failure.
-- Draft-only creation and posted-entry immutability are additionally exercised
-- by ledger-posting-postgresql; this fixture focuses on exact arithmetic.
BEGIN;
CREATE TEMP TABLE ledger_case(id UUID, label TEXT, entries TEXT[][], expect_ok BOOLEAN);
INSERT INTO ledger_case VALUES
  ('00000000-0000-4000-8000-00000000b001', 'balanced', ARRAY[['USD','2000'],['USD','-2000']], TRUE),
  ('00000000-0000-4000-8000-00000000b002', 'unbalanced', ARRAY[['USD','2000'],['USD','-1999']], FALSE),
  ('00000000-0000-4000-8000-00000000b003', 'cross-currency', ARRAY[['USD','100'],['EUR','-100']], FALSE),
  -- M + M + 2 = 2^64: zero under modular Int64 arithmetic, never under exact sums.
  ('00000000-0000-4000-8000-00000000b004', 'modular-wrap',
    ARRAY[['USD','9223372036854775807'],['USD','9223372036854775807'],['USD','2']], FALSE),
  ('00000000-0000-4000-8000-00000000b005', 'extreme-balanced',
    ARRAY[['USD','9223372036854775807'],['USD','9223372036854775807'],
          ['USD','-9223372036854775807'],['USD','-9223372036854775807']], TRUE),
  ('00000000-0000-4000-8000-00000000b006', 'empty', ARRAY[]::TEXT[][], FALSE);

DO $$
DECLARE
  c RECORD;
  i INT;
  posted BOOLEAN;
  rejection TEXT;
BEGIN
  FOR c IN SELECT * FROM ledger_case LOOP
    INSERT INTO commerce_ledger_transaction(id, transaction_type, source_type, source_id,
      status, effective_at, correlation_id, created_by)
    VALUES (c.id, 'payment_capture', 'ledger_probe', c.label, 'draft', NOW(),
      'ledger-probe:' || c.label, 'ledger-probe');
    IF array_length(c.entries, 1) IS NOT NULL THEN
      FOR i IN 1 .. array_length(c.entries, 1) LOOP
        INSERT INTO commerce_ledger_entry(transaction_id, account_code, currency, amount_minor)
        VALUES (c.id, 'probe.' || i, c.entries[i][1], c.entries[i][2]::BIGINT);
      END LOOP;
    END IF;
    BEGIN
      UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = c.id;
      posted := TRUE;
    EXCEPTION WHEN check_violation THEN
      GET STACKED DIAGNOSTICS rejection = MESSAGE_TEXT;
      IF rejection NOT IN (
        format('Ledger transaction %s is not balanced by currency', c.id),
        format('Ledger transaction %s has no entries', c.id)
      ) THEN
        RAISE;
      END IF;
      posted := FALSE;
    END;
    IF posted IS DISTINCT FROM c.expect_ok THEN
      RAISE EXCEPTION 'Ledger posting case % expected posted=% but got %', c.label, c.expect_ok, posted;
    END IF;
  END LOOP;
END $$;
ROLLBACK;
