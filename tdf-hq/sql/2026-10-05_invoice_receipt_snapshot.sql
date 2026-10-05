-- Preserve issued evidence. Incompatible legacy rows stop this migration;
-- never rewrite currency, amounts or duplicate receipts to make it pass.
CREATE TABLE IF NOT EXISTS receipt_number_counter (
    receipt_year integer PRIMARY KEY CHECK (receipt_year BETWEEN 1 AND 9999),
    last_number bigint NOT NULL CHECK (last_number >= 0)
);

-- Use numbers already issued, not row counts (which reuse deleted/gapped IDs).
INSERT INTO receipt_number_counter (receipt_year, last_number)
SELECT match[1]::integer, max(match[2]::numeric)::bigint
FROM receipt
CROSS JOIN LATERAL regexp_match(number, '^R-([0-9]{4})-([0-9]+)$') AS match
GROUP BY match[1]::integer
ON CONFLICT (receipt_year) DO UPDATE
SET last_number = greatest(receipt_number_counter.last_number, EXCLUDED.last_number);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
                 WHERE conrelid = 'receipt'::regclass AND conname = 'unique_receipt_invoice') THEN
    ALTER TABLE receipt ADD CONSTRAINT unique_receipt_invoice UNIQUE (invoice_id);
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
                 WHERE conrelid = 'invoice'::regclass AND conname = 'invoice_receipt_snapshot_identity') THEN
    ALTER TABLE invoice ADD CONSTRAINT invoice_receipt_snapshot_identity
      UNIQUE (id, currency, subtotal_cents, tax_cents, total_cents);
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
                 WHERE conrelid = 'receipt'::regclass AND conname = 'receipt_invoice_snapshot') THEN
    ALTER TABLE receipt ADD CONSTRAINT receipt_invoice_snapshot
      FOREIGN KEY (invoice_id, currency, subtotal_cents, tax_cents, total_cents)
      REFERENCES invoice (id, currency, subtotal_cents, tax_cents, total_cents)
      ON UPDATE RESTRICT ON DELETE RESTRICT;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
                 WHERE conrelid = 'receipt'::regclass AND conname = 'receipt_nonnegative_snapshot') THEN
    ALTER TABLE receipt ADD CONSTRAINT receipt_nonnegative_snapshot CHECK (
      currency ~ '^[A-Z]{3}$' AND subtotal_cents >= 0 AND tax_cents >= 0
      AND total_cents::numeric = subtotal_cents::numeric + tax_cents::numeric
    );
  END IF;
END $$;
