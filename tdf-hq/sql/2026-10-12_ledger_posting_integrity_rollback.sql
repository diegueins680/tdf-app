-- Restores the 2026-08-13 posting validation (status change only) and entry
-- protection (update and delete only). Transactions and entries are left as
-- they are. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION commerce_validate_ledger_posting()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status = 'posted' AND OLD.status <> 'posted' AND EXISTS (
    SELECT 1
    FROM commerce_ledger_entry
    WHERE transaction_id = NEW.id
    GROUP BY currency
    HAVING SUM(amount_minor) <> 0
  ) THEN
    RAISE EXCEPTION 'Ledger transaction % is not balanced by currency', NEW.id;
  END IF;
  IF NEW.status = 'posted' AND OLD.status <> 'posted' AND NOT EXISTS (
    SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = NEW.id
  ) THEN
    RAISE EXCEPTION 'Ledger transaction % has no entries', NEW.id;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_ledger_posting ON commerce_ledger_transaction;
CREATE TRIGGER trg_commerce_ledger_posting
  BEFORE UPDATE OF status ON commerce_ledger_transaction
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_ledger_posting();

CREATE OR REPLACE FUNCTION commerce_protect_posted_ledger()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  transaction_status TEXT;
BEGIN
  SELECT status INTO transaction_status
    FROM commerce_ledger_transaction
    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id);
  IF transaction_status IN ('posted', 'reversed') THEN
    RAISE EXCEPTION 'Posted ledger entries are immutable; create a reversing transaction';
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_ledger_entry_immutable ON commerce_ledger_entry;
CREATE TRIGGER trg_commerce_ledger_entry_immutable
  BEFORE UPDATE OR DELETE ON commerce_ledger_entry
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_posted_ledger();

COMMIT;
