-- SYS-MONEY-003: a row lock alone does not invalidate an older MVCC snapshot.
-- An entry writer must also create a transaction-row version. Otherwise a
-- REPEATABLE READ poster can wait for that writer, then validate stale entries
-- and commit an unbalanced posting. Keep all previous migration bytes intact.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE commerce_ledger_transaction, commerce_ledger_entry IN SHARE ROW EXCLUSIVE MODE;

-- Refuse history admitted through the old snapshot gap; never rewrite money.
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_ledger_transaction txn
    WHERE txn.status IN ('posted', 'reversed')
      AND (NOT EXISTS (SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = txn.id)
           OR EXISTS (SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = txn.id
                      GROUP BY currency HAVING SUM(amount_minor::numeric) <> 0))
  ) THEN
    RAISE EXCEPTION 'Legacy posted ledger transactions are empty or unbalanced; reconcile before migration'
      USING ERRCODE = 'check_violation';
  END IF;
END $$;

CREATE OR REPLACE FUNCTION commerce_protect_posted_ledger()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  transaction_status TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.transaction_id IS DISTINCT FROM OLD.transaction_id THEN
    RAISE EXCEPTION 'Ledger entries cannot move between transactions'
      USING ERRCODE = 'check_violation';
  END IF;
  -- This no-value-change UPDATE is intentional. Besides serializing writers,
  -- it makes an older REPEATABLE READ / SERIALIZABLE poster fail with 40001.
  -- READ COMMITTED posting refreshes its entry view after the lock wait.
  -- The posting trigger fires only for status/reversal_of, so this does not
  -- recursively run that trigger or change any accounting metadata value.
  UPDATE commerce_ledger_transaction SET created_at = created_at
    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id)
    RETURNING status INTO transaction_status;
  IF transaction_status IN ('posted', 'reversed') THEN
    RAISE EXCEPTION 'Posted ledger entries are immutable; create a reversing transaction'
      USING ERRCODE = 'check_violation';
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_ledger_entry_immutable ON commerce_ledger_entry;
CREATE TRIGGER trg_commerce_ledger_entry_immutable
  BEFORE INSERT OR UPDATE OR DELETE ON commerce_ledger_entry
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_posted_ledger();
COMMIT;

-- Application rollback retains this forward-compatible integrity guard.
-- Replacing it with the lock-only predecessor reopens a demonstrated anomaly.
