-- A posted ledger transaction was validated only on the status change
-- draft -> posted, and its entries were protected only against UPDATE and
-- DELETE. So a posted transaction could gain entries (unbalancing it), return
-- to 'draft' (where its entries became editable again), and a transaction
-- could be inserted already 'posted' or 'reversed' with no entries at all.
-- Posting is now the only way to 'posted', entries are fixed from that moment,
-- and the balance check compares exact per-currency sums (numeric SUM, never
-- a modular bigint). Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION commerce_validate_ledger_posting()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  reversed_status TEXT;
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NEW.status <> 'draft' THEN
      RAISE EXCEPTION 'Ledger transaction % must be created as draft and posted after its entries', NEW.id
        USING ERRCODE = 'check_violation';
    END IF;
    RETURN NEW;
  END IF;

  IF OLD.status = 'reversed'
     AND (NEW.status IS DISTINCT FROM 'reversed' OR NEW.reversal_of IS DISTINCT FROM OLD.reversal_of) THEN
    RAISE EXCEPTION 'Ledger transaction % is reversed and cannot change status or reversal', NEW.id
      USING ERRCODE = 'check_violation';
  END IF;
  IF OLD.status = 'posted' AND NEW.status = 'draft' THEN
    RAISE EXCEPTION 'Ledger transaction % is posted; create a reversing transaction', NEW.id
      USING ERRCODE = 'check_violation';
  END IF;
  IF OLD.status = 'draft' AND NEW.status = 'reversed' THEN
    RAISE EXCEPTION 'Ledger transaction % is a draft and has nothing to reverse', NEW.id
      USING ERRCODE = 'check_violation';
  END IF;
  IF NEW.status = 'reversed' AND OLD.status <> 'reversed' THEN
    SELECT status INTO reversed_status FROM commerce_ledger_transaction WHERE id = NEW.reversal_of;
    IF reversed_status IS DISTINCT FROM 'posted' OR NEW.reversal_of = NEW.id THEN
      RAISE EXCEPTION 'Ledger transaction % can only be reversed by a posted transaction', NEW.id
        USING ERRCODE = 'check_violation';
    END IF;
  END IF;

  IF NEW.status = 'posted' AND OLD.status <> 'posted' THEN
    IF EXISTS (
      SELECT 1
      FROM commerce_ledger_entry
      WHERE transaction_id = NEW.id
      GROUP BY currency
      HAVING SUM(amount_minor::numeric) <> 0
    ) THEN
      RAISE EXCEPTION 'Ledger transaction % is not balanced by currency', NEW.id
        USING ERRCODE = 'check_violation';
    END IF;
    IF NOT EXISTS (SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = NEW.id) THEN
      RAISE EXCEPTION 'Ledger transaction % has no entries', NEW.id
        USING ERRCODE = 'check_violation';
    END IF;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_ledger_posting ON commerce_ledger_transaction;
CREATE TRIGGER trg_commerce_ledger_posting
  BEFORE INSERT OR UPDATE OF status, reversal_of ON commerce_ledger_transaction
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_ledger_posting();

-- Entries of a posted or reversed transaction are fixed: no insert, update or
-- delete, and no entry moves between transactions. The transaction row is
-- locked so a concurrent posting sees the entry or the entry sees the posted
-- status, never neither.
CREATE OR REPLACE FUNCTION commerce_protect_posted_ledger()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  transaction_status TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.transaction_id IS DISTINCT FROM OLD.transaction_id THEN
    RAISE EXCEPTION 'Ledger entries cannot move between transactions'
      USING ERRCODE = 'check_violation';
  END IF;
  SELECT status INTO transaction_status
    FROM commerce_ledger_transaction
    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id)
    FOR NO KEY UPDATE;
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

-- Posting, the immutability check and the reference check read entries by
-- transaction; the table had no index for that.
CREATE INDEX IF NOT EXISTS idx_commerce_ledger_entry_transaction
  ON commerce_ledger_entry(transaction_id);

-- Existing posted transactions that the old rules let drift are reported,
-- not altered: a correction is a reviewed reversing transaction.
DO $$
DECLARE
  drifted RECORD;
BEGIN
  FOR drifted IN
    SELECT txn.id, entry.currency, SUM(entry.amount_minor::numeric) AS balance
    FROM commerce_ledger_transaction txn
    JOIN commerce_ledger_entry entry ON entry.transaction_id = txn.id
    WHERE txn.status IN ('posted', 'reversed')
    GROUP BY txn.id, entry.currency
    HAVING SUM(entry.amount_minor::numeric) <> 0
  LOOP
    RAISE WARNING 'Posted ledger transaction % is unbalanced in % by %',
      drifted.id, drifted.currency, drifted.balance;
  END LOOP;
  FOR drifted IN
    SELECT txn.id FROM commerce_ledger_transaction txn
    WHERE txn.status IN ('posted', 'reversed')
      AND NOT EXISTS (SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = txn.id)
  LOOP
    RAISE WARNING 'Posted ledger transaction % has no entries', drifted.id;
  END LOOP;
END $$;

COMMIT;
