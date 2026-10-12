-- Removes the rule that lines are created only with their checkout. The
-- 2026-10-04 correspondence rules (deferred sum check, immutable terms and
-- lines) are left in place. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TRIGGER IF EXISTS trg_commerce_checkout_line_fixed ON commerce_checkout_line_item;
DROP FUNCTION IF EXISTS commerce_reject_late_checkout_line();
COMMIT;
