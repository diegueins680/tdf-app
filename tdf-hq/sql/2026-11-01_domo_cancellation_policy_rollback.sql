-- Rollback of 2026-11-01_domo_cancellation_policy. Removes the policy table
-- only; quotes, checkouts and refunds are untouched.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TRIGGER IF EXISTS trg_domo_cancellation_policy_protect ON public.domo_cancellation_policy;
DROP FUNCTION IF EXISTS public.domo_cancellation_policy_protect();
DROP TABLE IF EXISTS public.domo_cancellation_policy;
COMMIT;
