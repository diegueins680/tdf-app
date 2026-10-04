-- PAY-CHECKOUT-003: preserve integer floor commission while allowing valid
-- BIGINT outputs whose multiplication intermediate exceeds BIGINT.
-- The applied September migration is immutable; replace only its known CHECK.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';
LOCK TABLE merch_order IN ACCESS EXCLUSIVE MODE;
DO $preflight$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid='public.merch_order'::regclass AND conname='merch_order_check2'
      AND contype='c' AND pg_get_expr(conbin,conrelid)=
        '(tdf_commission_minor = (((product_subtotal_minor - discount_minor) * tdf_commission_bps) / 10000))'
  ) THEN
    RAISE EXCEPTION 'Unexpected merchandise commission constraint; review schema before replacement';
  END IF;
END
$preflight$;
ALTER TABLE merch_order DROP CONSTRAINT merch_order_check2;
ALTER TABLE merch_order ADD CONSTRAINT merch_order_commission_exact CHECK (
  tdf_commission_minor::numeric = trunc(
    (product_subtotal_minor::numeric - discount_minor::numeric)
    * tdf_commission_bps::numeric / 10000
  )
);
-- Existing rows are validated by ADD CONSTRAINT. No amounts are rewritten.
-- Retain the compatible constraint during forward recovery; no down migration.
COMMIT;
