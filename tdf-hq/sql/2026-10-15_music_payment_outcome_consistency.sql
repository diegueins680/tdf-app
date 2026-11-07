-- PAY-MUSIC-OUTCOME-001. Compatibility repair; historical migration bytes stay intact.
-- Keep this migration when rolling back application code. Restoring the older
-- trigger would reintroduce cancellation and closed-order fulfillment defects.
BEGIN;

LOCK TABLE commerce_checkout_session, music_purchase_order, music_entitlement
  IN SHARE ROW EXCLUSIVE MODE;

-- Existing contradictory grants require explicit financial review. Never hide
-- historical inconsistency by deleting evidence or silently reopening orders.
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM music_entitlement entitlement
    JOIN music_purchase_order purchase ON purchase.id = entitlement.purchase_order_id
    WHERE entitlement.source_kind = 'purchase' AND entitlement.status = 'active'
      AND purchase.state <> 'paid'
  ) THEN
    RAISE EXCEPTION 'music purchase entitlement history requires reconciliation before migration'
      USING ERRCODE = '23514';
  END IF;
END;
$$;

-- Failure remains a payment retry quarantine even though fulfillment is no
-- longer incorrectly cancelled. Call in the same transaction as attempt
-- creation/replay; the checkout lock prevents stale admission after failure.
CREATE OR REPLACE FUNCTION music_checkout_payment_attempt_allowed(checkout_key UUID)
RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE
  checkout_status TEXT;
BEGIN
  SELECT status INTO checkout_status FROM commerce_checkout_session
    WHERE id = checkout_key AND domain_type = 'music_download'
    FOR UPDATE;
  -- Late pending/binding responses can reopen the checkout status. The
  -- persisted failure marker must still quarantine every subsequent attempt.
  RETURN FOUND AND checkout_status IN ('awaiting_payment','processing')
    AND NOT EXISTS (
      SELECT 1 FROM commerce_payment_attempt attempt WHERE attempt.checkout_id = checkout_key
        AND (attempt.status = 'failed' OR attempt.failure_code IS NOT NULL)
    );
END;
$$;

CREATE OR REPLACE FUNCTION music_sync_verified_checkout()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  purchase music_purchase_order%ROWTYPE;
  entitlement_asset UUID;
  provider_reference_value TEXT;
  verified_binding RECORD;
BEGIN
  IF NEW.domain_type <> 'music_download' OR OLD.status IS NOT DISTINCT FROM NEW.status THEN
    RETURN NEW;
  END IF;

  SELECT * INTO purchase FROM music_purchase_order
  WHERE id::TEXT = NEW.domain_order_id AND checkout_id = NEW.id
  FOR UPDATE;
  IF purchase.id IS NULL THEN
    RAISE EXCEPTION 'music checkout is not linked to its canonical purchase order';
  END IF;

  IF NEW.status = 'paid' THEN
    SELECT binding.provider, binding.environment, binding.merchant_account_ref,
      binding.provider_resource_id INTO verified_binding
    FROM commerce_payment_attempt attempt
    JOIN commerce_provider_binding binding
      ON binding.payment_attempt_id = attempt.id
      AND binding.provider = attempt.provider
      AND binding.environment = attempt.environment
      AND binding.merchant_account_ref = attempt.merchant_account_ref
      AND binding.merchant_reference = NEW.domain_order_id
      AND binding.amount_minor = NEW.total_minor
      AND binding.currency = NEW.currency
    WHERE attempt.checkout_id = NEW.id AND attempt.status = 'succeeded'
      AND attempt.environment = NEW.environment
      AND attempt.amount_minor = NEW.total_minor AND attempt.currency = NEW.currency
      AND ((attempt.provider = 'paypal' AND binding.resource_type = 'capture')
        OR (attempt.provider = 'datafast' AND binding.resource_type IN ('checkout','payment'))
        OR (attempt.provider = 'stripe' AND binding.resource_type = 'payment'))
    ORDER BY attempt.updated_at DESC, attempt.id DESC, binding.id DESC LIMIT 1;
    IF NOT FOUND THEN
      RAISE EXCEPTION 'music payment outcome has no canonical verified provider binding'
        USING ERRCODE = '23514';
    END IF;
    provider_reference_value := verified_binding.provider_resource_id;

    -- Money evidence is retained even if fulfillment is already closed. Do not
    -- reopen the order or grant a download; accounting must resolve the case.
    IF purchase.state NOT IN ('pending','awaiting_payment','paid') THEN
      INSERT INTO commerce_reconciliation_exception(
        provider, environment, merchant_account_ref, exception_type,
        internal_reference, provider_reference, expected_amount_minor,
        actual_amount_minor, currency, status
      ) VALUES (
        verified_binding.provider, verified_binding.environment,
        verified_binding.merchant_account_ref, 'music_paid_closed_order',
        NEW.id::TEXT, provider_reference_value, purchase.gross_minor,
        NEW.paid_minor, NEW.currency, 'open'
      ) ON CONFLICT DO NOTHING;
      INSERT INTO music_release_audit_event(
        release_id, release_version_id, actor_party_id, event_type, idempotency_key, data
      ) SELECT version.release_id, purchase.release_version_id, purchase.buyer_party_id,
          'purchase_payment_requires_review', 'music-payment-review:' || NEW.id,
          jsonb_build_object('purchase_order_id', purchase.id, 'checkout_id', NEW.id,
            'order_state', purchase.state)
        FROM music_release_version version WHERE version.id = purchase.release_version_id
      ON CONFLICT DO NOTHING;
      RETURN NEW;
    END IF;

    UPDATE music_purchase_order
    SET state = 'paid', paid_at = COALESCE(paid_at, NEW.paid_at, NOW()),
        provider_reference = COALESCE(provider_reference, provider_reference_value)
    WHERE id = purchase.id AND state IN ('pending','awaiting_payment');

    SELECT rule.downloadable_asset_id INTO entitlement_asset
      FROM music_availability_rule rule WHERE rule.id = purchase.availability_rule_id;
    IF entitlement_asset IS NULL THEN
      RAISE EXCEPTION 'paid music purchase has no downloadable asset';
    END IF;
    INSERT INTO music_entitlement(
      buyer_party_id,release_version_id,asset_id,source_kind,purchase_order_id,status,max_downloads
    ) VALUES (
      purchase.buyer_party_id,purchase.release_version_id,entitlement_asset,'purchase',purchase.id,'active',10
    ) ON CONFLICT (buyer_party_id,release_version_id,asset_id,source_kind,purchase_order_id)
      DO UPDATE SET status='active',revoked_at=NULL;

    INSERT INTO music_release_audit_event(
      release_id,release_version_id,actor_party_id,event_type,data
    ) SELECT version.release_id,purchase.release_version_id,purchase.buyer_party_id,'purchase_verified',
        jsonb_build_object('purchase_order_id',purchase.id,'checkout_id',NEW.id)
      FROM music_release_version version WHERE version.id=purchase.release_version_id;
  ELSIF NEW.status = 'refunded' THEN
    UPDATE music_purchase_order SET state='refunded' WHERE id=purchase.id AND state='paid';
    UPDATE music_entitlement SET status='refunded',revoked_at=COALESCE(revoked_at,NOW())
      WHERE purchase_order_id=purchase.id AND status='active';
  ELSIF NEW.status IN ('disputed','chargeback') THEN
    UPDATE music_purchase_order SET state='chargeback' WHERE id=purchase.id AND state IN ('paid','refunded');
    UPDATE music_entitlement SET status='revoked',revoked_at=COALESCE(revoked_at,NOW())
      WHERE purchase_order_id=purchase.id AND status='active';
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE music_purchase_order SET state='cancelled'
      WHERE id=purchase.id AND state IN ('pending','awaiting_payment');
  END IF;
  RETURN NEW;
END;
$$;

COMMIT;
