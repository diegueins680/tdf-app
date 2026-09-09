-- Artist and band merch storefronts.
--
-- Merch has its own catalog and inventory model. It deliberately does not use
-- studio assets or equipment listings. Financial execution remains attached to
-- the provider-neutral commerce_checkout_* tables.
BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS merch_store (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  directory_profile_id UUID NOT NULL UNIQUE REFERENCES directory_profile(id) ON DELETE RESTRICT,
  seller_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  primary_owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  slug TEXT NOT NULL UNIQUE,
  display_name TEXT NOT NULL,
  description TEXT,
  cover_image_url TEXT,
  logo_image_url TEXT,
  country_code TEXT NOT NULL DEFAULT 'EC',
  currency TEXT NOT NULL DEFAULT 'USD',
  application_status TEXT NOT NULL DEFAULT 'requested',
  operational_status TEXT NOT NULL DEFAULT 'inactive',
  application_note TEXT NOT NULL,
  application_idempotency_key TEXT NOT NULL,
  application_request_sha256 TEXT NOT NULL,
  reviewer_notes TEXT,
  requested_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  reviewed_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  reviewed_at TIMESTAMPTZ,
  activated_at TIMESTAMPTZ,
  suspended_at TIMESTAMPTZ,
  suspension_reason TEXT,
  version BIGINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(primary_owner_party_id, application_idempotency_key),
  CHECK (slug ~ '^[a-z0-9][a-z0-9-]{1,119}$'),
  CHECK (length(trim(display_name)) BETWEEN 1 AND 160),
  CHECK (length(trim(application_note)) BETWEEN 10 AND 2000),
  CHECK (length(application_idempotency_key) BETWEEN 8 AND 200),
  CHECK (application_request_sha256 ~ '^[a-f0-9]{64}$'),
  CHECK (description IS NULL OR length(description) <= 2000),
  CHECK (country_code ~ '^[A-Z]{2}$'),
  CHECK (currency ~ '^[A-Z]{3}$'),
  CHECK (application_status IN ('requested','under_review','approved','rejected','withdrawn')),
  CHECK (operational_status IN ('inactive','active','suspended','closed')),
  CHECK (application_status NOT IN ('approved') OR reviewed_by IS NOT NULL),
  CHECK (reviewed_at IS NULL OR reviewed_by IS NOT NULL),
  CHECK (operational_status <> 'active' OR (application_status = 'approved' AND activated_at IS NOT NULL)),
  CHECK (operational_status <> 'suspended' OR (suspended_at IS NOT NULL AND length(trim(suspension_reason)) >= 10))
);

CREATE INDEX IF NOT EXISTS merch_store_review_queue_idx
  ON merch_store(application_status, requested_at, id)
  WHERE application_status IN ('requested','under_review');
CREATE INDEX IF NOT EXISTS merch_store_public_idx
  ON merch_store(operational_status, slug)
  WHERE application_status = 'approved' AND operational_status = 'active';

CREATE TABLE IF NOT EXISTS merch_store_member (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  member_role TEXT NOT NULL DEFAULT 'collaborator',
  invitation_status TEXT NOT NULL DEFAULT 'pending',
  can_catalog BOOLEAN NOT NULL DEFAULT FALSE,
  can_stock BOOLEAN NOT NULL DEFAULT FALSE,
  can_orders BOOLEAN NOT NULL DEFAULT FALSE,
  can_fulfillment BOOLEAN NOT NULL DEFAULT FALSE,
  can_finance BOOLEAN NOT NULL DEFAULT FALSE,
  can_settings BOOLEAN NOT NULL DEFAULT FALSE,
  invited_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  invitation_idempotency_key TEXT,
  invitation_request_sha256 TEXT,
  accepted_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  revoke_reason TEXT,
  expires_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(store_id, party_id),
  UNIQUE(store_id, invited_by, invitation_idempotency_key),
  CHECK (member_role IN ('owner','collaborator')),
  CHECK (member_role = 'owner' OR (
    invitation_idempotency_key IS NOT NULL
    AND invitation_request_sha256 IS NOT NULL
    AND length(invitation_idempotency_key) BETWEEN 8 AND 200
    AND invitation_request_sha256 ~ '^[a-f0-9]{64}$'
  )),
  CHECK (invitation_status IN ('pending','accepted','declined','expired','revoked')),
  CHECK (member_role <> 'owner' OR (
    invitation_status = 'accepted' AND can_catalog AND can_stock AND can_orders
    AND can_fulfillment AND can_finance AND can_settings
  )),
  CHECK (invitation_status <> 'accepted' OR accepted_at IS NOT NULL),
  CHECK (invitation_status <> 'revoked' OR (revoked_at IS NOT NULL AND length(trim(revoke_reason)) >= 5)),
  CHECK (expires_at IS NULL OR expires_at > created_at)
);
CREATE UNIQUE INDEX IF NOT EXISTS merch_store_primary_owner_uidx
  ON merch_store_member(store_id) WHERE member_role = 'owner' AND invitation_status = 'accepted';
CREATE INDEX IF NOT EXISTS merch_store_member_party_idx
  ON merch_store_member(party_id, invitation_status, store_id);

CREATE TABLE IF NOT EXISTS merch_commission_policy (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID REFERENCES merch_store(id) ON DELETE RESTRICT,
  commission_bps INTEGER NOT NULL DEFAULT 1000,
  reason TEXT NOT NULL,
  effective_from TIMESTAMPTZ NOT NULL DEFAULT now(),
  effective_until TIMESTAMPTZ,
  approved_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (commission_bps BETWEEN 0 AND 10000),
  CHECK (length(trim(reason)) BETWEEN 5 AND 500),
  CHECK (effective_until IS NULL OR effective_until > effective_from)
);
CREATE INDEX IF NOT EXISTS merch_commission_policy_store_idx
  ON merch_commission_policy(store_id, effective_from DESC, id);
CREATE UNIQUE INDEX IF NOT EXISTS merch_commission_policy_global_active_uidx
  ON merch_commission_policy((store_id IS NULL)) WHERE store_id IS NULL AND effective_until IS NULL;
CREATE UNIQUE INDEX IF NOT EXISTS merch_commission_policy_store_active_uidx
  ON merch_commission_policy(store_id) WHERE store_id IS NOT NULL AND effective_until IS NULL;

CREATE TABLE IF NOT EXISTS merch_store_policy (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  version INTEGER NOT NULL CHECK (version > 0),
  shipping_policy TEXT NOT NULL,
  return_policy TEXT NOT NULL,
  preorder_policy TEXT,
  support_email TEXT,
  status TEXT NOT NULL DEFAULT 'draft',
  effective_at TIMESTAMPTZ,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(store_id, version),
  CHECK (status IN ('draft','active','superseded')),
  CHECK (length(trim(shipping_policy)) BETWEEN 20 AND 5000),
  CHECK (length(trim(return_policy)) BETWEEN 20 AND 5000),
  CHECK (status <> 'active' OR effective_at IS NOT NULL)
);
CREATE UNIQUE INDEX IF NOT EXISTS merch_store_policy_active_uidx
  ON merch_store_policy(store_id) WHERE status = 'active';

CREATE TABLE IF NOT EXISTS merch_shipping_zone (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  name TEXT NOT NULL,
  country_code TEXT NOT NULL DEFAULT 'EC',
  subdivision_codes TEXT[] NOT NULL DEFAULT '{}',
  delivery_method TEXT NOT NULL,
  rate_minor BIGINT NOT NULL DEFAULT 0,
  free_shipping_min_minor BIGINT,
  estimated_min_days INTEGER,
  estimated_max_days INTEGER,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (length(trim(name)) BETWEEN 1 AND 120),
  CHECK (country_code ~ '^[A-Z]{2}$'),
  CHECK (delivery_method IN ('coordinated_pickup','national_shipping')),
  CHECK (rate_minor >= 0),
  CHECK (free_shipping_min_minor IS NULL OR free_shipping_min_minor > 0),
  CHECK (estimated_min_days IS NULL OR estimated_min_days >= 0),
  CHECK (estimated_max_days IS NULL OR estimated_max_days >= estimated_min_days)
);

CREATE TABLE IF NOT EXISTS merch_product (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  slug TEXT NOT NULL,
  name TEXT NOT NULL,
  description TEXT NOT NULL,
  category TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft',
  visibility TEXT NOT NULL DEFAULT 'public',
  availability_mode TEXT NOT NULL DEFAULT 'in_stock',
  preorder_release_at TIMESTAMPTZ,
  publish_at TIMESTAMPTZ,
  unpublish_at TIMESTAMPTZ,
  buyer_limit INTEGER,
  policy_id UUID REFERENCES merch_store_policy(id) ON DELETE RESTRICT,
  rejection_reason TEXT,
  submitted_at TIMESTAMPTZ,
  reviewed_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  reviewed_at TIMESTAMPTZ,
  published_at TIMESTAMPTZ,
  archived_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  create_idempotency_key TEXT NOT NULL,
  create_request_sha256 TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(store_id, slug),
  UNIQUE(store_id, create_idempotency_key),
  CHECK (slug ~ '^[a-z0-9][a-z0-9-]{1,119}$'),
  CHECK (length(trim(name)) BETWEEN 1 AND 180),
  CHECK (length(trim(description)) BETWEEN 1 AND 10000),
  CHECK (length(create_idempotency_key) BETWEEN 8 AND 200),
  CHECK (create_request_sha256 ~ '^[a-f0-9]{64}$'),
  CHECK (category IN ('apparel','vinyl','cd','cassette','poster','accessory','limited_edition','bundle','other')),
  CHECK (status IN ('draft','pending_review','published','sold_out','paused','rejected','archived')),
  CHECK (visibility IN ('public','unlisted','hidden')),
  CHECK (availability_mode IN ('in_stock','preorder','made_to_order')),
  CHECK (availability_mode <> 'preorder' OR preorder_release_at IS NOT NULL),
  CHECK (unpublish_at IS NULL OR publish_at IS NULL OR unpublish_at > publish_at),
  CHECK (buyer_limit IS NULL OR buyer_limit > 0),
  CHECK (status <> 'pending_review' OR submitted_at IS NOT NULL),
  CHECK (status <> 'published' OR (reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL AND published_at IS NOT NULL)),
  CHECK (status <> 'rejected' OR (reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL AND length(trim(rejection_reason)) >= 5)),
  CHECK (status <> 'archived' OR archived_at IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS merch_product_public_idx
  ON merch_product(store_id, category, published_at DESC, id)
  WHERE status IN ('published','sold_out') AND visibility = 'public';
CREATE INDEX IF NOT EXISTS merch_product_review_idx
  ON merch_product(status, submitted_at, id) WHERE status = 'pending_review';

CREATE TABLE IF NOT EXISTS merch_product_variant (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE RESTRICT,
  sku TEXT NOT NULL,
  name TEXT NOT NULL,
  option_values JSONB NOT NULL DEFAULT '{}'::jsonb,
  price_minor BIGINT NOT NULL,
  compare_at_price_minor BIGINT,
  currency TEXT NOT NULL DEFAULT 'USD',
  weight_grams INTEGER NOT NULL,
  customs_description TEXT,
  stock_mode TEXT NOT NULL DEFAULT 'finite',
  stock_on_hand INTEGER NOT NULL DEFAULT 0,
  stock_reserved INTEGER NOT NULL DEFAULT 0,
  stock_sold INTEGER NOT NULL DEFAULT 0,
  reorder_threshold INTEGER NOT NULL DEFAULT 0,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  version BIGINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (length(trim(sku)) BETWEEN 1 AND 120),
  CHECK (length(trim(name)) BETWEEN 1 AND 180),
  CHECK (jsonb_typeof(option_values) = 'object'),
  CHECK (price_minor > 0),
  CHECK (compare_at_price_minor IS NULL OR compare_at_price_minor >= price_minor),
  CHECK (currency ~ '^[A-Z]{3}$'),
  CHECK (weight_grams BETWEEN 1 AND 100000),
  CHECK (stock_mode IN ('finite','made_to_order')),
  CHECK (stock_on_hand >= 0 AND stock_reserved >= 0 AND stock_sold >= 0),
  CHECK (stock_mode = 'made_to_order' OR stock_reserved + stock_sold <= stock_on_hand),
  CHECK (reorder_threshold >= 0)
);
CREATE UNIQUE INDEX IF NOT EXISTS merch_product_variant_store_sku_uidx
  ON merch_product_variant(store_id, lower(sku));
CREATE INDEX IF NOT EXISTS merch_product_variant_product_idx ON merch_product_variant(product_id, active, id);

CREATE TABLE IF NOT EXISTS merch_product_image (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE RESTRICT,
  object_key TEXT NOT NULL UNIQUE,
  original_filename TEXT NOT NULL,
  mime_type TEXT NOT NULL,
  byte_size BIGINT NOT NULL,
  width_px INTEGER NOT NULL,
  height_px INTEGER NOT NULL,
  checksum_sha256 TEXT NOT NULL,
  variants JSONB NOT NULL DEFAULT '{}'::jsonb,
  alt_text TEXT NOT NULL,
  sort_order INTEGER NOT NULL DEFAULT 0,
  scan_status TEXT NOT NULL DEFAULT 'pending',
  moderation_status TEXT NOT NULL DEFAULT 'pending',
  deleted_at TIMESTAMPTZ,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (object_key ~ '^merch/[a-f0-9-]{36}/[a-f0-9-]{36}/[a-z0-9._-]+$'),
  CHECK (mime_type IN ('image/jpeg','image/png','image/webp')),
  CHECK (byte_size BETWEEN 1 AND 10485760),
  CHECK (width_px BETWEEN 1 AND 12000 AND height_px BETWEEN 1 AND 12000),
  CHECK (checksum_sha256 ~ '^[a-f0-9]{64}$'),
  CHECK (jsonb_typeof(variants) = 'object'),
  CHECK (length(trim(alt_text)) BETWEEN 1 AND 500),
  CHECK (sort_order >= 0),
  CHECK (scan_status IN ('pending','clean','rejected','failed')),
  CHECK (moderation_status IN ('pending','allowed','blocked'))
);
CREATE UNIQUE INDEX IF NOT EXISTS merch_product_image_sort_uidx
  ON merch_product_image(product_id, sort_order) WHERE deleted_at IS NULL;

CREATE TABLE IF NOT EXISTS merch_cart (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  customer_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  lookup_token_hash TEXT NOT NULL UNIQUE,
  status TEXT NOT NULL DEFAULT 'active',
  currency TEXT NOT NULL DEFAULT 'USD',
  expires_at TIMESTAMPTZ NOT NULL DEFAULT (now() + interval '30 days'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (status IN ('active','checkout_started','converted','expired','abandoned')),
  CHECK (currency ~ '^[A-Z]{3}$'),
  CHECK (expires_at > created_at)
);

CREATE TABLE IF NOT EXISTS merch_cart_item (
  cart_id UUID NOT NULL REFERENCES merch_cart(id) ON DELETE CASCADE,
  variant_id UUID NOT NULL REFERENCES merch_product_variant(id) ON DELETE RESTRICT,
  quantity INTEGER NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(cart_id, variant_id),
  CHECK (quantity BETWEEN 1 AND 100)
);

CREATE TABLE IF NOT EXISTS merch_order (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_number TEXT NOT NULL UNIQUE,
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  cart_id UUID REFERENCES merch_cart(id) ON DELETE RESTRICT,
  checkout_id UUID UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  customer_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  customer_email TEXT NOT NULL,
  customer_name TEXT NOT NULL,
  customer_phone TEXT,
  lookup_token_hash TEXT NOT NULL UNIQUE,
  currency TEXT NOT NULL DEFAULT 'USD',
  product_subtotal_minor BIGINT NOT NULL,
  discount_minor BIGINT NOT NULL DEFAULT 0,
  tax_minor BIGINT NOT NULL DEFAULT 0,
  shipping_minor BIGINT NOT NULL DEFAULT 0,
  processor_fee_minor BIGINT NOT NULL DEFAULT 0,
  tdf_commission_bps INTEGER NOT NULL,
  tdf_commission_minor BIGINT NOT NULL,
  seller_net_minor BIGINT NOT NULL,
  total_minor BIGINT NOT NULL,
  refunded_minor BIGINT NOT NULL DEFAULT 0,
  adjusted_minor BIGINT NOT NULL DEFAULT 0,
  commercial_status TEXT NOT NULL DEFAULT 'created',
  payment_status TEXT NOT NULL DEFAULT 'pending',
  fulfillment_status TEXT NOT NULL DEFAULT 'pending',
  refund_status TEXT NOT NULL DEFAULT 'none',
  dispute_status TEXT NOT NULL DEFAULT 'none',
  settlement_status TEXT NOT NULL DEFAULT 'not_ready',
  shipping_method TEXT NOT NULL,
  shipping_zone_snapshot JSONB NOT NULL,
  recipient_snapshot JSONB NOT NULL,
  policy_snapshot JSONB NOT NULL,
  commission_snapshot JSONB NOT NULL,
  create_idempotency_key TEXT NOT NULL,
  create_request_sha256 TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  confirmed_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  UNIQUE(store_id, create_idempotency_key),
  CHECK (order_number ~ '^TDF-MERCH-[A-Z0-9]{8,24}$'),
  CHECK (length(trim(customer_email)) BETWEEN 3 AND 320),
  CHECK (length(trim(customer_name)) BETWEEN 1 AND 200),
  CHECK (currency ~ '^[A-Z]{3}$'),
  CHECK (product_subtotal_minor > 0),
  CHECK (discount_minor >= 0 AND discount_minor <= product_subtotal_minor),
  CHECK (tax_minor >= 0 AND shipping_minor >= 0 AND processor_fee_minor >= 0),
  CHECK (tdf_commission_bps BETWEEN 0 AND 10000),
  CHECK (tdf_commission_minor = ((product_subtotal_minor - discount_minor) * tdf_commission_bps) / 10000),
  CHECK (total_minor = product_subtotal_minor - discount_minor + tax_minor + shipping_minor),
  CHECK (seller_net_minor = product_subtotal_minor - discount_minor + shipping_minor + tax_minor - tdf_commission_minor - processor_fee_minor),
  CHECK (seller_net_minor >= 0),
  CHECK (refunded_minor >= 0 AND refunded_minor <= total_minor),
  CHECK (jsonb_typeof(shipping_zone_snapshot) = 'object'),
  CHECK (jsonb_typeof(recipient_snapshot) = 'object'),
  CHECK (jsonb_typeof(policy_snapshot) = 'object'),
  CHECK (jsonb_typeof(commission_snapshot) = 'object'),
  CHECK (commercial_status IN ('created','confirmed','cancelled','completed')),
  CHECK (payment_status IN ('pending','processing','paid','partially_refunded','refunded','disputed','chargeback','failed','cancelled')),
  CHECK (fulfillment_status IN ('pending','preparing','ready_for_pickup','shipped','delivered','cancelled','return_requested','returned','problem')),
  CHECK (refund_status IN ('none','requested','approved','processing','partial','completed','rejected','cancelled')),
  CHECK (dispute_status IN ('none','inquiry','open','won','lost','chargeback')),
  CHECK (settlement_status IN ('not_ready','ready','under_review','approved','paid','held','adjusted','reversed')),
  CHECK (shipping_method IN ('coordinated_pickup','national_shipping')),
  CHECK (length(create_idempotency_key) BETWEEN 8 AND 200),
  CHECK (create_request_sha256 ~ '^[a-f0-9]{64}$')
);
CREATE INDEX IF NOT EXISTS merch_order_store_queue_idx
  ON merch_order(store_id, fulfillment_status, created_at DESC, id);
CREATE INDEX IF NOT EXISTS merch_order_customer_idx
  ON merch_order(customer_party_id, created_at DESC, id) WHERE customer_party_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS merch_order_line (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  line_number INTEGER NOT NULL,
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE RESTRICT,
  variant_id UUID NOT NULL REFERENCES merch_product_variant(id) ON DELETE RESTRICT,
  quantity INTEGER NOT NULL,
  unit_price_minor BIGINT NOT NULL,
  subtotal_minor BIGINT NOT NULL,
  discount_minor BIGINT NOT NULL DEFAULT 0,
  tax_minor BIGINT NOT NULL DEFAULT 0,
  total_minor BIGINT NOT NULL,
  product_snapshot JSONB NOT NULL,
  variant_snapshot JSONB NOT NULL,
  policy_snapshot JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(order_id, line_number),
  CHECK (line_number > 0 AND quantity > 0),
  CHECK (unit_price_minor > 0),
  CHECK (subtotal_minor = quantity::BIGINT * unit_price_minor),
  CHECK (discount_minor >= 0 AND discount_minor <= subtotal_minor AND tax_minor >= 0),
  CHECK (total_minor = subtotal_minor - discount_minor + tax_minor),
  CHECK (jsonb_typeof(product_snapshot) = 'object'),
  CHECK (jsonb_typeof(variant_snapshot) = 'object'),
  CHECK (jsonb_typeof(policy_snapshot) = 'object')
);

CREATE TABLE IF NOT EXISTS merch_inventory_reservation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  variant_id UUID NOT NULL REFERENCES merch_product_variant(id) ON DELETE RESTRICT,
  commerce_hold_id UUID NOT NULL UNIQUE REFERENCES commerce_reservation_hold(id) ON DELETE RESTRICT,
  quantity INTEGER NOT NULL CHECK (quantity > 0),
  status TEXT NOT NULL DEFAULT 'active',
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  released_at TIMESTAMPTZ,
  consumed_at TIMESTAMPTZ,
  UNIQUE(order_id, variant_id),
  CHECK (status IN ('active','consumed','released','expired')),
  CHECK (status <> 'consumed' OR consumed_at IS NOT NULL),
  CHECK (status NOT IN ('released','expired') OR released_at IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS merch_inventory_reservation_expiry_idx
  ON merch_inventory_reservation(expires_at, id) WHERE status = 'active';

CREATE TABLE IF NOT EXISTS merch_fulfillment_event (
  id BIGSERIAL PRIMARY KEY,
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL,
  from_status TEXT,
  to_status TEXT,
  actor_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  public_note TEXT,
  private_note TEXT,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (event_type IN ('order_created','payment_confirmed','preparation_started','pickup_ready','shipment_created','shipped','delivered','problem_reported','cancellation_requested','cancelled','return_requested','returned','refund_requested','refund_updated','dispute_updated')),
  CHECK (jsonb_typeof(metadata) = 'object')
);
CREATE INDEX IF NOT EXISTS merch_fulfillment_event_timeline_idx
  ON merch_fulfillment_event(order_id, created_at, id);

CREATE TABLE IF NOT EXISTS merch_shipment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  carrier TEXT,
  tracking_number TEXT,
  tracking_url TEXT,
  status TEXT NOT NULL DEFAULT 'pending',
  shipped_at TIMESTAMPTZ,
  delivered_at TIMESTAMPTZ,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (status IN ('pending','ready','shipped','delivered','returned','lost','cancelled')),
  CHECK (carrier IS NULL OR length(trim(carrier)) BETWEEN 1 AND 120),
  CHECK (tracking_number IS NULL OR length(trim(tracking_number)) BETWEEN 1 AND 180),
  CHECK (status <> 'shipped' OR shipped_at IS NOT NULL),
  CHECK (status <> 'delivered' OR delivered_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS merch_order_issue (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  opened_by_type TEXT NOT NULL,
  opened_by_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  issue_type TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'open',
  public_message TEXT NOT NULL,
  internal_notes TEXT,
  resolution TEXT,
  idempotency_key TEXT NOT NULL,
  request_sha256 TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  closed_at TIMESTAMPTZ,
  UNIQUE(order_id, idempotency_key),
  CHECK (opened_by_type IN ('buyer','seller','staff','system')),
  CHECK (issue_type IN ('general','address','stock','shipping','damaged','missing','cancellation','return','refund','dispute','fraud')),
  CHECK (status IN ('open','seller_review','staff_review','awaiting_buyer','resolved','rejected','cancelled')),
  CHECK (length(trim(public_message)) BETWEEN 10 AND 5000),
  CHECK (length(idempotency_key) BETWEEN 8 AND 200),
  CHECK (request_sha256 ~ '^[a-f0-9]{64}$'),
  CHECK (status NOT IN ('resolved','rejected','cancelled') OR closed_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS merch_settlement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  period_start TIMESTAMPTZ NOT NULL,
  period_end TIMESTAMPTZ NOT NULL,
  currency TEXT NOT NULL DEFAULT 'USD',
  gross_product_minor BIGINT NOT NULL,
  discounts_minor BIGINT NOT NULL,
  taxes_minor BIGINT NOT NULL,
  shipping_minor BIGINT NOT NULL,
  processor_fees_minor BIGINT NOT NULL,
  tdf_commission_minor BIGINT NOT NULL,
  refunds_minor BIGINT NOT NULL,
  adjustments_minor BIGINT NOT NULL,
  seller_net_minor BIGINT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft',
  evidence_object_key TEXT,
  review_notes TEXT,
  prepared_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  approved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  paid_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  approved_at TIMESTAMPTZ,
  paid_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (period_end > period_start),
  CHECK (currency ~ '^[A-Z]{3}$'),
  CHECK (gross_product_minor >= 0 AND discounts_minor >= 0 AND taxes_minor >= 0 AND shipping_minor >= 0 AND processor_fees_minor >= 0 AND tdf_commission_minor >= 0 AND refunds_minor >= 0),
  CHECK (status IN ('draft','under_review','approved','paid','held','reversed')),
  CHECK (status NOT IN ('approved','paid') OR (approved_by IS NOT NULL AND approved_at IS NOT NULL)),
  CHECK (status <> 'paid' OR (paid_by IS NOT NULL AND paid_at IS NOT NULL AND evidence_object_key IS NOT NULL)),
  CHECK (approved_by IS NULL OR approved_by <> prepared_by),
  CHECK (paid_by IS NULL OR paid_by <> prepared_by)
);

CREATE TABLE IF NOT EXISTS merch_settlement_order (
  settlement_id UUID NOT NULL REFERENCES merch_settlement(id) ON DELETE RESTRICT,
  order_id UUID NOT NULL UNIQUE REFERENCES merch_order(id) ON DELETE RESTRICT,
  seller_net_minor BIGINT NOT NULL,
  refund_minor BIGINT NOT NULL DEFAULT 0,
  adjustment_minor BIGINT NOT NULL DEFAULT 0,
  PRIMARY KEY(settlement_id, order_id)
);

CREATE TABLE IF NOT EXISTS merch_favorite (
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(party_id, product_id)
);

CREATE TABLE IF NOT EXISTS merch_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_line_id UUID NOT NULL UNIQUE REFERENCES merch_order_line(id) ON DELETE RESTRICT,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE RESTRICT,
  rating INTEGER NOT NULL,
  body TEXT,
  status TEXT NOT NULL DEFAULT 'pending',
  moderated_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  moderated_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (rating BETWEEN 1 AND 5),
  CHECK (body IS NULL OR length(body) BETWEEN 1 AND 3000),
  CHECK (status IN ('pending','published','hidden','rejected')),
  CHECK (status = 'pending' OR (moderated_by IS NOT NULL AND moderated_at IS NOT NULL))
);

CREATE TABLE IF NOT EXISTS merch_analytics_event (
  id BIGSERIAL PRIMARY KEY,
  event_name TEXT NOT NULL,
  store_id UUID REFERENCES merch_store(id) ON DELETE SET NULL,
  product_id UUID REFERENCES merch_product(id) ON DELETE SET NULL,
  order_id UUID REFERENCES merch_order(id) ON DELETE SET NULL,
  anonymous_session_hash TEXT,
  actor_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  properties JSONB NOT NULL DEFAULT '{}'::jsonb,
  consent_basis TEXT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (event_name IN ('store_application_submitted','store_activated','first_product_published','product_published','storefront_viewed','product_viewed','cart_item_added','checkout_started','checkout_completed','checkout_abandoned','order_completed','product_sold_out','refund_completed','dispute_opened','repeat_purchase','artist_followed','collaboration_requested')),
  CHECK (jsonb_typeof(properties) = 'object'),
  CHECK (consent_basis IN ('essential','consented_analytics')),
  CHECK (NOT (properties ?| ARRAY['email','phone','address','card','payment_token','tracking_number']))
);
CREATE INDEX IF NOT EXISTS merch_analytics_event_reporting_idx
  ON merch_analytics_event(event_name, occurred_at DESC, id);

CREATE TABLE IF NOT EXISTS merch_notification_outbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  recipient_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL,
  aggregate_type TEXT NOT NULL,
  aggregate_id UUID NOT NULL,
  locale TEXT NOT NULL DEFAULT 'es',
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  delivery_status TEXT NOT NULL DEFAULT 'pending',
  attempt_count INTEGER NOT NULL DEFAULT 0,
  available_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  processed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(recipient_party_id, event_type, aggregate_type, aggregate_id),
  CHECK (event_type IN ('store_application_received','store_approved','store_rejected','collaborator_invited','product_reviewed','order_received','payment_confirmed','pickup_ready','order_shipped','order_delivered','issue_updated','refund_updated','settlement_updated')),
  CHECK (aggregate_type IN ('store','member','product','order','issue','settlement')),
  CHECK (locale IN ('es','en')),
  CHECK (jsonb_typeof(payload) = 'object'),
  CHECK (delivery_status IN ('pending','processing','sent','failed','suppressed')),
  CHECK (attempt_count BETWEEN 0 AND 20)
);

CREATE TABLE IF NOT EXISTS merch_audit_event (
  id BIGSERIAL PRIMARY KEY,
  store_id UUID REFERENCES merch_store(id) ON DELETE RESTRICT,
  actor_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  actor_type TEXT NOT NULL,
  action TEXT NOT NULL,
  entity_type TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  correlation_id TEXT NOT NULL,
  before_state JSONB,
  after_state JSONB,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (actor_type IN ('buyer','seller','staff','system','provider')),
  CHECK (jsonb_typeof(metadata) = 'object'),
  CHECK (before_state IS NULL OR jsonb_typeof(before_state) = 'object'),
  CHECK (after_state IS NULL OR jsonb_typeof(after_state) = 'object')
);
CREATE INDEX IF NOT EXISTS merch_audit_event_store_idx
  ON merch_audit_event(store_id, created_at DESC, id);

CREATE OR REPLACE FUNCTION merch_reject_immutable_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% records are immutable; append a compensating record', TG_TABLE_NAME;
END $$;

DROP TRIGGER IF EXISTS merch_order_line_immutable_trigger ON merch_order_line;
CREATE TRIGGER merch_order_line_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_order_line
  FOR EACH ROW EXECUTE FUNCTION merch_reject_immutable_mutation();
DROP TRIGGER IF EXISTS merch_audit_event_immutable_trigger ON merch_audit_event;
CREATE TRIGGER merch_audit_event_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_audit_event
  FOR EACH ROW EXECUTE FUNCTION merch_reject_immutable_mutation();
DROP TRIGGER IF EXISTS merch_fulfillment_event_immutable_trigger ON merch_fulfillment_event;
CREATE TRIGGER merch_fulfillment_event_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_fulfillment_event
  FOR EACH ROW EXECUTE FUNCTION merch_reject_immutable_mutation();

CREATE OR REPLACE FUNCTION merch_validate_store_eligibility()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  profile_record directory_profile%ROWTYPE;
BEGIN
  SELECT * INTO profile_record FROM directory_profile WHERE id = NEW.directory_profile_id FOR SHARE;
  IF NOT FOUND OR profile_record.subject_party_id <> NEW.seller_party_id THEN
    RAISE EXCEPTION 'Merch store seller must match its directory profile subject';
  END IF;
  IF profile_record.profile_kind NOT IN ('artist','band','project')
     OR profile_record.profile_status <> 'published'
     OR profile_record.moderation_status <> 'allowed' THEN
    RAISE EXCEPTION 'Only published, allowed artist, band, or project profiles can request a merch store';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM directory_profile_manager manager
    WHERE manager.profile_id = NEW.directory_profile_id
      AND manager.account_party_id = NEW.primary_owner_party_id
      AND manager.active AND manager.can_manage
  ) THEN
    RAISE EXCEPTION 'Merch store primary owner must be an active profile manager';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM directory_profile_manager manager
    WHERE manager.profile_id = NEW.directory_profile_id
      AND manager.account_party_id = NEW.primary_owner_party_id
      AND manager.active AND manager.can_manage
      AND manager.source_claim_id IS NOT NULL
    UNION ALL
    SELECT 1 FROM directory_verification verification
    WHERE verification.profile_id = NEW.directory_profile_id
      AND verification.status = 'verified'
  ) THEN
    RAISE EXCEPTION 'Merch seller profile must be claimed or verified';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_store_eligibility_trigger ON merch_store;
CREATE TRIGGER merch_store_eligibility_trigger
  BEFORE INSERT OR UPDATE OF directory_profile_id, seller_party_id, primary_owner_party_id
  ON merch_store FOR EACH ROW EXECUTE FUNCTION merch_validate_store_eligibility();

CREATE OR REPLACE FUNCTION merch_ensure_owner_membership()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO merch_store_member(
    store_id, party_id, member_role, invitation_status,
    can_catalog, can_stock, can_orders, can_fulfillment, can_finance, can_settings,
    invited_by, accepted_at
  ) VALUES (
    NEW.id, NEW.primary_owner_party_id, 'owner', 'accepted',
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    NEW.primary_owner_party_id, now()
  )
  ON CONFLICT(store_id, party_id) DO UPDATE SET
    member_role = 'owner', invitation_status = 'accepted',
    can_catalog = TRUE, can_stock = TRUE, can_orders = TRUE,
    can_fulfillment = TRUE, can_finance = TRUE, can_settings = TRUE,
    accepted_at = coalesce(merch_store_member.accepted_at, now()),
    revoked_at = NULL, revoke_reason = NULL, updated_at = now();
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_store_owner_trigger ON merch_store;
CREATE TRIGGER merch_store_owner_trigger
  AFTER INSERT OR UPDATE OF primary_owner_party_id ON merch_store
  FOR EACH ROW EXECUTE FUNCTION merch_ensure_owner_membership();

CREATE OR REPLACE FUNCTION merch_validate_product_policy_store()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.policy_id IS NOT NULL AND NOT EXISTS (
    SELECT 1 FROM merch_store_policy policy
    WHERE policy.id = NEW.policy_id AND policy.store_id = NEW.store_id
  ) THEN
    RAISE EXCEPTION 'Merch product policy must belong to the same store';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_product_policy_store_trigger ON merch_product;
CREATE TRIGGER merch_product_policy_store_trigger
  BEFORE INSERT OR UPDATE OF store_id, policy_id ON merch_product
  FOR EACH ROW EXECUTE FUNCTION merch_validate_product_policy_store();

CREATE OR REPLACE FUNCTION merch_validate_variant_store_sku()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_store UUID;
BEGIN
  SELECT store_id INTO target_store FROM merch_product WHERE id = NEW.product_id;
  IF target_store IS NULL OR NEW.store_id <> target_store THEN
    RAISE EXCEPTION 'Merch variant store must match its product store';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_variant_store_sku_trigger ON merch_product_variant;
CREATE TRIGGER merch_variant_store_sku_trigger
  BEFORE INSERT OR UPDATE OF store_id, product_id, sku ON merch_product_variant
  FOR EACH ROW EXECUTE FUNCTION merch_validate_variant_store_sku();

CREATE OR REPLACE FUNCTION merch_validate_cart_item_store()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM merch_cart cart
    JOIN merch_product_variant variant ON variant.id = NEW.variant_id
    JOIN merch_product product ON product.id = variant.product_id
    WHERE cart.id = NEW.cart_id AND product.store_id = cart.store_id
      AND cart.status = 'active' AND cart.expires_at > now()
  ) THEN
    RAISE EXCEPTION 'A merch cart can contain products from only one active seller';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_cart_item_store_trigger ON merch_cart_item;
CREATE TRIGGER merch_cart_item_store_trigger
  BEFORE INSERT OR UPDATE OF cart_id, variant_id ON merch_cart_item
  FOR EACH ROW EXECUTE FUNCTION merch_validate_cart_item_store();

CREATE OR REPLACE FUNCTION merch_calculate_commission_bps(store_value UUID, at_value TIMESTAMPTZ DEFAULT now())
RETURNS INTEGER LANGUAGE sql STABLE AS $$
  SELECT coalesce(
    (SELECT policy.commission_bps FROM merch_commission_policy policy
      WHERE policy.store_id = store_value AND policy.effective_from <= at_value
        AND (policy.effective_until IS NULL OR policy.effective_until > at_value)
      ORDER BY policy.effective_from DESC, policy.id DESC LIMIT 1),
    (SELECT policy.commission_bps FROM merch_commission_policy policy
      WHERE policy.store_id IS NULL AND policy.effective_from <= at_value
        AND (policy.effective_until IS NULL OR policy.effective_until > at_value)
      ORDER BY policy.effective_from DESC, policy.id DESC LIMIT 1),
    1000
  )::INTEGER
$$;

CREATE OR REPLACE FUNCTION merch_reserve_stock(
  order_value UUID,
  checkout_value UUID,
  requested_items JSONB,
  expires_value TIMESTAMPTZ
)
RETURNS SETOF merch_inventory_reservation LANGUAGE plpgsql AS $$
DECLARE
  requested RECORD;
  variant_record merch_product_variant%ROWTYPE;
  order_store UUID;
  reservation_record merch_inventory_reservation%ROWTYPE;
  hold_value UUID;
BEGIN
  IF jsonb_typeof(requested_items) <> 'array' OR jsonb_array_length(requested_items) = 0 THEN
    RAISE EXCEPTION 'requested_items must be a non-empty JSON array';
  END IF;
  IF expires_value <= now() OR expires_value > now() + interval '2 hours' THEN
    RAISE EXCEPTION 'Merch reservation expiry must be within two hours';
  END IF;
  SELECT store_id INTO order_store FROM merch_order
    WHERE id = order_value AND checkout_id = checkout_value FOR UPDATE;
  IF NOT FOUND THEN RAISE EXCEPTION 'Order and checkout binding is invalid'; END IF;
  IF NOT EXISTS (
    SELECT 1 FROM commerce_checkout_session checkout
    WHERE checkout.id = checkout_value AND checkout.domain_type = 'merch_order'
      AND checkout.domain_order_id = order_value::text
      AND checkout.status IN ('holding','awaiting_payment')
      AND checkout.expires_at = expires_value
  ) THEN RAISE EXCEPTION 'Canonical merch checkout is not eligible for stock reservation'; END IF;

  FOR requested IN
    SELECT value->>'variantId' AS variant_id, (value->>'quantity')::INTEGER AS quantity
    FROM jsonb_array_elements(requested_items)
    ORDER BY value->>'variantId'
  LOOP
    IF requested.quantity IS NULL OR requested.quantity <= 0 OR requested.quantity > 100 THEN
      RAISE EXCEPTION 'Invalid merch reservation quantity';
    END IF;
    SELECT variant.* INTO variant_record
    FROM merch_product_variant variant
    JOIN merch_product product ON product.id = variant.product_id
    WHERE variant.id = requested.variant_id::UUID AND product.store_id = order_store
      AND variant.active AND product.status = 'published'
    FOR UPDATE OF variant;
    IF NOT FOUND THEN RAISE EXCEPTION 'Merch variant is not available'; END IF;
    IF variant_record.stock_mode = 'finite'
       AND variant_record.stock_on_hand - variant_record.stock_sold - variant_record.stock_reserved < requested.quantity THEN
      RAISE EXCEPTION 'Insufficient merch stock for variant %', variant_record.id;
    END IF;

    UPDATE merch_product_variant SET stock_reserved = stock_reserved + requested.quantity,
      updated_at = now(), version = version + 1 WHERE id = variant_record.id;
    INSERT INTO commerce_reservation_hold(checkout_id, resource_type, resource_id, quantity, status, expires_at)
      VALUES(checkout_value, 'merch_variant_reservation', gen_random_uuid()::text, requested.quantity, 'active', expires_value)
      RETURNING id INTO hold_value;
    INSERT INTO merch_inventory_reservation(order_id, checkout_id, variant_id, commerce_hold_id, quantity, expires_at)
      VALUES(order_value, checkout_value, variant_record.id, hold_value, requested.quantity, expires_value)
      RETURNING * INTO reservation_record;
    RETURN NEXT reservation_record;
  END LOOP;
END $$;

CREATE OR REPLACE FUNCTION merch_release_expired_reservations(at_value TIMESTAMPTZ DEFAULT now())
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE
  reservation RECORD;
  released_count INTEGER := 0;
BEGIN
  FOR reservation IN
    SELECT * FROM merch_inventory_reservation
    WHERE status = 'active' AND expires_at <= at_value
    ORDER BY variant_id, id FOR UPDATE SKIP LOCKED
  LOOP
    PERFORM 1 FROM merch_product_variant WHERE id = reservation.variant_id FOR UPDATE;
    UPDATE merch_product_variant
      SET stock_reserved = stock_reserved - reservation.quantity, updated_at = now(), version = version + 1
      WHERE id = reservation.variant_id AND stock_reserved >= reservation.quantity;
    IF NOT FOUND THEN RAISE EXCEPTION 'Merch reserved stock invariant is broken'; END IF;
    UPDATE merch_inventory_reservation SET status = 'expired', released_at = at_value
      WHERE id = reservation.id;
    UPDATE commerce_reservation_hold SET status = 'expired'
      WHERE id = reservation.commerce_hold_id AND status = 'active';
    released_count := released_count + 1;
  END LOOP;
  RETURN released_count;
END $$;

CREATE OR REPLACE FUNCTION merch_guard_paid_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'merch_order' AND NEW.status = 'paid' AND OLD.status <> 'paid' THEN
    IF NEW.paid_minor <> NEW.total_minor OR NOT EXISTS (
      SELECT 1 FROM commerce_payment_attempt attempt
      WHERE attempt.checkout_id = NEW.id AND attempt.status = 'succeeded'
        AND attempt.amount_minor = NEW.total_minor AND attempt.currency = NEW.currency
        AND attempt.environment = NEW.environment
    ) THEN
      RAISE EXCEPTION 'Merch checkout requires a verified successful server-side payment attempt';
    END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_guard_paid_checkout_trigger ON commerce_checkout_session;
CREATE TRIGGER merch_guard_paid_checkout_trigger
  BEFORE UPDATE OF status, paid_minor ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION merch_guard_paid_checkout();

CREATE OR REPLACE FUNCTION merch_apply_checkout_status()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  reservation RECORD;
BEGIN
  IF NEW.domain_type <> 'merch_order' OR NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    FOR reservation IN
      SELECT * FROM merch_inventory_reservation WHERE checkout_id = NEW.id AND status = 'active'
      ORDER BY variant_id, id FOR UPDATE
    LOOP
      PERFORM 1 FROM merch_product_variant WHERE id = reservation.variant_id FOR UPDATE;
      UPDATE merch_product_variant SET
        stock_reserved = stock_reserved - reservation.quantity,
        stock_sold = stock_sold + reservation.quantity,
        updated_at = now(), version = version + 1
      WHERE id = reservation.variant_id AND stock_reserved >= reservation.quantity;
      IF NOT FOUND THEN RAISE EXCEPTION 'Merch reserved stock invariant is broken'; END IF;
      UPDATE merch_inventory_reservation SET status = 'consumed', consumed_at = now() WHERE id = reservation.id;
      UPDATE commerce_reservation_hold SET status = 'consumed' WHERE id = reservation.commerce_hold_id AND status = 'active';
    END LOOP;
    UPDATE merch_order SET payment_status = 'paid', commercial_status = 'confirmed', confirmed_at = coalesce(confirmed_at, now()), updated_at = now()
      WHERE id::text = NEW.domain_order_id AND checkout_id = NEW.id;
  ELSIF NEW.status IN ('cancelled','expired','failed') THEN
    FOR reservation IN
      SELECT * FROM merch_inventory_reservation WHERE checkout_id = NEW.id AND status = 'active'
      ORDER BY variant_id, id FOR UPDATE
    LOOP
      PERFORM 1 FROM merch_product_variant WHERE id = reservation.variant_id FOR UPDATE;
      UPDATE merch_product_variant SET stock_reserved = stock_reserved - reservation.quantity,
        updated_at = now(), version = version + 1
        WHERE id = reservation.variant_id AND stock_reserved >= reservation.quantity;
      IF NOT FOUND THEN RAISE EXCEPTION 'Merch reserved stock invariant is broken'; END IF;
      UPDATE merch_inventory_reservation SET status = CASE WHEN NEW.status = 'expired' THEN 'expired' ELSE 'released' END,
        released_at = now() WHERE id = reservation.id;
      UPDATE commerce_reservation_hold SET status = CASE WHEN NEW.status = 'expired' THEN 'expired' ELSE 'released' END
        WHERE id = reservation.commerce_hold_id AND status = 'active';
    END LOOP;
    UPDATE merch_order SET payment_status = CASE WHEN NEW.status = 'cancelled' THEN 'cancelled' ELSE 'failed' END, updated_at = now()
      WHERE id::text = NEW.domain_order_id AND checkout_id = NEW.id AND payment_status NOT IN ('paid','partially_refunded','refunded');
  ELSIF NEW.status = 'partially_refunded' THEN
    UPDATE merch_order SET payment_status = 'partially_refunded', refund_status = 'partial', refunded_minor = NEW.refunded_minor, updated_at = now()
      WHERE id::text = NEW.domain_order_id AND checkout_id = NEW.id;
  ELSIF NEW.status = 'refunded' THEN
    UPDATE merch_order SET payment_status = 'refunded', refund_status = 'completed', refunded_minor = NEW.refunded_minor, updated_at = now()
      WHERE id::text = NEW.domain_order_id AND checkout_id = NEW.id;
  ELSIF NEW.status IN ('disputed','chargeback') THEN
    UPDATE merch_order SET payment_status = NEW.status, dispute_status = CASE WHEN NEW.status = 'chargeback' THEN 'chargeback' ELSE 'open' END, updated_at = now()
      WHERE id::text = NEW.domain_order_id AND checkout_id = NEW.id;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_apply_checkout_status_trigger ON commerce_checkout_session;
CREATE TRIGGER merch_apply_checkout_status_trigger
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION merch_apply_checkout_status();

CREATE OR REPLACE FUNCTION merch_protect_order_snapshots()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.store_id, OLD.currency, OLD.product_subtotal_minor, OLD.discount_minor, OLD.tax_minor,
      OLD.shipping_minor, OLD.tdf_commission_bps, OLD.tdf_commission_minor, OLD.total_minor,
      OLD.shipping_method, OLD.shipping_zone_snapshot, OLD.recipient_snapshot,
      OLD.policy_snapshot, OLD.commission_snapshot, OLD.create_idempotency_key, OLD.create_request_sha256)
     IS DISTINCT FROM
     (NEW.store_id, NEW.currency, NEW.product_subtotal_minor, NEW.discount_minor, NEW.tax_minor,
      NEW.shipping_minor, NEW.tdf_commission_bps, NEW.tdf_commission_minor, NEW.total_minor,
      NEW.shipping_method, NEW.shipping_zone_snapshot, NEW.recipient_snapshot,
      NEW.policy_snapshot, NEW.commission_snapshot, NEW.create_idempotency_key, NEW.create_request_sha256) THEN
    RAISE EXCEPTION 'Merch order commercial snapshots are immutable';
  END IF;
  IF NEW.processor_fee_minor < OLD.processor_fee_minor AND NEW.payment_status NOT IN ('refunded','chargeback') THEN
    RAISE EXCEPTION 'Processor fees can only be corrected through an audited adjustment';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_order_snapshot_immutable_trigger ON merch_order;
CREATE TRIGGER merch_order_snapshot_immutable_trigger
  BEFORE UPDATE ON merch_order FOR EACH ROW EXECUTE FUNCTION merch_protect_order_snapshots();

CREATE OR REPLACE FUNCTION merch_validate_review_purchase()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM merch_order_line line
    JOIN merch_order order_record ON order_record.id = line.order_id
    WHERE line.id = NEW.order_line_id AND line.product_id = NEW.product_id
      AND order_record.customer_party_id = NEW.author_party_id
      AND order_record.payment_status IN ('paid','partially_refunded')
      AND order_record.fulfillment_status = 'delivered'
  ) THEN RAISE EXCEPTION 'Only verified purchasers of delivered merch can review a product'; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_review_verified_purchase_trigger ON merch_review;
CREATE TRIGGER merch_review_verified_purchase_trigger
  BEFORE INSERT OR UPDATE OF order_line_id, author_party_id, product_id ON merch_review
  FOR EACH ROW EXECUTE FUNCTION merch_validate_review_purchase();

CREATE OR REPLACE VIEW merch_public_storefront AS
SELECT store.id, store.slug, store.display_name, store.description,
  store.cover_image_url, store.logo_image_url, store.country_code, store.currency,
  profile.id AS directory_profile_id, profile.slug AS profile_slug,
  profile.public_name AS profile_name
FROM merch_store store
JOIN directory_profile profile ON profile.id = store.directory_profile_id
WHERE store.application_status = 'approved' AND store.operational_status = 'active'
  AND profile.profile_status = 'published' AND profile.visibility = 'public'
  AND profile.moderation_status = 'allowed';

CREATE OR REPLACE VIEW merch_public_product AS
SELECT product.id, product.store_id, product.slug, product.name, product.description,
  product.category, product.status, product.availability_mode, product.preorder_release_at,
  product.buyer_limit, product.version, product.published_at,
  min(variant.price_minor) FILTER (WHERE variant.active) AS price_from_minor,
  min(variant.currency) FILTER (WHERE variant.active) AS currency,
  bool_or(variant.stock_mode = 'made_to_order' OR variant.stock_on_hand - variant.stock_sold - variant.stock_reserved > 0) FILTER (WHERE variant.active) AS available,
  (SELECT image.object_key FROM merch_product_image image
    WHERE image.product_id = product.id AND image.deleted_at IS NULL
      AND image.scan_status = 'clean' AND image.moderation_status = 'allowed'
    ORDER BY image.sort_order, image.id LIMIT 1) AS primary_image_object_key
FROM merch_product product
JOIN merch_public_storefront store ON store.id = product.store_id
JOIN merch_product_variant variant ON variant.product_id = product.id
WHERE product.status IN ('published','sold_out') AND product.visibility = 'public'
  AND (product.publish_at IS NULL OR product.publish_at <= now())
  AND (product.unpublish_at IS NULL OR product.unpublish_at > now())
GROUP BY product.id;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason)
SELECT flag_key, FALSE, environment, reason
FROM (VALUES
  ('merch.storefronts', 'Requires staged verification and operational approval'),
  ('merch.seller_applications', 'Enable only for invited claimed or verified artists'),
  ('merch.public_catalog', 'Requires approved pilot catalogs, policies, and moderation'),
  ('merch.checkout', 'Requires end-to-end sandbox checkout and operations verification'),
  ('merch.checkout.runtime_ready', 'Kill switch: enable only after a merch payment adapter, refunds, webhooks, and reconciliation pass staging'),
  ('merch.checkout.datafast', 'Requires Datafast merchant capability, signed callbacks, refunds, and reconciliation'),
  ('merch.checkout.paypal', 'Requires PayPal merchant capability, signed webhooks, refunds, and reconciliation'),
  ('merch.checkout.manual', 'Requires approved bank account, independent evidence review, and reconciliation'),
  ('merch.reviews', 'Requires verified-purchase moderation rollout'),
  ('merch.notifications', 'Requires opt-in templates, worker monitoring, and support readiness'),
  ('merch.experimental', 'Experimental merch functions remain disabled by default')
) AS flags(flag_key, reason)
CROSS JOIN (VALUES ('sandbox'),('staging'),('production')) AS environments(environment)
ON CONFLICT(flag_key, environment) DO NOTHING;

COMMENT ON TABLE merch_store IS 'Pilot-gated artist/band storefront attached to a claimed or verified public directory profile.';
COMMENT ON TABLE merch_product IS 'Merch-specific catalog; never represents studio assets, tickets, services, or digital downloads.';
COMMENT ON TABLE merch_order IS 'Immutable commercial snapshot. Payment, fulfillment, dispute, refund, and settlement states remain separate.';
COMMENT ON COLUMN merch_order.recipient_snapshot IS 'Operational delivery snapshot. Never expose from a public lookup response; redact from logs and exports.';
COMMENT ON TABLE merch_settlement IS 'Manual, dual-control seller settlement record. Automated payouts are intentionally out of scope.';
COMMENT ON TABLE merch_analytics_event IS 'Privacy-minimized product analytics. Payment credentials and recipient PII are forbidden.';

COMMIT;
