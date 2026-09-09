-- End-to-end verified commercial reputation for merchandise stores/products.
-- Additive, replay-safe, dark in staging/production, and without fake ratings.
\set ON_ERROR_STOP on

BEGIN;

DO $$
BEGIN
  IF to_regclass('public.party') IS NULL THEN
    RAISE EXCEPTION 'Cannot install merch reputation without canonical party';
  END IF;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_record_operational_signal(
  p_signal_key TEXT,
  p_store_id UUID,
  p_order_id UUID,
  p_metric TEXT,
  p_outcome NUMERIC,
  p_evidence_quality NUMERIC,
  p_responsibility TEXT,
  p_evidence_key TEXT,
  p_source_type TEXT,
  p_source_record_id TEXT,
  p_captured_by TEXT,
  p_evidence_payload JSONB,
  p_occurred_at TIMESTAMPTZ
) RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE existing_id UUID; evidence_id UUID; signal_id UUID; actual_store UUID;
  evidence_record RECORD;
BEGIN
  SELECT id INTO existing_id FROM merch_reputation_operational_signal
    WHERE signal_key=p_signal_key;
  IF FOUND THEN RETURN existing_id; END IF;
  IF p_captured_by NOT IN ('server','payment_provider','fulfillment_provider','moderator') THEN
    RAISE EXCEPTION 'Operational signals require a trusted server source';
  END IF;
  IF p_order_id IS NOT NULL THEN
    SELECT store_id INTO actual_store FROM merch_order WHERE id=p_order_id;
    IF actual_store IS DISTINCT FROM p_store_id THEN
      RAISE EXCEPTION 'Operational signal order belongs to another store';
    END IF;
  END IF;
  INSERT INTO merch_reputation_evidence(
    evidence_key,store_id,order_id,source_type,source_record_id,captured_by,payload,occurred_at
  ) VALUES (
    p_evidence_key,p_store_id,p_order_id,p_source_type,p_source_record_id,
    p_captured_by,p_evidence_payload,p_occurred_at
  ) ON CONFLICT (evidence_key) DO NOTHING
  RETURNING id INTO evidence_id;
  IF evidence_id IS NULL THEN
    SELECT id INTO evidence_id FROM merch_reputation_evidence WHERE evidence_key=p_evidence_key;
  END IF;
  SELECT * INTO evidence_record FROM merch_reputation_evidence WHERE id=evidence_id;
  IF evidence_record.store_id IS DISTINCT FROM p_store_id
    OR evidence_record.order_id IS DISTINCT FROM p_order_id
    OR evidence_record.source_type IS DISTINCT FROM p_source_type
    OR evidence_record.source_record_id IS DISTINCT FROM p_source_record_id
    OR evidence_record.captured_by IS DISTINCT FROM p_captured_by
    OR evidence_record.payload IS DISTINCT FROM p_evidence_payload
    OR evidence_record.occurred_at IS DISTINCT FROM p_occurred_at THEN
    RAISE EXCEPTION 'Evidence key cannot be reused for different operational evidence';
  END IF;
  INSERT INTO merch_reputation_operational_signal(
    signal_key,store_id,order_id,metric,outcome,evidence_quality,responsibility,evidence_id,occurred_at
  ) VALUES (
    p_signal_key,p_store_id,p_order_id,p_metric,p_outcome,p_evidence_quality,
    p_responsibility,evidence_id,p_occurred_at
  ) ON CONFLICT (signal_key) DO NOTHING
  RETURNING id INTO signal_id;
  IF signal_id IS NULL THEN
    SELECT id INTO signal_id FROM merch_reputation_operational_signal WHERE signal_key=p_signal_key;
    RETURN signal_id;
  END IF;
  INSERT INTO merch_reputation_event(
    event_key,subject_kind,store_id,event_type,operational_signal_id,payload,occurred_at
  ) VALUES (
    'signal:'||signal_id,'store',p_store_id,'operational_signal_recorded',
    signal_id,jsonb_build_object('metric',p_metric,'responsibility',p_responsibility),p_occurred_at
  );
  RETURN signal_id;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_rebuild_aggregate(
  p_subject_kind TEXT,
  p_subject_id UUID,
  p_through TIMESTAMPTZ DEFAULT NOW(),
  p_last_event_id UUID DEFAULT NULL
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  formula RECORD;
  params JSONB;
  target_store UUID;
  eligible_orders INTEGER;
  review_count INTEGER;
  primary_count INTEGER;
  weighted_rating NUMERIC;
  rating_weight NUMERIC;
  review_average NUMERIC;
  operational_sum NUMERIC;
  operational_weight_sum NUMERIC;
  operational_average NUMERIC;
  combined NUMERIC;
  public_score NUMERIC(2,1);
  publication TEXT;
  confidence_label TEXT;
  primary_days INTEGER;
  half_life NUMERIC;
  prior_mean NUMERIC;
  prior_strength NUMERIC;
  buyer_weight NUMERIC;
  ops_weight NUMERIC;
  calculation JSONB;
BEGIN
  IF p_subject_kind NOT IN ('store','product') THEN RAISE EXCEPTION 'Unknown reputation subject'; END IF;
  SELECT * INTO formula FROM merch_reputation_formula_version
    WHERE status='active' ORDER BY activated_at DESC LIMIT 1;
  IF NOT FOUND THEN RAISE EXCEPTION 'No active merch reputation formula'; END IF;
  params:=formula.parameters;
  half_life:=(params->>'halfLifeDays')::NUMERIC;
  prior_mean:=(params->>'priorMean')::NUMERIC;
  prior_strength:=(params->>'priorStrength')::NUMERIC;
  buyer_weight:=(params->>'reviewWeight')::NUMERIC;
  ops_weight:=(params->>'operationalWeight')::NUMERIC;
  primary_days:=(params->>'primaryPeriodDays')::INTEGER;
  IF p_subject_kind='store' THEN
    target_store:=p_subject_id;
    SELECT count(*) INTO eligible_orders FROM merch_order orders
      WHERE orders.store_id=target_store AND orders.fraud_state='clear'
        AND orders.verified_buyer_at IS NOT NULL
        AND NOT merch_reputation_accounts_related(orders.store_id,orders.buyer_party_id)
        AND ((orders.payment_state IN ('verified','partially_refunded','refunded','disputed','chargeback')
          AND orders.verified_payment_at IS NOT NULL
          AND ((orders.fulfillment_state='delivered' AND orders.delivered_at IS NOT NULL)
            OR (orders.fulfillment_state='picked_up' AND orders.pickup_confirmed_at IS NOT NULL)))
          OR (orders.order_state='cancelled' AND orders.cancelled_at IS NOT NULL));
  ELSE
    SELECT store_id INTO target_store FROM merch_product WHERE id=p_subject_id;
    IF target_store IS NULL THEN RAISE EXCEPTION 'Unknown product subject'; END IF;
    SELECT count(DISTINCT line.order_id) INTO eligible_orders
      FROM merch_order_line line JOIN merch_order orders ON orders.id=line.order_id
      WHERE line.product_id=p_subject_id AND orders.fraud_state='clear'
        AND orders.verified_buyer_at IS NOT NULL
        AND NOT merch_reputation_accounts_related(orders.store_id,orders.buyer_party_id)
        AND orders.payment_state IN ('verified','partially_refunded','refunded','disputed','chargeback')
        AND orders.verified_payment_at IS NOT NULL
        AND line.fulfillment_state IN ('delivered','picked_up','replaced','returned','refunded')
        AND line.delivered_at IS NOT NULL;
  END IF;

  SELECT count(*),
    count(*) FILTER (WHERE revision.submitted_at >= p_through-make_interval(days=>primary_days)),
    COALESCE(sum(revision.overall_rating *
      power(0.5,EXTRACT(EPOCH FROM (p_through-revision.submitted_at))/86400/half_life)),0),
    COALESCE(sum(power(0.5,
      EXTRACT(EPOCH FROM (p_through-revision.submitted_at))/86400/half_life)),0)
  INTO review_count,primary_count,weighted_rating,rating_weight
  FROM merch_review review
  JOIN merch_order reviewed_order ON reviewed_order.id=review.order_id
  JOIN merch_review_revision revision
    ON revision.review_id=review.id AND revision.revision_no=review.current_revision
  WHERE review.review_kind=p_subject_kind
    AND (CASE p_subject_kind WHEN 'store' THEN review.store_id ELSE review.product_id END)=p_subject_id
    AND review.status IN ('published','limited') AND reviewed_order.fraud_state='clear'
    AND revision.submitted_at<=p_through;
  review_average:=(weighted_rating+prior_mean*prior_strength)/(rating_weight+prior_strength);

  IF p_subject_kind='store' THEN
    SELECT
      sum((1+4*signal.outcome)*signal.evidence_quality *
        power(0.5,EXTRACT(EPOCH FROM (p_through-signal.occurred_at))/86400/half_life)),
      sum(signal.evidence_quality *
        power(0.5,EXTRACT(EPOCH FROM (p_through-signal.occurred_at))/86400/half_life))
    INTO operational_sum,operational_weight_sum
    FROM merch_reputation_operational_signal signal
    WHERE signal.store_id=target_store AND signal.responsibility='seller'
      AND (signal.order_id IS NULL OR EXISTS (SELECT 1 FROM merch_order signal_order
        WHERE signal_order.id=signal.order_id AND signal_order.fraud_state='clear'))
      AND signal.occurred_at<=p_through;
  END IF;
  operational_average:=CASE WHEN operational_weight_sum>0
    THEN operational_sum/operational_weight_sum ELSE NULL END;
  combined:=CASE WHEN operational_average IS NULL THEN review_average
    ELSE review_average*buyer_weight+operational_average*ops_weight END;
  IF p_subject_kind='store' AND eligible_orders<(params->>'minimumEvaluableOrders')::INTEGER THEN
    publication:='new_store'; public_score:=NULL; confidence_label:='new';
  ELSIF review_count=0 THEN
    publication:='unrated'; public_score:=NULL; confidence_label:='limited';
  ELSE
    publication:='published'; public_score:=round(combined,1);
    confidence_label:=CASE WHEN review_count<(params->>'limitedEvidenceReviewCount')::INTEGER THEN 'limited'
      WHEN review_count<(params->>'strongEvidenceReviewCount')::INTEGER THEN 'moderate' ELSE 'strong' END;
  END IF;
  calculation:=jsonb_build_object(
    'formulaVersion',formula.id,'purchaseValueWeighted',false,'bayesianPriorMean',prior_mean,
    'bayesianPriorStrength',prior_strength,'reviewWeight',buyer_weight,
    'operationalWeightApplied',CASE WHEN operational_average IS NULL THEN 0 ELSE ops_weight END,
    'sellerSignalsOnly',true,'halfLifeDays',half_life,'primaryPeriodDays',primary_days
  );
  INSERT INTO merch_reputation_aggregate(
    subject_kind,subject_id,store_id,formula_version_id,publication_state,public_rating,
    review_average,operational_average,verified_review_count,evaluable_order_count,
    primary_period_review_count,historical_review_count,confidence,primary_period_start,
    calculated_through,last_event_id,calculation
  ) VALUES (
    p_subject_kind,p_subject_id,target_store,formula.id,publication,public_score,
    review_average,operational_average,review_count,eligible_orders,primary_count,review_count,
    confidence_label,p_through-make_interval(days=>primary_days),p_through,p_last_event_id,calculation
  ) ON CONFLICT (subject_kind,subject_id,formula_version_id) DO UPDATE SET
    store_id=EXCLUDED.store_id,publication_state=EXCLUDED.publication_state,
    public_rating=EXCLUDED.public_rating,review_average=EXCLUDED.review_average,
    operational_average=EXCLUDED.operational_average,
    verified_review_count=EXCLUDED.verified_review_count,
    evaluable_order_count=EXCLUDED.evaluable_order_count,
    primary_period_review_count=EXCLUDED.primary_period_review_count,
    historical_review_count=EXCLUDED.historical_review_count,confidence=EXCLUDED.confidence,
    primary_period_start=EXCLUDED.primary_period_start,
    calculated_through=EXCLUDED.calculated_through,last_event_id=EXCLUDED.last_event_id,
    calculation=EXCLUDED.calculation,updated_at=NOW();
  DELETE FROM merch_reputation_dimension_aggregate
    WHERE subject_kind=p_subject_kind AND subject_id=p_subject_id
      AND formula_version_id=formula.id;
  INSERT INTO merch_reputation_dimension_aggregate(
    subject_kind,subject_id,dimension_code,formula_version_id,
    public_average,verified_review_count,distribution
  )
  SELECT p_subject_kind,p_subject_id,rating.dimension_code,formula.id,
    round((sum(rating.rating*power(0.5,
      EXTRACT(EPOCH FROM (p_through-revision.submitted_at))/86400/half_life))
      /NULLIF(sum(power(0.5,
        EXTRACT(EPOCH FROM (p_through-revision.submitted_at))/86400/half_life)),0))::NUMERIC,1),
    count(*)::INTEGER,
    jsonb_build_object(
      '1',count(*) FILTER (WHERE rating.rating=1),
      '2',count(*) FILTER (WHERE rating.rating=2),
      '3',count(*) FILTER (WHERE rating.rating=3),
      '4',count(*) FILTER (WHERE rating.rating=4),
      '5',count(*) FILTER (WHERE rating.rating=5)
    )
  FROM merch_review review
  JOIN merch_order reviewed_order ON reviewed_order.id=review.order_id
  JOIN merch_review_revision revision
    ON revision.review_id=review.id AND revision.revision_no=review.current_revision
  JOIN merch_review_dimension_rating rating ON rating.revision_id=revision.id
  WHERE review.review_kind=p_subject_kind
    AND (CASE p_subject_kind WHEN 'store' THEN review.store_id ELSE review.product_id END)=p_subject_id
    AND review.status IN ('published','limited') AND reviewed_order.fraud_state='clear'
    AND revision.submitted_at<=p_through
  GROUP BY rating.dimension_code;
  RETURN jsonb_build_object(
    'subjectKind',p_subject_kind,'subjectId',p_subject_id,'storeId',target_store,
    'state',publication,'rating',public_score,'verifiedReviews',review_count,
    'eligibleOrders',eligible_orders,'confidence',confidence_label,'formulaVersion',formula.id
  );
END $$;

CREATE TABLE IF NOT EXISTS merch_store (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  artist_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  slug TEXT NOT NULL CHECK (slug = lower(slug) AND slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'),
  name TEXT NOT NULL CHECK (length(btrim(name)) BETWEEN 2 AND 120),
  status TEXT NOT NULL DEFAULT 'draft'
    CHECK (status IN ('draft','published','paused','archived')),
  identity_verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (slug)
);

CREATE TABLE IF NOT EXISTS merch_store_member (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  member_role TEXT NOT NULL CHECK (member_role IN ('owner','admin','collaborator')),
  status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','suspended','removed')),
  added_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  removed_at TIMESTAMPTZ,
  UNIQUE (store_id, party_id)
);

CREATE INDEX IF NOT EXISTS merch_store_member_party_idx
  ON merch_store_member(party_id, store_id) WHERE status='active';
CREATE INDEX IF NOT EXISTS merch_store_artist_idx
  ON merch_store(artist_party_id, created_at) WHERE status='published';

CREATE OR REPLACE FUNCTION merch_store_sync_owner_membership()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO merch_store_member(store_id, party_id, member_role, status)
  VALUES (NEW.id, NEW.owner_party_id, 'owner', 'active')
  ON CONFLICT (store_id, party_id) DO UPDATE
    SET member_role='owner', status='active', removed_at=NULL;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_store_sync_owner_membership_trigger ON merch_store;
CREATE TRIGGER merch_store_sync_owner_membership_trigger
  AFTER INSERT OR UPDATE OF owner_party_id ON merch_store
  FOR EACH ROW EXECUTE FUNCTION merch_store_sync_owner_membership();

CREATE TABLE IF NOT EXISTS merch_product (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  slug TEXT NOT NULL CHECK (slug = lower(slug) AND slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'),
  name TEXT NOT NULL CHECK (length(btrim(name)) BETWEEN 2 AND 160),
  status TEXT NOT NULL DEFAULT 'draft'
    CHECK (status IN ('draft','published','sold_out','archived')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (store_id, slug),
  UNIQUE (id, store_id)
);

CREATE TABLE IF NOT EXISTS merch_order (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  buyer_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  order_state TEXT NOT NULL DEFAULT 'pending'
    CHECK (order_state IN ('pending','confirmed','preparing','partially_fulfilled','fulfilled','cancelled','closed')),
  payment_state TEXT NOT NULL DEFAULT 'awaiting_payment'
    CHECK (payment_state IN ('awaiting_payment','verified','partially_refunded','refunded','disputed','chargeback')),
  fulfillment_state TEXT NOT NULL DEFAULT 'pending'
    CHECK (fulfillment_state IN ('pending','preparing','partially_delivered','delivered','picked_up','cancelled')),
  verified_buyer_at TIMESTAMPTZ,
  verified_payment_at TIMESTAMPTZ,
  delivered_at TIMESTAMPTZ,
  pickup_confirmed_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  cancellation_resolved_at TIMESTAMPTZ,
  fraud_state TEXT NOT NULL DEFAULT 'clear' CHECK (fraud_state IN ('clear','review','confirmed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (id, store_id),
  CHECK ((fulfillment_state <> 'delivered') OR delivered_at IS NOT NULL),
  CHECK ((fulfillment_state <> 'picked_up') OR pickup_confirmed_at IS NOT NULL),
  CHECK ((order_state <> 'cancelled') OR cancelled_at IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS merch_order_buyer_idx ON merch_order(buyer_party_id, updated_at DESC);
CREATE INDEX IF NOT EXISTS merch_order_store_reputation_idx
  ON merch_order(store_id, fulfillment_state, delivered_at DESC);

CREATE TABLE IF NOT EXISTS merch_order_line (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  product_id UUID NOT NULL REFERENCES merch_product(id) ON DELETE RESTRICT,
  store_id UUID NOT NULL,
  quantity INTEGER NOT NULL CHECK (quantity BETWEEN 1 AND 100),
  variant_snapshot JSONB NOT NULL DEFAULT '{}'::jsonb CHECK (jsonb_typeof(variant_snapshot)='object'),
  fulfillment_state TEXT NOT NULL DEFAULT 'pending'
    CHECK (fulfillment_state IN ('pending','partially_delivered','delivered','picked_up','replaced','returned','refunded','cancelled')),
  delivered_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (id, order_id),
  FOREIGN KEY (order_id, store_id) REFERENCES merch_order(id, store_id) ON DELETE RESTRICT,
  FOREIGN KEY (product_id, store_id) REFERENCES merch_product(id, store_id) ON DELETE RESTRICT,
  CHECK ((fulfillment_state NOT IN ('delivered','picked_up','replaced')) OR delivered_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS merch_reputation_feature_flag (
  flag_key TEXT NOT NULL CHECK (flag_key IN (
    'store_reviews','product_reviews','seller_responses','review_images','badges',
    'search_influence','comparison_cards','moderation','notifications'
  )),
  environment TEXT NOT NULL CHECK (environment IN ('development','staging','production')),
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  version INTEGER NOT NULL DEFAULT 1 CHECK (version > 0),
  reason TEXT NOT NULL CHECK (length(btrim(reason)) BETWEEN 8 AND 500),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  PRIMARY KEY (flag_key, environment)
);

INSERT INTO merch_reputation_feature_flag(flag_key, environment, enabled, reason)
SELECT flag_key, environment, environment='development',
       CASE WHEN environment='development' THEN 'Local synthetic-data rollout only'
       ELSE 'Disabled pending the documented staged rollout gates' END
FROM unnest(ARRAY[
  'store_reviews','product_reviews','seller_responses','review_images','badges',
  'search_influence','comparison_cards','moderation','notifications'
]) AS flag(flag_key)
CROSS JOIN unnest(ARRAY['development','staging','production']) AS env(environment)
ON CONFLICT (flag_key, environment) DO NOTHING;

CREATE TABLE IF NOT EXISTS merch_reputation_formula_version (
  id TEXT PRIMARY KEY,
  status TEXT NOT NULL CHECK (status IN ('draft','active','retired')),
  parameters JSONB NOT NULL CHECK (jsonb_typeof(parameters)='object'),
  explanation_es TEXT NOT NULL,
  explanation_en TEXT NOT NULL,
  approved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  approved_at TIMESTAMPTZ,
  activated_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((status <> 'active') OR activated_at IS NOT NULL)
);

INSERT INTO merch_reputation_formula_version(
  id,status,parameters,explanation_es,explanation_en,activated_at
) VALUES (
  'merch-commercial-bayes-v1','active',
  '{"priorMean":3.5,"priorStrength":5,"reviewWeight":0.85,"operationalWeight":0.15,"halfLifeDays":730,"minimumEvaluableOrders":5,"rankingContributionCap":0.12,"primaryPeriodDays":365,"limitedEvidenceReviewCount":10,"strongEvidenceReviewCount":30}'::jsonb,
  'Las compras verificadas dominan la calificación. Aplicamos un promedio bayesiano para muestras pequeñas, decaimiento moderado y hasta 15 % de señales operativas atribuibles al vendedor. El precio de la compra no cambia su peso.',
  'Verified purchases dominate the rating. We use a Bayesian average for small samples, moderate time decay, and at most 15% seller-attributable operational evidence. Purchase value never changes weight.',
  NOW()
) ON CONFLICT (id) DO NOTHING;

CREATE OR REPLACE FUNCTION merch_reputation_formula_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='DELETE' THEN RAISE EXCEPTION 'Reputation formulas are durable'; END IF;
  IF OLD.status IN ('active','retired') AND ROW(NEW.id,NEW.parameters,NEW.activated_at)
      IS DISTINCT FROM ROW(OLD.id,OLD.parameters,OLD.activated_at) THEN
    RAISE EXCEPTION 'Activated merch reputation formulas are immutable';
  END IF;
  IF OLD.status='active' AND NEW.status='draft' THEN
    RAISE EXCEPTION 'An active merch reputation formula cannot return to draft';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_reputation_formula_immutable_trigger ON merch_reputation_formula_version;
CREATE TRIGGER merch_reputation_formula_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_formula_version
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_formula_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_dimension (
  code TEXT PRIMARY KEY,
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  definition_es TEXT NOT NULL,
  definition_en TEXT NOT NULL,
  governed BOOLEAN NOT NULL DEFAULT TRUE,
  status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('proposed','testing','active','retired')),
  minimum_sample INTEGER NOT NULL DEFAULT 5 CHECK (minimum_sample >= 0),
  formula_version_id TEXT REFERENCES merch_reputation_formula_version(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

INSERT INTO merch_reputation_dimension(
  code,subject_kind,name_es,name_en,definition_es,definition_en,minimum_sample,formula_version_id
) VALUES
  ('description_accuracy','product','Conforme a la descripción','As described','Qué tan fiel fue el artículo recibido a la descripción publicada.','How closely the received item matched its listing.',5,'merch-commercial-bayes-v1'),
  ('product_quality','product','Calidad del producto','Product quality','Calidad material y de fabricación del artículo recibido.','Material and manufacturing quality of the received item.',5,'merch-commercial-bayes-v1'),
  ('preparation_dispatch','store','Preparación y despacho','Preparation and dispatch','Rapidez de preparación y entrega inicial al courier o disponibilidad para retiro.','Speed of preparation and initial courier handoff or pickup readiness.',5,'merch-commercial-bayes-v1'),
  ('communication','store','Comunicación','Communication','Claridad y oportunidad de la comunicación comercial.','Clarity and timeliness of commercial communication.',5,'merch-commercial-bayes-v1'),
  ('packaging','store','Empaque','Packaging','Protección, presentación y adecuación del empaque.','Protection, presentation and suitability of packaging.',5,'merch-commercial-bayes-v1'),
  ('problem_resolution','store','Resolución de problemas','Problem resolution','Calidad y oportunidad de la solución cuando existió un problema.','Quality and timeliness of resolution when a problem occurred.',5,'merch-commercial-bayes-v1')
ON CONFLICT (code) DO NOTHING;

CREATE TABLE IF NOT EXISTS merch_reputation_category_suggestion (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  suggested_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  label TEXT NOT NULL CHECK (length(btrim(label)) BETWEEN 3 AND 80),
  definition TEXT NOT NULL CHECK (length(btrim(definition)) BETWEEN 20 AND 500),
  normalized_key TEXT NOT NULL CHECK (length(normalized_key) BETWEEN 3 AND 80),
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','duplicate','testing','approved','rejected')),
  minimum_sample INTEGER CHECK (minimum_sample IS NULL OR minimum_sample >= 5),
  bias_test JSONB,
  utility_test JSONB,
  decided_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  decided_at TIMESTAMPTZ,
  decision_reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (subject_kind, normalized_key),
  CHECK (status<>'approved' OR (
    minimum_sample IS NOT NULL
    AND jsonb_typeof(bias_test)='object' AND bias_test @> '{"passed":true}'::jsonb
    AND jsonb_typeof(utility_test)='object' AND utility_test @> '{"passed":true}'::jsonb
    AND decided_by IS NOT NULL AND decided_at IS NOT NULL
    AND decision_reason IS NOT NULL AND length(btrim(decision_reason))>=20
  ))
);
ALTER TABLE merch_reputation_category_suggestion
  ADD COLUMN IF NOT EXISTS minimum_sample INTEGER
  CHECK (minimum_sample IS NULL OR minimum_sample >= 5);

CREATE TABLE IF NOT EXISTS merch_review_privacy_preference (
  party_id BIGINT PRIMARY KEY REFERENCES party(id) ON DELETE RESTRICT,
  show_public_identity BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_reputation_priority_profile (
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  current_revision INTEGER NOT NULL DEFAULT 0 CHECK (current_revision >= 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (party_id, subject_kind)
);

CREATE TABLE IF NOT EXISTS merch_reputation_priority_revision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  party_id BIGINT NOT NULL,
  subject_kind TEXT NOT NULL,
  revision_no INTEGER NOT NULL CHECK (revision_no > 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  FOREIGN KEY (party_id, subject_kind)
    REFERENCES merch_reputation_priority_profile(party_id, subject_kind) ON DELETE RESTRICT,
  UNIQUE (party_id, subject_kind, revision_no)
);

CREATE TABLE IF NOT EXISTS merch_reputation_priority_item (
  revision_id UUID NOT NULL REFERENCES merch_reputation_priority_revision(id) ON DELETE RESTRICT,
  dimension_code TEXT NOT NULL REFERENCES merch_reputation_dimension(code) ON DELETE RESTRICT,
  position SMALLINT NOT NULL CHECK (position BETWEEN 1 AND 20),
  PRIMARY KEY (revision_id, dimension_code),
  UNIQUE (revision_id, position)
);

CREATE TABLE IF NOT EXISTS merch_review_media_asset (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  uploaded_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  storage_key TEXT NOT NULL UNIQUE CHECK (
    length(btrim(storage_key)) BETWEEN 8 AND 500
    AND storage_key ~ '^[A-Za-z0-9][A-Za-z0-9/_.-]+$'
    AND storage_key !~ '(^|/)\.\.(/|$)'
  ),
  content_type TEXT NOT NULL CHECK (content_type IN ('image/jpeg','image/png','image/webp')),
  byte_size BIGINT NOT NULL CHECK (byte_size BETWEEN 1 AND 10485760),
  sha256 TEXT NOT NULL CHECK (sha256 ~ '^[a-f0-9]{64}$'),
  width INTEGER NOT NULL CHECK (width BETWEEN 1 AND 10000),
  height INTEGER NOT NULL CHECK (height BETWEEN 1 AND 10000),
  scan_status TEXT NOT NULL DEFAULT 'pending' CHECK (scan_status IN ('pending','safe','unsafe','failed')),
  moderation_status TEXT NOT NULL DEFAULT 'pending' CHECK (moderation_status IN ('pending','published','hidden','rejected')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  review_kind TEXT NOT NULL CHECK (review_kind IN ('store','product')),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  product_id UUID REFERENCES merch_product(id) ON DELETE RESTRICT,
  order_id UUID NOT NULL REFERENCES merch_order(id) ON DELETE RESTRICT,
  order_line_id UUID REFERENCES merch_order_line(id) ON DELETE RESTRICT,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'published' CHECK (status IN ('published','hidden','limited','removed')),
  current_revision INTEGER NOT NULL DEFAULT 0 CHECK (current_revision >= 0),
  edit_deadline TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((review_kind='store' AND product_id IS NULL AND order_line_id IS NULL)
    OR (review_kind='product' AND product_id IS NOT NULL AND order_line_id IS NOT NULL))
);

CREATE UNIQUE INDEX IF NOT EXISTS merch_review_one_store_per_order
  ON merch_review(order_id) WHERE review_kind='store';
CREATE UNIQUE INDEX IF NOT EXISTS merch_review_one_product_per_line
  ON merch_review(order_line_id) WHERE review_kind='product';
CREATE INDEX IF NOT EXISTS merch_review_store_public_idx
  ON merch_review(store_id, created_at DESC, id DESC) WHERE status IN ('published','limited');
CREATE INDEX IF NOT EXISTS merch_review_product_public_idx
  ON merch_review(product_id, created_at DESC, id DESC) WHERE status IN ('published','limited');

CREATE TABLE IF NOT EXISTS merch_review_revision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  review_id UUID NOT NULL REFERENCES merch_review(id) ON DELETE RESTRICT,
  revision_no INTEGER NOT NULL CHECK (revision_no > 0),
  overall_rating SMALLINT NOT NULL CHECK (overall_rating BETWEEN 1 AND 5),
  issue_occurred BOOLEAN NOT NULL DEFAULT FALSE,
  comment TEXT CHECK (comment IS NULL OR (
    length(btrim(comment)) BETWEEN 10 AND 3000
    AND comment !~ '[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]')),
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (review_id, revision_no)
);

CREATE TABLE IF NOT EXISTS merch_review_dimension_rating (
  revision_id UUID NOT NULL REFERENCES merch_review_revision(id) ON DELETE RESTRICT,
  dimension_code TEXT NOT NULL REFERENCES merch_reputation_dimension(code) ON DELETE RESTRICT,
  rating SMALLINT NOT NULL CHECK (rating BETWEEN 1 AND 5),
  PRIMARY KEY (revision_id, dimension_code)
);

CREATE TABLE IF NOT EXISTS merch_review_image (
  revision_id UUID NOT NULL REFERENCES merch_review_revision(id) ON DELETE RESTRICT,
  media_asset_id UUID NOT NULL REFERENCES merch_review_media_asset(id) ON DELETE RESTRICT,
  alt_text TEXT NOT NULL CHECK (length(btrim(alt_text)) BETWEEN 3 AND 300),
  position SMALLINT NOT NULL CHECK (position BETWEEN 1 AND 4),
  PRIMARY KEY (revision_id, media_asset_id),
  UNIQUE (revision_id, position)
);

CREATE OR REPLACE FUNCTION merch_reputation_accounts_related(UUID,BIGINT)
RETURNS BOOLEAN LANGUAGE plpgsql STABLE AS $$
DECLARE artist_party BIGINT;
BEGIN
  IF EXISTS (SELECT 1 FROM merch_store_member
    WHERE store_id=$1 AND party_id=$2 AND status='active') THEN RETURN TRUE; END IF;
  SELECT artist_party_id INTO artist_party FROM merch_store WHERE id=$1;
  IF artist_party IS NULL THEN RETURN FALSE; END IF;
  IF artist_party=$2 THEN RETURN TRUE; END IF;
  IF to_regclass('public.band') IS NOT NULL AND to_regclass('public.band_member') IS NOT NULL THEN
    RETURN EXISTS (SELECT 1 FROM band JOIN band_member ON band_member.band_id=band.id
      WHERE band.party_id=artist_party AND band_member.party_id=$2);
  END IF;
  RETURN FALSE;
END $$;

CREATE OR REPLACE FUNCTION merch_review_evidence_is_eligible(TEXT,UUID,BIGINT,TIMESTAMPTZ DEFAULT NOW())
RETURNS BOOLEAN LANGUAGE plpgsql STABLE AS $$
DECLARE source_time TIMESTAMPTZ; source_store UUID; source_buyer BIGINT;
  source_fraud TEXT; source_payment TEXT; source_fulfillment TEXT; source_order_state TEXT;
  source_verified_buyer TIMESTAMPTZ; source_verified_payment TIMESTAMPTZ;
BEGIN
  IF $1='store' THEN
    SELECT COALESCE(delivered_at,pickup_confirmed_at,cancellation_resolved_at,cancelled_at),
      store_id,buyer_party_id,fraud_state,payment_state,fulfillment_state,order_state,
      verified_buyer_at,verified_payment_at
    INTO source_time,source_store,source_buyer,source_fraud,source_payment,source_fulfillment,
      source_order_state,source_verified_buyer,source_verified_payment
    FROM merch_order WHERE id=$2;
  ELSIF $1='product' THEN
    SELECT line.delivered_at,orders.store_id,orders.buyer_party_id,orders.fraud_state,
      orders.payment_state,line.fulfillment_state,orders.order_state,
      orders.verified_buyer_at,orders.verified_payment_at
    INTO source_time,source_store,source_buyer,source_fraud,source_payment,source_fulfillment,
      source_order_state,source_verified_buyer,source_verified_payment
    FROM merch_order_line line JOIN merch_order orders ON orders.id=line.order_id WHERE line.id=$2;
  ELSE RETURN FALSE;
  END IF;
  IF source_time IS NULL OR source_verified_buyer IS NULL
    OR source_buyer IS DISTINCT FROM $3 OR source_fraud<>'clear'
    OR merch_reputation_accounts_related(source_store,$3) OR $4<source_time
    OR $4>source_time+INTERVAL '30 days' THEN RETURN FALSE; END IF;
  IF $1='product' THEN
    RETURN source_payment IN ('verified','partially_refunded','refunded','disputed','chargeback')
      AND source_verified_payment IS NOT NULL
      AND source_fulfillment IN ('delivered','picked_up','replaced','returned','refunded');
  END IF;
  RETURN (source_payment IN ('verified','partially_refunded','refunded','disputed','chargeback')
    AND source_verified_payment IS NOT NULL
    AND source_fulfillment IN ('delivered','picked_up')) OR source_order_state='cancelled';
END $$;

CREATE OR REPLACE FUNCTION merch_review_validate_identity()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE expected_store UUID; expected_product UUID; expected_order UUID;
BEGIN
  IF TG_OP='UPDATE' THEN
    IF ROW(NEW.review_kind,NEW.store_id,NEW.product_id,NEW.order_id,NEW.order_line_id,
      NEW.author_party_id,NEW.edit_deadline,NEW.created_at) IS DISTINCT FROM
      ROW(OLD.review_kind,OLD.store_id,OLD.product_id,OLD.order_id,OLD.order_line_id,
      OLD.author_party_id,OLD.edit_deadline,OLD.created_at) THEN
      RAISE EXCEPTION 'Review identity and eligibility evidence are immutable';
    END IF;
    IF NEW.current_revision<OLD.current_revision THEN RAISE EXCEPTION 'Review revisions cannot move backwards'; END IF;
    NEW.updated_at:=NOW(); RETURN NEW;
  END IF;
  IF NEW.review_kind='store' THEN
    SELECT store_id INTO expected_store FROM merch_order WHERE id=NEW.order_id;
    IF expected_store IS DISTINCT FROM NEW.store_id
      OR NOT merch_review_evidence_is_eligible('store',NEW.order_id,NEW.author_party_id) THEN
      RAISE EXCEPTION 'Store review requires an eligible coherent order'; END IF;
  ELSE
    SELECT store_id,product_id,order_id INTO expected_store,expected_product,expected_order
    FROM merch_order_line WHERE id=NEW.order_line_id;
    IF ROW(expected_store,expected_product,expected_order) IS DISTINCT FROM
      ROW(NEW.store_id,NEW.product_id,NEW.order_id)
      OR NOT merch_review_evidence_is_eligible('product',NEW.order_line_id,NEW.author_party_id) THEN
      RAISE EXCEPTION 'Product review requires an eligible coherent delivered line'; END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_review_validate_identity_trigger ON merch_review;
CREATE TRIGGER merch_review_validate_identity_trigger BEFORE INSERT OR UPDATE ON merch_review
FOR EACH ROW EXECUTE FUNCTION merch_review_validate_identity();

CREATE OR REPLACE FUNCTION merch_reputation_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'Durable reputation evidence is immutable'; END $$;
DROP TRIGGER IF EXISTS merch_review_revision_immutable_trigger ON merch_review_revision;
CREATE TRIGGER merch_review_revision_immutable_trigger BEFORE UPDATE OR DELETE ON merch_review_revision
FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();
DROP TRIGGER IF EXISTS merch_review_dimension_immutable_trigger ON merch_review_dimension_rating;
CREATE TRIGGER merch_review_dimension_immutable_trigger BEFORE UPDATE OR DELETE ON merch_review_dimension_rating
FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();
DROP TRIGGER IF EXISTS merch_review_image_immutable_trigger ON merch_review_image;
CREATE TRIGGER merch_review_image_immutable_trigger BEFORE UPDATE OR DELETE ON merch_review_image
FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();
DROP TRIGGER IF EXISTS merch_reputation_priority_revision_immutable_trigger
  ON merch_reputation_priority_revision;
CREATE TRIGGER merch_reputation_priority_revision_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_priority_revision
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();
DROP TRIGGER IF EXISTS merch_reputation_priority_item_immutable_trigger
  ON merch_reputation_priority_item;
CREATE TRIGGER merch_reputation_priority_item_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_priority_item
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE OR REPLACE FUNCTION merch_review_validate_revision_dimensions()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE kind TEXT; cancelled BOOLEAN; issue_present BOOLEAN; invalid_count BIGINT; required_count BIGINT;
BEGIN
  SELECT review.review_kind,(orders.order_state='cancelled'),revision.issue_occurred
  INTO kind,cancelled,issue_present FROM merch_review_revision revision
  JOIN merch_review review ON review.id=revision.review_id
  JOIN merch_order orders ON orders.id=review.order_id WHERE revision.id=NEW.id;
  SELECT count(*) INTO invalid_count FROM merch_review_dimension_rating rating
  JOIN merch_reputation_dimension dimension ON dimension.code=rating.dimension_code
  WHERE rating.revision_id=NEW.id AND dimension.subject_kind<>kind;
  IF invalid_count>0 THEN RAISE EXCEPTION 'Review dimensions do not match typed subject'; END IF;
  IF kind='product' THEN
    SELECT count(*) INTO required_count FROM merch_review_dimension_rating WHERE revision_id=NEW.id
      AND dimension_code IN ('description_accuracy','product_quality');
    IF required_count<>2 THEN RAISE EXCEPTION 'Product reviews require description and quality'; END IF;
  ELSIF cancelled THEN
    SELECT count(*) INTO invalid_count FROM merch_review_dimension_rating WHERE revision_id=NEW.id
      AND dimension_code NOT IN ('communication','problem_resolution');
    IF invalid_count>0 OR NOT EXISTS (SELECT 1 FROM merch_review_dimension_rating
      WHERE revision_id=NEW.id AND dimension_code='communication') THEN
      RAISE EXCEPTION 'Cancelled orders can rate only communication and resolution'; END IF;
  ELSE
    SELECT count(*) INTO required_count FROM merch_review_dimension_rating WHERE revision_id=NEW.id
      AND dimension_code IN ('preparation_dispatch','communication','packaging');
    IF required_count<>3 THEN RAISE EXCEPTION 'Store reviews require service/logistics dimensions'; END IF;
  END IF;
  IF issue_present IS DISTINCT FROM EXISTS (SELECT 1 FROM merch_review_dimension_rating
    WHERE revision_id=NEW.id AND dimension_code='problem_resolution') THEN
    RAISE EXCEPTION 'Problem resolution is rated iff a problem occurred'; END IF;
  RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS merch_review_validate_revision_dimensions_trigger ON merch_review_revision;
CREATE CONSTRAINT TRIGGER merch_review_validate_revision_dimensions_trigger
AFTER INSERT ON merch_review_revision DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW EXECUTE FUNCTION merch_review_validate_revision_dimensions();

CREATE TABLE IF NOT EXISTS merch_seller_response (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  review_id UUID NOT NULL UNIQUE REFERENCES merch_review(id) ON DELETE RESTRICT,
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  current_revision INTEGER NOT NULL DEFAULT 0 CHECK (current_revision >= 0),
  status TEXT NOT NULL DEFAULT 'published'
    CHECK (status IN ('published','hidden','limited','removed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_seller_response_revision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  response_id UUID NOT NULL REFERENCES merch_seller_response(id) ON DELETE RESTRICT,
  revision_no INTEGER NOT NULL CHECK (revision_no > 0),
  authored_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  body TEXT NOT NULL CHECK (length(btrim(body)) BETWEEN 2 AND 2000
    AND body !~ '[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]'),
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (response_id, revision_no)
);
DROP TRIGGER IF EXISTS merch_seller_response_revision_immutable_trigger ON merch_seller_response_revision;
CREATE TRIGGER merch_seller_response_revision_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_seller_response_revision
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_evidence (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  evidence_key TEXT NOT NULL UNIQUE CHECK (length(btrim(evidence_key)) BETWEEN 8 AND 200),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  order_id UUID REFERENCES merch_order(id) ON DELETE RESTRICT,
  source_type TEXT NOT NULL CHECK (source_type IN (
    'order_state','verified_payment','fulfillment','delivery','pickup','tracking',
    'cancellation','refund','dispute','resolution','moderation'
  )),
  source_record_id TEXT NOT NULL CHECK (length(btrim(source_record_id)) BETWEEN 1 AND 200),
  captured_by TEXT NOT NULL CHECK (captured_by IN ('server','payment_provider','fulfillment_provider','moderator')),
  payload JSONB NOT NULL CHECK (jsonb_typeof(payload)='object'),
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
DROP TRIGGER IF EXISTS merch_reputation_evidence_immutable_trigger ON merch_reputation_evidence;
CREATE TRIGGER merch_reputation_evidence_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_evidence
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_operational_signal (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  signal_key TEXT NOT NULL UNIQUE CHECK (length(btrim(signal_key)) BETWEEN 8 AND 200),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  order_id UUID REFERENCES merch_order(id) ON DELETE RESTRICT,
  metric TEXT NOT NULL CHECK (metric IN (
    'dispatch_on_time','seller_cancellation','response_time','tracking_on_time',
    'seller_dispute','refund_compliance','resolution_quality','resolution_timeliness'
  )),
  outcome NUMERIC(5,4) NOT NULL CHECK (outcome BETWEEN 0 AND 1),
  evidence_quality NUMERIC(5,4) NOT NULL CHECK (evidence_quality BETWEEN 0 AND 1),
  responsibility TEXT NOT NULL CHECK (responsibility IN ('seller','courier','buyer','platform','unknown')),
  evidence_id UUID NOT NULL REFERENCES merch_reputation_evidence(id) ON DELETE RESTRICT,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS merch_reputation_signal_store_idx
  ON merch_reputation_operational_signal(store_id, occurred_at DESC);
DROP TRIGGER IF EXISTS merch_reputation_operational_signal_immutable_trigger ON merch_reputation_operational_signal;
CREATE TRIGGER merch_reputation_operational_signal_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_operational_signal
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_key TEXT NOT NULL UNIQUE CHECK (length(btrim(event_key)) BETWEEN 8 AND 240),
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  product_id UUID REFERENCES merch_product(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL CHECK (event_type IN (
    'review_created','review_revised','review_visibility_changed','seller_response_changed',
    'operational_signal_recorded','moderation_decided','badge_changed','aggregate_rebuild_requested'
  )),
  review_id UUID REFERENCES merch_review(id) ON DELETE RESTRICT,
  operational_signal_id UUID REFERENCES merch_reputation_operational_signal(id) ON DELETE RESTRICT,
  payload JSONB NOT NULL DEFAULT '{}'::jsonb CHECK (jsonb_typeof(payload)='object'),
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((subject_kind='store' AND product_id IS NULL)
    OR (subject_kind='product' AND product_id IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS merch_reputation_event_pending_idx
  ON merch_reputation_event(recorded_at, id);
DROP TRIGGER IF EXISTS merch_reputation_event_immutable_trigger ON merch_reputation_event;
CREATE TRIGGER merch_reputation_event_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_event
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_projection_checkpoint (
  event_id UUID PRIMARY KEY REFERENCES merch_reputation_event(id) ON DELETE RESTRICT,
  attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  processed_at TIMESTAMPTZ,
  last_error TEXT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_reputation_aggregate (
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  subject_id UUID NOT NULL,
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  formula_version_id TEXT NOT NULL REFERENCES merch_reputation_formula_version(id) ON DELETE RESTRICT,
  publication_state TEXT NOT NULL CHECK (publication_state IN ('new_store','unrated','published')),
  public_rating NUMERIC(2,1) CHECK (public_rating BETWEEN 1 AND 5),
  review_average NUMERIC(8,6) CHECK (review_average BETWEEN 1 AND 5),
  operational_average NUMERIC(8,6) CHECK (operational_average BETWEEN 1 AND 5),
  verified_review_count INTEGER NOT NULL DEFAULT 0 CHECK (verified_review_count >= 0),
  evaluable_order_count INTEGER NOT NULL DEFAULT 0 CHECK (evaluable_order_count >= 0),
  primary_period_review_count INTEGER NOT NULL DEFAULT 0 CHECK (primary_period_review_count >= 0),
  historical_review_count INTEGER NOT NULL DEFAULT 0 CHECK (historical_review_count >= 0),
  confidence TEXT NOT NULL CHECK (confidence IN ('new','limited','moderate','strong')),
  primary_period_start TIMESTAMPTZ NOT NULL,
  calculated_through TIMESTAMPTZ NOT NULL,
  last_event_id UUID REFERENCES merch_reputation_event(id) ON DELETE RESTRICT,
  calculation JSONB NOT NULL CHECK (jsonb_typeof(calculation)='object'),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (subject_kind, subject_id, formula_version_id),
  CHECK ((publication_state='published' AND public_rating IS NOT NULL)
    OR (publication_state<>'published' AND public_rating IS NULL)),
  CHECK ((subject_kind='store' AND subject_id=store_id) OR subject_kind='product')
);

CREATE TABLE IF NOT EXISTS merch_reputation_dimension_aggregate (
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('store','product')),
  subject_id UUID NOT NULL,
  dimension_code TEXT NOT NULL REFERENCES merch_reputation_dimension(code) ON DELETE RESTRICT,
  formula_version_id TEXT NOT NULL REFERENCES merch_reputation_formula_version(id) ON DELETE RESTRICT,
  public_average NUMERIC(2,1) CHECK (public_average BETWEEN 1 AND 5),
  verified_review_count INTEGER NOT NULL CHECK (verified_review_count >= 0),
  distribution JSONB NOT NULL CHECK (jsonb_typeof(distribution)='object'),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (subject_kind, subject_id, dimension_code, formula_version_id)
);

CREATE TABLE IF NOT EXISTS merch_reputation_idempotency (
  idempotency_key TEXT PRIMARY KEY CHECK (length(btrim(idempotency_key)) BETWEEN 8 AND 200),
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  action TEXT NOT NULL CHECK (action IN (
    'submit_review','seller_response','report','appeal','moderation_decision',
    'appeal_decision','preference_update','moderation_transition',
    'category_suggestion','category_suggestion_decision'
  )),
  request_hash TEXT NOT NULL CHECK (request_hash ~ '^[a-f0-9]{32}$'),
  response JSONB NOT NULL CHECK (jsonb_typeof(response)='object'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_reputation_badge_definition (
  code TEXT PRIMARY KEY CHECK (code IN (
    'identity_verified','on_time_dispatch','standout_communication',
    'excellent_resolution','trusted_seller'
  )),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  requirements_es TEXT NOT NULL,
  requirements_en TEXT NOT NULL,
  requirements JSONB NOT NULL CHECK (jsonb_typeof(requirements)='object'),
  minimum_sample INTEGER NOT NULL CHECK (minimum_sample >= 5),
  validity_days INTEGER NOT NULL CHECK (validity_days BETWEEN 30 AND 730),
  formula_version_id TEXT NOT NULL REFERENCES merch_reputation_formula_version(id) ON DELETE RESTRICT,
  evaluator_key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
INSERT INTO merch_reputation_badge_definition
  (code,name_es,name_en,requirements_es,requirements_en,requirements,minimum_sample,validity_days,formula_version_id,evaluator_key)
VALUES
  ('identity_verified','Identidad verificada','Verified identity','Identidad vigente verificada por la plataforma y al menos cinco órdenes evaluables.','A current platform-verified identity and at least five evaluable orders.',
    '{"identityRequired":true}'::jsonb,5,365,'merch-commercial-bayes-v1','identity_verified_v1'),
  ('on_time_dispatch','Despacho puntual','On-time dispatch','Al menos 90 % de despachos a tiempo en 20 órdenes durante 180 días.','At least 90% on-time dispatch across 20 orders in 180 days.',
    '{"metric":"dispatch_on_time","minimumOutcome":0.90,"responsibility":"seller"}'::jsonb,20,180,'merch-commercial-bayes-v1','on_time_dispatch_v1'),
  ('standout_communication','Comunicación destacada','Standout communication','Promedio verificado de comunicación de 4,5 o más en 20 evaluaciones durante 180 días.','Verified communication average of at least 4.5 across 20 reviews over 180 days.',
    '{"dimension":"communication","minimumAverage":4.5}'::jsonb,20,180,'merch-commercial-bayes-v1','standout_communication_v1'),
  ('excellent_resolution','Excelente resolución','Excellent resolution','Promedio verificado de resolución de 4,5 o más en 10 casos durante 365 días.','Verified resolution average of at least 4.5 across 10 cases in 365 days.',
    '{"dimension":"problem_resolution","minimumAverage":4.5,"issueRequired":true}'::jsonb,10,365,'merch-commercial-bayes-v1','excellent_resolution_v1'),
  ('trusted_seller','Vendedor confiable','Trusted seller','Al menos 30 evaluaciones verificadas, confianza fuerte y puntaje comercial mínimo de 4,0 durante 365 días.','At least 30 verified reviews, strong evidence, and a minimum 4.0 commercial score over 365 days.',
    '{"minimumRating":4.0,"requiredConfidence":"strong"}'::jsonb,30,365,'merch-commercial-bayes-v1','trusted_seller_v1')
ON CONFLICT (code) DO NOTHING;

CREATE TABLE IF NOT EXISTS merch_reputation_badge_award (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  badge_code TEXT NOT NULL REFERENCES merch_reputation_badge_definition(code) ON DELETE RESTRICT,
  formula_version_id TEXT NOT NULL REFERENCES merch_reputation_formula_version(id) ON DELETE RESTRICT,
  status TEXT NOT NULL CHECK (status IN ('active','expired','revoked')),
  evidence_snapshot JSONB NOT NULL CHECK (jsonb_typeof(evidence_snapshot)='object'),
  awarded_at TIMESTAMPTZ NOT NULL,
  valid_until TIMESTAMPTZ NOT NULL,
  revoked_at TIMESTAMPTZ,
  revocation_reason TEXT,
  UNIQUE (store_id, badge_code, awarded_at),
  CHECK (valid_until > awarded_at),
  CHECK ((status='revoked') = (revoked_at IS NOT NULL))
);
CREATE UNIQUE INDEX IF NOT EXISTS merch_reputation_badge_active_unique
  ON merch_reputation_badge_award(store_id,badge_code) WHERE status='active';

CREATE TABLE IF NOT EXISTS merch_reputation_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  target_type TEXT NOT NULL CHECK (target_type IN ('review','seller_response')),
  target_id UUID NOT NULL,
  reporter_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  reason TEXT NOT NULL CHECK (reason IN (
    'offensive','personal_information','spam','extortion','conflict_of_interest',
    'false_review','coordinated_manipulation','duplicate','irrelevant'
  )),
  details TEXT CHECK (details IS NULL OR length(btrim(details)) BETWEEN 10 AND 2000),
  authorized_evidence JSONB NOT NULL DEFAULT '[]'::jsonb
    CHECK (jsonb_typeof(authorized_evidence)='array'),
  status TEXT NOT NULL DEFAULT 'submitted'
    CHECK (status IN ('submitted','triaged','in_review','decided','withdrawn')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (target_type, target_id, reporter_party_id, reason)
);

CREATE TABLE IF NOT EXISTS merch_reputation_moderation_case (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL UNIQUE REFERENCES merch_reputation_report(id) ON DELETE RESTRICT,
  state TEXT NOT NULL DEFAULT 'open'
    CHECK (state IN ('open','in_review','awaiting_evidence','provisionally_hidden','decided','appealed','closed')),
  assigned_to BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  provisional_reason TEXT,
  provisional_previous_visibility TEXT
    CHECK (provisional_previous_visibility IS NULL OR provisional_previous_visibility IN ('published','hidden','limited','removed')),
  opened_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
ALTER TABLE merch_reputation_moderation_case
  ADD COLUMN IF NOT EXISTS provisional_previous_visibility TEXT
  CHECK (provisional_previous_visibility IS NULL OR provisional_previous_visibility IN ('published','hidden','limited','removed'));

CREATE TABLE IF NOT EXISTS merch_reputation_moderation_decision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  case_id UUID NOT NULL REFERENCES merch_reputation_moderation_case(id) ON DELETE RESTRICT,
  decided_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  decision TEXT NOT NULL CHECK (decision IN ('approve','reject_report','hide','restore','limit')),
  reason_code TEXT NOT NULL CHECK (reason_code IN (
    'legitimate_negative_opinion','offensive','personal_information','spam','extortion',
    'conflict_of_interest','false_review','coordinated_manipulation','duplicate','irrelevant'
  )),
  rationale TEXT NOT NULL CHECK (length(btrim(rationale)) BETWEEN 20 AND 3000),
  evidence_snapshot JSONB NOT NULL CHECK (jsonb_typeof(evidence_snapshot)='object'),
  previous_visibility TEXT NOT NULL CHECK (previous_visibility IN ('published','hidden','limited','removed')),
  decided_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
DROP TRIGGER IF EXISTS merch_reputation_moderation_decision_immutable_trigger
  ON merch_reputation_moderation_decision;
CREATE TRIGGER merch_reputation_moderation_decision_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_moderation_decision
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_appeal (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  decision_id UUID NOT NULL REFERENCES merch_reputation_moderation_decision(id) ON DELETE RESTRICT,
  appellant_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  grounds TEXT NOT NULL CHECK (length(btrim(grounds)) BETWEEN 20 AND 3000),
  state TEXT NOT NULL DEFAULT 'open'
    CHECK (state IN ('open','awaiting_evidence','upheld','reversed','closed')),
  reviewed_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  outcome_reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  decided_at TIMESTAMPTZ,
  UNIQUE (decision_id, appellant_party_id),
  CHECK (reviewed_by IS NULL OR reviewed_by IS DISTINCT FROM appellant_party_id)
);

CREATE TABLE IF NOT EXISTS merch_reputation_audit_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  actor_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  action TEXT NOT NULL,
  record_type TEXT NOT NULL,
  record_id TEXT NOT NULL,
  reason TEXT NOT NULL,
  evidence JSONB NOT NULL DEFAULT '{}'::jsonb CHECK (jsonb_typeof(evidence)='object'),
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
DROP TRIGGER IF EXISTS merch_reputation_audit_event_immutable_trigger ON merch_reputation_audit_event;
CREATE TRIGGER merch_reputation_audit_event_immutable_trigger
  BEFORE UPDATE OR DELETE ON merch_reputation_audit_event
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_immutable();

CREATE TABLE IF NOT EXISTS merch_reputation_risk_policy_version (
  id TEXT PRIMARY KEY,
  status TEXT NOT NULL CHECK (status IN ('draft','active','retired')),
  rules JSONB NOT NULL CHECK (jsonb_typeof(rules)='object'),
  approved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((status <> 'active') OR approved_at IS NOT NULL)
);
INSERT INTO merch_reputation_risk_policy_version(id,status,rules,approved_at)
VALUES
  ('merch-risk-v1-draft','draft',
   '{"requiresCrossFunctionalApproval":true,"automaticFinancialPenaltyFromRating":false,"materialMeasuresRequireEvidence":true,"financialMeasuresRequireIndependentHumanReview":true}'::jsonb,
   NULL)
ON CONFLICT (id) DO NOTHING;

CREATE TABLE IF NOT EXISTS merch_reputation_risk_case (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  opened_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  policy_version_id TEXT NOT NULL REFERENCES merch_reputation_risk_policy_version(id) ON DELETE RESTRICT,
  trigger_type TEXT NOT NULL CHECK (trigger_type IN (
    'suspected_fraud','payment_misappropriation','tracking_falsification',
    'repeated_nonfulfillment','abuse'
  )),
  reason TEXT NOT NULL CHECK (length(btrim(reason)) BETWEEN 20 AND 3000),
  evidence JSONB NOT NULL CHECK (jsonb_typeof(evidence)='object'),
  state TEXT NOT NULL DEFAULT 'open' CHECK (state IN ('open','investigating','appealed','resolved','closed')),
  notify_affected_party BOOLEAN NOT NULL DEFAULT TRUE,
  notification_exception_reason TEXT,
  opened_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (notify_affected_party OR length(btrim(notification_exception_reason)) >= 20)
);

CREATE TABLE IF NOT EXISTS merch_reputation_risk_measure (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  case_id UUID NOT NULL REFERENCES merch_reputation_risk_case(id) ON DELETE RESTRICT,
  measure_type TEXT NOT NULL CHECK (measure_type IN (
    'enhanced_review','listing_limit','store_pause','settlement_hold'
  )),
  imposed_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  independently_reviewed_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  reason TEXT NOT NULL CHECK (length(btrim(reason)) BETWEEN 20 AND 3000),
  evidence JSONB NOT NULL CHECK (jsonb_typeof(evidence)='object'),
  imposed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  lifted_at TIMESTAMPTZ,
  CHECK (independently_reviewed_by IS NULL OR independently_reviewed_by <> imposed_by),
  CHECK (measure_type <> 'settlement_hold' OR independently_reviewed_by IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS merch_reputation_notification_preference (
  party_id BIGINT PRIMARY KEY REFERENCES party(id) ON DELETE RESTRICT,
  review_invitation BOOLEAN NOT NULL DEFAULT FALSE,
  review_reminder BOOLEAN NOT NULL DEFAULT FALSE,
  seller_response BOOLEAN NOT NULL DEFAULT FALSE,
  moderation_change BOOLEAN NOT NULL DEFAULT FALSE,
  evidence_request BOOLEAN NOT NULL DEFAULT FALSE,
  appeal_result BOOLEAN NOT NULL DEFAULT FALSE,
  badge_change BOOLEAN NOT NULL DEFAULT FALSE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS merch_reputation_notification_outbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  notification_key TEXT NOT NULL UNIQUE,
  recipient_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  notification_type TEXT NOT NULL CHECK (notification_type IN (
    'review_invitation','review_reminder','seller_response','moderation_change',
    'evidence_request','appeal_result','badge_gained','badge_lost'
  )),
  safe_payload JSONB NOT NULL CHECK (jsonb_typeof(safe_payload)='object'),
  available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  sent_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (sent_at IS NULL OR cancelled_at IS NULL),
  CHECK (NOT (safe_payload ?| ARRAY[
    'orderNumber','address','email','phone','payment','tracking','comment','responseBody'
  ]))
);
ALTER TABLE merch_reputation_notification_outbox
  ADD COLUMN IF NOT EXISTS cancelled_at TIMESTAMPTZ;

CREATE TABLE IF NOT EXISTS merch_reputation_exposure_daily (
  day DATE NOT NULL,
  store_id UUID NOT NULL REFERENCES merch_store(id) ON DELETE RESTRICT,
  surface TEXT NOT NULL CHECK (surface IN ('search','recommendations','new_store_discovery','checkout')),
  impressions BIGINT NOT NULL DEFAULT 0 CHECK (impressions >= 0),
  conversions BIGINT NOT NULL DEFAULT 0 CHECK (conversions >= 0),
  is_new_store BOOLEAN NOT NULL,
  reputation_contribution_sum NUMERIC(16,6) NOT NULL DEFAULT 0,
  total_ranking_score_sum NUMERIC(16,6) NOT NULL DEFAULT 0,
  PRIMARY KEY (day, store_id, surface)
);

CREATE OR REPLACE FUNCTION merch_reputation_recalculate_badges(
  p_store_id UUID,
  p_through TIMESTAMPTZ DEFAULT NOW(),
  p_environment TEXT DEFAULT 'production'
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  definition RECORD;
  sample_count INTEGER;
  measured_value NUMERIC;
  meets_requirement BOOLEAN;
  active_award merch_reputation_badge_award%ROWTYPE;
  award_id UUID;
  gained INTEGER:=0;
  lost INTEGER:=0;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key='badges' AND environment=p_environment AND enabled) THEN
    RETURN jsonb_build_object('storeId',p_store_id,'enabled',false,'gained',0,'lost',0);
  END IF;
  PERFORM pg_advisory_xact_lock(hashtextextended(p_store_id::TEXT,47));
  UPDATE merch_reputation_badge_award SET status='expired'
    WHERE store_id=p_store_id AND status='active' AND valid_until<=p_through;
  FOR definition IN SELECT * FROM merch_reputation_badge_definition ORDER BY code LOOP
    sample_count:=0;
    measured_value:=NULL;
    meets_requirement:=FALSE;
    active_award:=NULL;
    CASE definition.code
      WHEN 'identity_verified' THEN
        SELECT count(orders.id)::INTEGER,
          CASE WHEN store.identity_verified_at IS NOT NULL THEN 1 ELSE 0 END
          INTO sample_count,measured_value
        FROM merch_store store LEFT JOIN merch_order orders ON orders.store_id=store.id
          AND orders.fraud_state='clear'
          AND orders.verified_buyer_at IS NOT NULL
          AND NOT merch_reputation_accounts_related(store.id,orders.buyer_party_id)
          AND ((orders.payment_state IN ('verified','partially_refunded','refunded','disputed','chargeback')
              AND orders.verified_payment_at IS NOT NULL
              AND ((orders.fulfillment_state='delivered' AND orders.delivered_at IS NOT NULL)
                OR (orders.fulfillment_state='picked_up' AND orders.pickup_confirmed_at IS NOT NULL)))
            OR (orders.order_state='cancelled' AND orders.cancelled_at IS NOT NULL))
        WHERE store.id=p_store_id GROUP BY store.identity_verified_at;
        meets_requirement:=COALESCE(measured_value,0)=1
          AND COALESCE(sample_count,0)>=definition.minimum_sample;
      WHEN 'on_time_dispatch' THEN
        SELECT count(*)::INTEGER,avg(signal.outcome) INTO sample_count,measured_value
        FROM merch_reputation_operational_signal signal
        WHERE signal.store_id=p_store_id AND signal.metric=definition.requirements->>'metric'
          AND signal.responsibility=definition.requirements->>'responsibility'
          AND (signal.order_id IS NULL OR EXISTS (SELECT 1 FROM merch_order signal_order
            WHERE signal_order.id=signal.order_id AND signal_order.fraud_state='clear'))
          AND signal.occurred_at BETWEEN p_through-make_interval(days=>definition.validity_days) AND p_through;
        meets_requirement:=COALESCE(sample_count,0)>=definition.minimum_sample
          AND COALESCE(measured_value,0)>=(definition.requirements->>'minimumOutcome')::NUMERIC;
      WHEN 'standout_communication' THEN
        SELECT count(*)::INTEGER,avg(rating.rating) INTO sample_count,measured_value
        FROM merch_review review JOIN merch_order reviewed_order ON reviewed_order.id=review.order_id
        JOIN merch_review_revision revision
          ON revision.review_id=review.id AND revision.revision_no=review.current_revision
        JOIN merch_review_dimension_rating rating ON rating.revision_id=revision.id
        WHERE review.store_id=p_store_id AND review.review_kind='store'
          AND review.status IN ('published','limited') AND reviewed_order.fraud_state='clear'
          AND rating.dimension_code=definition.requirements->>'dimension'
          AND revision.submitted_at BETWEEN p_through-make_interval(days=>definition.validity_days) AND p_through;
        meets_requirement:=COALESCE(sample_count,0)>=definition.minimum_sample
          AND COALESCE(measured_value,0)>=(definition.requirements->>'minimumAverage')::NUMERIC;
      WHEN 'excellent_resolution' THEN
        SELECT count(*)::INTEGER,avg(rating.rating) INTO sample_count,measured_value
        FROM merch_review review JOIN merch_order reviewed_order ON reviewed_order.id=review.order_id
        JOIN merch_review_revision revision
          ON revision.review_id=review.id AND revision.revision_no=review.current_revision
        JOIN merch_review_dimension_rating rating ON rating.revision_id=revision.id
        WHERE review.store_id=p_store_id AND review.review_kind='store'
          AND review.status IN ('published','limited') AND reviewed_order.fraud_state='clear'
          AND revision.issue_occurred
          AND rating.dimension_code=definition.requirements->>'dimension'
          AND revision.submitted_at BETWEEN p_through-make_interval(days=>definition.validity_days) AND p_through;
        meets_requirement:=COALESCE(sample_count,0)>=definition.minimum_sample
          AND COALESCE(measured_value,0)>=(definition.requirements->>'minimumAverage')::NUMERIC;
      WHEN 'trusted_seller' THEN
        SELECT aggregate.verified_review_count,aggregate.public_rating
          INTO sample_count,measured_value
        FROM merch_reputation_aggregate aggregate
        WHERE aggregate.subject_kind='store' AND aggregate.subject_id=p_store_id
          AND aggregate.formula_version_id=definition.formula_version_id
        ORDER BY aggregate.calculated_through DESC LIMIT 1;
        meets_requirement:=COALESCE(sample_count,0)>=definition.minimum_sample
          AND COALESCE(measured_value,0)>=(definition.requirements->>'minimumRating')::NUMERIC
          AND EXISTS (SELECT 1 FROM merch_reputation_aggregate aggregate
            WHERE aggregate.subject_kind='store' AND aggregate.subject_id=p_store_id
              AND aggregate.formula_version_id=definition.formula_version_id
              AND aggregate.confidence=definition.requirements->>'requiredConfidence');
    END CASE;
    SELECT * INTO active_award FROM merch_reputation_badge_award
      WHERE store_id=p_store_id AND badge_code=definition.code AND status='active' FOR UPDATE;
    IF meets_requirement AND active_award.id IS NULL THEN
      INSERT INTO merch_reputation_badge_award(
        store_id,badge_code,formula_version_id,status,evidence_snapshot,awarded_at,valid_until
      ) VALUES (
        p_store_id,definition.code,definition.formula_version_id,'active',
        jsonb_build_object('evaluatorKey',definition.evaluator_key,'sample',sample_count,
          'value',measured_value,'requirements',definition.requirements,'calculatedThrough',p_through),
        p_through,p_through+make_interval(days=>definition.validity_days)
      ) RETURNING id INTO award_id;
      gained:=gained+1;
      INSERT INTO merch_reputation_notification_outbox(
        notification_key,recipient_party_id,notification_type,safe_payload
      ) SELECT 'badge-gained:'||award_id,store.owner_party_id,'badge_gained',
          jsonb_build_object('storeId',p_store_id,'badgeCode',definition.code)
        FROM merch_store store JOIN merch_reputation_notification_preference preference
          ON preference.party_id=store.owner_party_id AND preference.badge_change
        WHERE store.id=p_store_id ON CONFLICT (notification_key) DO NOTHING;
    ELSIF NOT meets_requirement AND active_award.id IS NOT NULL THEN
      UPDATE merch_reputation_badge_award SET status='revoked',revoked_at=p_through,
        revocation_reason='Reproducible requirements are no longer met'
        WHERE id=active_award.id;
      lost:=lost+1;
      INSERT INTO merch_reputation_notification_outbox(
        notification_key,recipient_party_id,notification_type,safe_payload
      ) SELECT 'badge-lost:'||active_award.id,store.owner_party_id,'badge_lost',
          jsonb_build_object('storeId',p_store_id,'badgeCode',definition.code)
        FROM merch_store store JOIN merch_reputation_notification_preference preference
          ON preference.party_id=store.owner_party_id AND preference.badge_change
        WHERE store.id=p_store_id ON CONFLICT (notification_key) DO NOTHING;
    END IF;
  END LOOP;
  RETURN jsonb_build_object('storeId',p_store_id,'enabled',true,'gained',gained,'lost',lost);
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_set_priorities(
  p_actor_party_id BIGINT,
  p_subject_kind TEXT,
  p_dimension_codes JSONB,
  p_expected_revision INTEGER,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  profile merch_reputation_priority_profile%ROWTYPE;
  next_revision INTEGER;
  revision_id UUID;
  entry RECORD;
  response JSONB;
BEGIN
  IF p_subject_kind NOT IN ('store','product')
    OR jsonb_typeof(p_dimension_codes)<>'array'
    OR jsonb_array_length(p_dimension_codes) NOT BETWEEN 1 AND 20
    OR p_expected_revision<0 THEN
    RAISE EXCEPTION 'Invalid commercial reputation priorities';
  END IF;
  IF (SELECT count(*) FROM jsonb_array_elements_text(p_dimension_codes))
      <> (SELECT count(DISTINCT value) FROM jsonb_array_elements_text(p_dimension_codes)) THEN
    RAISE EXCEPTION 'Commercial reputation priorities cannot contain duplicates';
  END IF;
  IF EXISTS (
    SELECT 1 FROM jsonb_array_elements_text(p_dimension_codes) item
    LEFT JOIN merch_reputation_dimension dimension ON dimension.code=item.value
    WHERE dimension.code IS NULL OR dimension.subject_kind<>p_subject_kind
      OR NOT dimension.governed OR dimension.status<>'active'
  ) THEN
    RAISE EXCEPTION 'Commercial reputation priority is not an active governed dimension';
  END IF;
  request_hash:=md5(jsonb_build_object(
    'actor',p_actor_party_id,'subjectKind',p_subject_kind,
    'dimensionCodes',p_dimension_codes,'expectedRevision',p_expected_revision
  )::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency
    WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'preference_update' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  INSERT INTO merch_reputation_priority_profile(party_id,subject_kind)
    VALUES (p_actor_party_id,p_subject_kind) ON CONFLICT DO NOTHING;
  SELECT * INTO profile FROM merch_reputation_priority_profile
    WHERE party_id=p_actor_party_id AND subject_kind=p_subject_kind FOR UPDATE;
  IF profile.current_revision IS DISTINCT FROM p_expected_revision THEN
    RAISE EXCEPTION 'Commercial reputation priority revision conflict';
  END IF;
  next_revision:=profile.current_revision+1;
  INSERT INTO merch_reputation_priority_revision(party_id,subject_kind,revision_no)
    VALUES (p_actor_party_id,p_subject_kind,next_revision) RETURNING id INTO revision_id;
  FOR entry IN SELECT value,ordinality FROM jsonb_array_elements_text(p_dimension_codes) WITH ORDINALITY
  LOOP
    INSERT INTO merch_reputation_priority_item(revision_id,dimension_code,position)
      VALUES (revision_id,entry.value,entry.ordinality);
  END LOOP;
  UPDATE merch_reputation_priority_profile
    SET current_revision=next_revision,updated_at=NOW()
    WHERE party_id=p_actor_party_id AND subject_kind=p_subject_kind;
  response:=jsonb_build_object(
    'subjectKind',p_subject_kind,'revision',next_revision,
    'orderedDimensionCodes',p_dimension_codes,
    'affectsPublicScore',false
  );
  INSERT INTO merch_reputation_idempotency(
    idempotency_key,actor_party_id,action,request_hash,response
  ) VALUES (p_idempotency_key,p_actor_party_id,'preference_update',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_submit_category_suggestion(
  p_actor_party_id BIGINT,
  p_subject_kind TEXT,
  p_label TEXT,
  p_definition TEXT,
  p_idempotency_key TEXT,
  p_environment TEXT DEFAULT 'production'
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  normalized TEXT;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  suggestion_id UUID;
  created BOOLEAN:=FALSE;
  response JSONB;
BEGIN
  IF p_subject_kind NOT IN ('store','product')
    OR length(btrim(p_label)) NOT BETWEEN 3 AND 80
    OR length(btrim(p_definition)) NOT BETWEEN 20 AND 500 THEN
    RAISE EXCEPTION 'Invalid commercial category suggestion';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key=CASE WHEN p_subject_kind='store' THEN 'store_reviews' ELSE 'product_reviews' END
        AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Commercial category suggestions are disabled';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key='moderation' AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Commercial category moderation is disabled';
  END IF;
  normalized:=trim(BOTH '-' FROM lower(regexp_replace(btrim(p_label),'[^[:alnum:]]+','-','g')));
  IF length(normalized) NOT BETWEEN 3 AND 80 THEN
    RAISE EXCEPTION 'Commercial category suggestion has no usable label';
  END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'subjectKind',p_subject_kind,
    'label',btrim(p_label),'definition',btrim(p_definition))::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'category_suggestion' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  INSERT INTO merch_reputation_category_suggestion(
    suggested_by,subject_kind,label,definition,normalized_key
  ) VALUES (
    p_actor_party_id,p_subject_kind,btrim(p_label),btrim(p_definition),normalized
  ) ON CONFLICT (subject_kind,normalized_key) DO NOTHING
  RETURNING id INTO suggestion_id;
  IF suggestion_id IS NULL THEN
    SELECT id INTO suggestion_id FROM merch_reputation_category_suggestion
      WHERE subject_kind=p_subject_kind AND normalized_key=normalized;
  ELSE
    created:=TRUE;
  END IF;
  response:=jsonb_build_object('suggestionId',suggestion_id,
    'status',CASE WHEN created THEN 'pending' ELSE 'duplicate' END,
    'created',created,'affectsPublicScore',false);
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'category_suggestion',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_decide_category_suggestion(
  p_actor_party_id BIGINT,
  p_suggestion_id UUID,
  p_status TEXT,
  p_minimum_sample INTEGER,
  p_bias_test JSONB,
  p_utility_test JSONB,
  p_reason TEXT,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  suggestion merch_reputation_category_suggestion%ROWTYPE;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  response JSONB;
BEGIN
  IF p_status NOT IN ('duplicate','testing','approved','rejected')
    OR length(btrim(p_reason)) NOT BETWEEN 20 AND 3000
    OR (p_minimum_sample IS NOT NULL AND p_minimum_sample<5) THEN
    RAISE EXCEPTION 'Invalid category suggestion decision';
  END IF;
  IF p_status='approved' AND (
      p_minimum_sample IS NULL OR jsonb_typeof(p_bias_test)<>'object'
      OR NOT p_bias_test @> '{"passed":true}'::jsonb
      OR jsonb_typeof(p_utility_test)<>'object'
      OR NOT p_utility_test @> '{"passed":true}'::jsonb) THEN
    RAISE EXCEPTION 'Approval requires minimum sample and passed bias and utility tests';
  END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'suggestion',p_suggestion_id,
    'status',p_status,'minimumSample',p_minimum_sample,'biasTest',p_bias_test,
    'utilityTest',p_utility_test,'reason',p_reason)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'category_suggestion_decision' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO suggestion FROM merch_reputation_category_suggestion
    WHERE id=p_suggestion_id FOR UPDATE;
  IF NOT FOUND OR suggestion.status IN ('approved','rejected','duplicate') THEN
    RAISE EXCEPTION 'Category suggestion cannot transition from its current state';
  END IF;
  UPDATE merch_reputation_category_suggestion SET status=p_status,
    minimum_sample=p_minimum_sample,bias_test=p_bias_test,utility_test=p_utility_test,
    decided_by=CASE WHEN p_status IN ('approved','rejected','duplicate') THEN p_actor_party_id END,
    decided_at=CASE WHEN p_status IN ('approved','rejected','duplicate') THEN NOW() END,
    decision_reason=btrim(p_reason)
    WHERE id=p_suggestion_id;
  INSERT INTO merch_reputation_audit_event(
    actor_party_id,action,record_type,record_id,reason,evidence
  ) VALUES (
    p_actor_party_id,'category_suggestion_'||p_status,'category_suggestion',
    p_suggestion_id::TEXT,btrim(p_reason),jsonb_build_object(
      'minimumSample',p_minimum_sample,'biasTest',p_bias_test,'utilityTest',p_utility_test)
  );
  response:=jsonb_build_object('suggestionId',p_suggestion_id,'status',p_status,
    'minimumSample',p_minimum_sample,'affectsPublicScore',false);
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'category_suggestion_decision',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_submit_review(
  p_actor_party_id BIGINT,
  p_review_kind TEXT,
  p_order_id UUID,
  p_order_line_id UUID,
  p_overall_rating SMALLINT,
  p_issue_occurred BOOLEAN,
  p_comment TEXT,
  p_dimensions JSONB,
  p_images JSONB,
  p_expected_revision INTEGER,
  p_idempotency_key TEXT,
  p_environment TEXT DEFAULT 'production'
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  target_store UUID;
  target_product UUID;
  source_time TIMESTAMPTZ;
  target_review merch_review%ROWTYPE;
  revision_id UUID;
  next_revision INTEGER;
  entry RECORD;
  response JSONB;
  required_flag TEXT;
BEGIN
  IF p_review_kind NOT IN ('store','product')
    OR p_environment NOT IN ('development','staging','production')
    OR jsonb_typeof(p_dimensions) <> 'object'
    OR jsonb_typeof(COALESCE(p_images,'[]'::jsonb)) <> 'array' THEN
    RAISE EXCEPTION 'Invalid review request';
  END IF;
  required_flag := CASE p_review_kind WHEN 'store' THEN 'store_reviews' ELSE 'product_reviews' END;
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key=required_flag AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Merch reputation feature is disabled';
  END IF;
  IF jsonb_array_length(COALESCE(p_images,'[]'::jsonb)) > 4 THEN
    RAISE EXCEPTION 'At most four review images are allowed';
  END IF;
  IF jsonb_array_length(COALESCE(p_images,'[]'::jsonb)) > 0
    AND NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key='review_images' AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Review images are disabled';
  END IF;
  request_hash := md5(jsonb_build_object(
    'actor',p_actor_party_id,'kind',p_review_kind,'order',p_order_id,'line',p_order_line_id,
    'rating',p_overall_rating,'issue',p_issue_occurred,'comment',p_comment,
    'dimensions',p_dimensions,'images',COALESCE(p_images,'[]'::jsonb),
    'expectedRevision',p_expected_revision
  )::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key, 0));
  SELECT * INTO prior FROM merch_reputation_idempotency
    WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action <> 'submit_review' OR prior.request_hash <> request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;

  IF p_review_kind='store' THEN
    SELECT store_id,COALESCE(delivered_at,pickup_confirmed_at,cancellation_resolved_at,cancelled_at)
      INTO target_store,source_time FROM merch_order WHERE id=p_order_id FOR UPDATE;
    IF p_order_line_id IS NOT NULL
      OR NOT merch_review_evidence_is_eligible('store',p_order_id,p_actor_party_id) THEN
      RAISE EXCEPTION 'Order is not eligible for this store review';
    END IF;
  ELSE
    SELECT line.store_id,line.product_id,line.delivered_at
      INTO target_store,target_product,source_time
      FROM merch_order_line line
      WHERE line.id=p_order_line_id AND line.order_id=p_order_id FOR UPDATE;
    IF target_product IS NULL
      OR NOT merch_review_evidence_is_eligible('product',p_order_line_id,p_actor_party_id) THEN
      RAISE EXCEPTION 'Order line is not eligible for this product review';
    END IF;
  END IF;

  IF p_review_kind='store' THEN
    SELECT * INTO target_review FROM merch_review
      WHERE review_kind='store' AND order_id=p_order_id FOR UPDATE;
  ELSE
    SELECT * INTO target_review FROM merch_review
      WHERE review_kind='product' AND order_line_id=p_order_line_id FOR UPDATE;
  END IF;
  IF NOT FOUND THEN
    IF p_expected_revision <> 0 THEN RAISE EXCEPTION 'Review revision conflict'; END IF;
    INSERT INTO merch_review(
      review_kind,store_id,product_id,order_id,order_line_id,author_party_id,edit_deadline
    ) VALUES (
      p_review_kind,target_store,target_product,p_order_id,p_order_line_id,p_actor_party_id,
      source_time + INTERVAL '30 days'
    ) RETURNING * INTO target_review;
  ELSE
    IF target_review.author_party_id IS DISTINCT FROM p_actor_party_id THEN
      RAISE EXCEPTION 'Review belongs to another buyer';
    END IF;
    IF NOW() > target_review.edit_deadline THEN RAISE EXCEPTION 'Review edit period expired'; END IF;
    IF p_expected_revision IS DISTINCT FROM target_review.current_revision THEN
      RAISE EXCEPTION 'Review revision conflict';
    END IF;
  END IF;

  next_revision := target_review.current_revision + 1;
  INSERT INTO merch_review_revision(
    review_id,revision_no,overall_rating,issue_occurred,comment
  ) VALUES (
    target_review.id,next_revision,p_overall_rating,p_issue_occurred,NULLIF(btrim(p_comment),'')
  ) RETURNING id INTO revision_id;
  FOR entry IN SELECT key,value FROM jsonb_each_text(p_dimensions)
  LOOP
    INSERT INTO merch_review_dimension_rating(revision_id,dimension_code,rating)
    VALUES (revision_id,entry.key,entry.value::SMALLINT);
  END LOOP;
  FOR entry IN SELECT value,ordinality FROM jsonb_array_elements(COALESCE(p_images,'[]'::jsonb))
      WITH ORDINALITY
  LOOP
    PERFORM 1 FROM merch_review_media_asset asset
      WHERE asset.id=(entry.value->>'mediaAssetId')::UUID
        AND asset.uploaded_by=p_actor_party_id
        AND asset.scan_status='safe'
        AND asset.moderation_status IN ('pending','published');
    IF NOT FOUND THEN
      RAISE EXCEPTION 'Review image is unavailable, unsafe, or belongs to another user';
    END IF;
    INSERT INTO merch_review_image(revision_id,media_asset_id,alt_text,position)
    SELECT revision_id,(entry.value->>'mediaAssetId')::UUID,entry.value->>'altText',entry.ordinality;
  END LOOP;
  UPDATE merch_review SET current_revision=next_revision WHERE id=target_review.id;
  IF p_review_kind='store' THEN
    UPDATE merch_reputation_notification_outbox SET cancelled_at=NOW()
      WHERE notification_key='review-reminder:'||p_order_id
        AND sent_at IS NULL AND cancelled_at IS NULL;
  END IF;
  INSERT INTO merch_reputation_event(
    event_key,subject_kind,store_id,product_id,event_type,review_id,payload
  ) VALUES (
    'review:'||target_review.id||':revision:'||next_revision,p_review_kind,
    target_store,target_product,
    CASE next_revision WHEN 1 THEN 'review_created' ELSE 'review_revised' END,
    target_review.id,jsonb_build_object('revision',next_revision,'verifiedPurchase',true)
  );
  response := jsonb_build_object(
    'reviewId',target_review.id,'revision',next_revision,'status','published',
    'editDeadline',target_review.edit_deadline,'verifiedPurchase',true
  );
  INSERT INTO merch_reputation_idempotency(
    idempotency_key,actor_party_id,action,request_hash,response
  ) VALUES (p_idempotency_key,p_actor_party_id,'submit_review',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_respond(
  p_actor_party_id BIGINT,
  p_review_id UUID,
  p_body TEXT,
  p_expected_revision INTEGER,
  p_idempotency_key TEXT,
  p_environment TEXT DEFAULT 'production'
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  target_review merch_review%ROWTYPE;
  target_response merch_seller_response%ROWTYPE;
  next_revision INTEGER;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  response JSONB;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key='seller_responses' AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Seller responses are disabled';
  END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'review',p_review_id,
    'body',p_body,'expectedRevision',p_expected_revision)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'seller_response' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO target_review FROM merch_review WHERE id=p_review_id;
  IF NOT FOUND OR NOT EXISTS (SELECT 1 FROM merch_store_member
      WHERE store_id=target_review.store_id AND party_id=p_actor_party_id AND status='active') THEN
    RAISE EXCEPTION 'Seller response is outside actor scope';
  END IF;
  SELECT * INTO target_response FROM merch_seller_response
    WHERE review_id=p_review_id FOR UPDATE;
  IF NOT FOUND THEN
    IF p_expected_revision<>0 THEN RAISE EXCEPTION 'Seller response revision conflict'; END IF;
    INSERT INTO merch_seller_response(review_id,store_id)
      VALUES (p_review_id,target_review.store_id) RETURNING * INTO target_response;
  ELSIF target_response.current_revision IS DISTINCT FROM p_expected_revision THEN
    RAISE EXCEPTION 'Seller response revision conflict';
  END IF;
  next_revision:=target_response.current_revision+1;
  INSERT INTO merch_seller_response_revision(response_id,revision_no,authored_by,body)
    VALUES (target_response.id,next_revision,p_actor_party_id,p_body);
  UPDATE merch_seller_response SET current_revision=next_revision,updated_at=NOW()
    WHERE id=target_response.id;
  INSERT INTO merch_reputation_event(
    event_key,subject_kind,store_id,event_type,review_id,payload
  ) VALUES (
    'response:'||target_response.id||':revision:'||next_revision,'store',
    target_review.store_id,'seller_response_changed',p_review_id,
    jsonb_build_object('responseId',target_response.id,'revision',next_revision,'actor',p_actor_party_id)
  );
  INSERT INTO merch_reputation_notification_outbox(
    notification_key,recipient_party_id,notification_type,safe_payload
  )
  SELECT 'seller-response:'||target_response.id||':'||next_revision,
    target_review.author_party_id,'seller_response',
    jsonb_build_object('reviewId',p_review_id,'storeId',target_review.store_id)
  FROM merch_reputation_notification_preference preference
  WHERE preference.party_id=target_review.author_party_id AND preference.seller_response
  ON CONFLICT (notification_key) DO NOTHING;
  response:=jsonb_build_object('responseId',target_response.id,'revision',next_revision,'status','published');
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'seller_response',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_report_content(
  p_actor_party_id BIGINT,
  p_target_type TEXT,
  p_target_id UUID,
  p_reason TEXT,
  p_details TEXT,
  p_authorized_evidence JSONB,
  p_idempotency_key TEXT,
  p_environment TEXT DEFAULT 'production'
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  target_exists BOOLEAN;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  report_id UUID;
  case_id UUID;
  response JSONB;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM merch_reputation_feature_flag
      WHERE flag_key='moderation' AND environment=p_environment AND enabled) THEN
    RAISE EXCEPTION 'Merch review moderation is disabled';
  END IF;
  target_exists:=CASE p_target_type
    WHEN 'review' THEN EXISTS (SELECT 1 FROM merch_review WHERE id=p_target_id)
    WHEN 'seller_response' THEN EXISTS (SELECT 1 FROM merch_seller_response WHERE id=p_target_id)
    ELSE FALSE END;
  IF NOT target_exists THEN RAISE EXCEPTION 'Report target does not exist'; END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'targetType',p_target_type,
    'targetId',p_target_id,'reason',p_reason,'details',p_details,
    'evidence',p_authorized_evidence)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'report' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  INSERT INTO merch_reputation_report(
    target_type,target_id,reporter_party_id,reason,details,authorized_evidence
  ) VALUES (
    p_target_type,p_target_id,p_actor_party_id,p_reason,NULLIF(btrim(p_details),''),
    COALESCE(p_authorized_evidence,'[]'::jsonb)
  ) RETURNING id INTO report_id;
  INSERT INTO merch_reputation_moderation_case(report_id)
    VALUES (report_id) RETURNING id INTO case_id;
  response:=jsonb_build_object('reportId',report_id,'caseId',case_id,'status','submitted');
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'report',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_transition_moderation_case(
  p_actor_party_id BIGINT,
  p_case_id UUID,
  p_action TEXT,
  p_rationale TEXT,
  p_evidence_snapshot JSONB,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  case_row merch_reputation_moderation_case%ROWTYPE;
  report_row merch_reputation_report%ROWTYPE;
  review_row merch_review%ROWTYPE;
  seller_response_row merch_seller_response%ROWTYPE;
  target_store_id UUID;
  target_review_id UUID;
  prior_visibility TEXT;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  next_state TEXT;
  response JSONB;
BEGIN
  IF p_action NOT IN ('triage','request_evidence','provisionally_hide','resume_review')
    OR length(btrim(p_rationale)) NOT BETWEEN 20 AND 3000
    OR jsonb_typeof(p_evidence_snapshot)<>'object' THEN
    RAISE EXCEPTION 'Invalid moderation workflow transition';
  END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'case',p_case_id,
    'action',p_action,'rationale',p_rationale,'evidence',p_evidence_snapshot)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'moderation_transition' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO case_row FROM merch_reputation_moderation_case
    WHERE id=p_case_id FOR UPDATE;
  IF NOT FOUND OR case_row.state IN ('decided','appealed','closed') THEN
    RAISE EXCEPTION 'Moderation case cannot transition from its current state';
  END IF;
  SELECT * INTO report_row FROM merch_reputation_report WHERE id=case_row.report_id;
  IF report_row.target_type='review' THEN
    SELECT * INTO review_row FROM merch_review WHERE id=report_row.target_id FOR UPDATE;
    target_store_id:=review_row.store_id;
    target_review_id:=review_row.id;
    prior_visibility:=review_row.status;
  ELSE
    SELECT * INTO seller_response_row FROM merch_seller_response
      WHERE id=report_row.target_id FOR UPDATE;
    target_store_id:=seller_response_row.store_id;
    prior_visibility:=seller_response_row.status;
  END IF;
  IF p_action='triage' THEN
    IF case_row.state<>'open' THEN
      RAISE EXCEPTION 'Only an open moderation case can be triaged';
    END IF;
    next_state:='in_review';
    UPDATE merch_reputation_report SET status='triaged' WHERE id=report_row.id;
  ELSIF p_action='request_evidence' THEN
    IF case_row.state NOT IN ('open','in_review','awaiting_evidence') THEN
      RAISE EXCEPTION 'Evidence cannot be requested from the current moderation state';
    END IF;
    next_state:='awaiting_evidence';
    UPDATE merch_reputation_report SET status='in_review' WHERE id=report_row.id;
    INSERT INTO merch_reputation_notification_outbox(
      notification_key,recipient_party_id,notification_type,safe_payload
    )
    SELECT 'evidence-request:'||p_case_id||':'||recipient.party_id,
      recipient.party_id,'evidence_request',jsonb_build_object('caseId',p_case_id)
    FROM (
      SELECT report_row.reporter_party_id AS party_id
      UNION
      SELECT CASE WHEN report_row.target_type='review' THEN review_row.author_party_id
        ELSE store.owner_party_id END
      FROM merch_store store WHERE store.id=target_store_id
    ) recipient
    JOIN merch_reputation_notification_preference preference
      ON preference.party_id=recipient.party_id AND preference.evidence_request
    ON CONFLICT (notification_key) DO NOTHING;
  ELSIF p_action='provisionally_hide' THEN
    IF case_row.state NOT IN ('open','in_review','awaiting_evidence') THEN
      RAISE EXCEPTION 'Content cannot be provisionally hidden from the current moderation state';
    END IF;
    next_state:='provisionally_hidden';
    IF report_row.target_type='review' THEN
      UPDATE merch_review SET status='hidden' WHERE id=report_row.target_id;
    ELSE
      UPDATE merch_seller_response SET status='hidden' WHERE id=report_row.target_id;
    END IF;
  ELSE
    IF case_row.state<>'provisionally_hidden' OR case_row.provisional_previous_visibility IS NULL THEN
      RAISE EXCEPTION 'Only provisionally hidden content can resume review';
    END IF;
    next_state:='in_review';
    IF report_row.target_type='review' THEN
      UPDATE merch_review SET status=case_row.provisional_previous_visibility
        WHERE id=report_row.target_id;
    ELSE
      UPDATE merch_seller_response SET status=case_row.provisional_previous_visibility
        WHERE id=report_row.target_id;
    END IF;
  END IF;
  UPDATE merch_reputation_moderation_case SET state=next_state,
    assigned_to=COALESCE(assigned_to,p_actor_party_id),
    provisional_reason=CASE WHEN p_action='provisionally_hide' THEN p_rationale
      WHEN p_action='resume_review' THEN NULL ELSE provisional_reason END,
    provisional_previous_visibility=CASE
      WHEN p_action='provisionally_hide' THEN COALESCE(provisional_previous_visibility,prior_visibility)
      ELSE provisional_previous_visibility END,
    updated_at=NOW()
    WHERE id=p_case_id;
  INSERT INTO merch_reputation_audit_event(
    actor_party_id,action,record_type,record_id,reason,evidence
  ) VALUES (
    p_actor_party_id,'moderation_'||p_action,'moderation_case',p_case_id::TEXT,
    p_rationale,p_evidence_snapshot
  );
  IF p_action IN ('provisionally_hide','resume_review') THEN
    INSERT INTO merch_reputation_event(
      event_key,subject_kind,store_id,event_type,review_id,payload
    ) VALUES (
      'moderation-transition:'||p_idempotency_key,'store',target_store_id,
      CASE WHEN report_row.target_type='review' THEN 'review_visibility_changed'
        ELSE 'seller_response_changed' END,target_review_id,
      jsonb_build_object('caseId',p_case_id,'action',p_action)
    );
  END IF;
  response:=jsonb_build_object('caseId',p_case_id,'state',next_state,'action',p_action);
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'moderation_transition',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_decide_moderation(
  p_actor_party_id BIGINT,
  p_case_id UUID,
  p_decision TEXT,
  p_reason_code TEXT,
  p_rationale TEXT,
  p_evidence_snapshot JSONB,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  target_case merch_reputation_moderation_case%ROWTYPE;
  target_report merch_reputation_report%ROWTYPE;
  target_review merch_review%ROWTYPE;
  target_response merch_seller_response%ROWTYPE;
  decision_id UUID;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  new_visibility TEXT;
  target_store UUID;
  previous_visibility TEXT;
  response JSONB;
BEGIN
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'case',p_case_id,
    'decision',p_decision,'reasonCode',p_reason_code,'rationale',p_rationale,
    'evidence',p_evidence_snapshot)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'moderation_decision' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO target_case FROM merch_reputation_moderation_case
    WHERE id=p_case_id FOR UPDATE;
  IF NOT FOUND THEN RAISE EXCEPTION 'Moderation case does not exist'; END IF;
  IF target_case.state IN ('decided','appealed','closed') THEN
    RAISE EXCEPTION 'Moderation case is already decided';
  END IF;
  SELECT * INTO target_report FROM merch_reputation_report WHERE id=target_case.report_id;
  new_visibility:=CASE p_decision
    WHEN 'hide' THEN 'hidden' WHEN 'limit' THEN 'limited'
    ELSE NULL END;
  IF target_report.target_type='review' THEN
    SELECT * INTO target_review FROM merch_review WHERE id=target_report.target_id FOR UPDATE;
    target_store:=target_review.store_id;
    previous_visibility:=COALESCE(target_case.provisional_previous_visibility,target_review.status);
  ELSE
    SELECT * INTO target_response FROM merch_seller_response
      WHERE id=target_report.target_id FOR UPDATE;
    target_store:=target_response.store_id;
    previous_visibility:=COALESCE(target_case.provisional_previous_visibility,target_response.status);
  END IF;
  IF p_decision IN ('restore','approve','reject_report') THEN
    new_visibility:=previous_visibility;
  END IF;
  IF target_report.target_type='review' THEN
    UPDATE merch_review SET status=new_visibility WHERE id=target_review.id;
  ELSE
    UPDATE merch_seller_response SET status=new_visibility WHERE id=target_response.id;
  END IF;
  INSERT INTO merch_reputation_moderation_decision(
    case_id,decided_by,decision,reason_code,rationale,evidence_snapshot,previous_visibility
  ) VALUES (
    p_case_id,p_actor_party_id,p_decision,p_reason_code,p_rationale,p_evidence_snapshot,previous_visibility
  ) RETURNING id INTO decision_id;
  UPDATE merch_reputation_moderation_case SET state='decided',updated_at=NOW()
    WHERE id=p_case_id;
  UPDATE merch_reputation_report SET status='decided' WHERE id=target_report.id;
  INSERT INTO merch_reputation_audit_event(
    actor_party_id,action,record_type,record_id,reason,evidence
  ) VALUES (
    p_actor_party_id,'moderation_decided',target_report.target_type,
    target_report.target_id::TEXT,p_rationale,p_evidence_snapshot
  );
  INSERT INTO merch_reputation_event(
    event_key,subject_kind,store_id,event_type,review_id,payload
  ) VALUES (
    'moderation:'||decision_id,'store',target_store,'moderation_decided',
    CASE WHEN target_report.target_type='review' THEN target_report.target_id END,
    jsonb_build_object('decisionId',decision_id,'targetType',target_report.target_type)
  );
  INSERT INTO merch_reputation_notification_outbox(
    notification_key,recipient_party_id,notification_type,safe_payload
  )
  SELECT 'moderation-change:'||decision_id||':'||recipient.party_id,
    recipient.party_id,'moderation_change',
    jsonb_build_object('caseId',p_case_id,'decisionId',decision_id,'decision',p_decision)
  FROM (
    SELECT target_report.reporter_party_id AS party_id
    UNION
    SELECT CASE WHEN target_report.target_type='review' THEN target_review.author_party_id
      ELSE store.owner_party_id END
    FROM merch_store store WHERE store.id=target_store
  ) recipient
  JOIN merch_reputation_notification_preference preference
    ON preference.party_id=recipient.party_id AND preference.moderation_change
  ON CONFLICT (notification_key) DO NOTHING;
  response:=jsonb_build_object('decisionId',decision_id,'caseId',p_case_id,'state','decided');
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'moderation_decision',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_resolve_appeal(
  p_actor_party_id BIGINT,
  p_appeal_id UUID,
  p_outcome TEXT,
  p_rationale TEXT,
  p_evidence_snapshot JSONB,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  appeal_row merch_reputation_appeal%ROWTYPE;
  decision_row merch_reputation_moderation_decision%ROWTYPE;
  report_row merch_reputation_report%ROWTYPE;
  review_row merch_review%ROWTYPE;
  response_row merch_seller_response%ROWTYPE;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  target_store UUID;
  response JSONB;
BEGIN
  IF p_outcome NOT IN ('upheld','reversed')
    OR length(btrim(p_rationale)) NOT BETWEEN 20 AND 3000
    OR jsonb_typeof(p_evidence_snapshot)<>'object' THEN
    RAISE EXCEPTION 'Invalid appeal decision';
  END IF;
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,'appeal',p_appeal_id,
    'outcome',p_outcome,'rationale',p_rationale,'evidence',p_evidence_snapshot)::TEXT);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'appeal_decision' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO appeal_row FROM merch_reputation_appeal WHERE id=p_appeal_id FOR UPDATE;
  IF NOT FOUND OR appeal_row.state NOT IN ('open','awaiting_evidence') THEN
    RAISE EXCEPTION 'Appeal is not open';
  END IF;
  SELECT * INTO decision_row FROM merch_reputation_moderation_decision
    WHERE id=appeal_row.decision_id;
  IF p_actor_party_id IN (appeal_row.appellant_party_id,decision_row.decided_by) THEN
    RAISE EXCEPTION 'Appeal requires an independent reviewer';
  END IF;
  SELECT report.* INTO report_row FROM merch_reputation_moderation_case moderation_case
    JOIN merch_reputation_report report ON report.id=moderation_case.report_id
    WHERE moderation_case.id=decision_row.case_id;
  IF report_row.target_type='review' THEN
    SELECT * INTO review_row FROM merch_review WHERE id=report_row.target_id FOR UPDATE;
    target_store:=review_row.store_id;
    IF p_outcome='reversed' THEN
      UPDATE merch_review SET status=decision_row.previous_visibility WHERE id=review_row.id;
    END IF;
  ELSE
    SELECT * INTO response_row FROM merch_seller_response WHERE id=report_row.target_id FOR UPDATE;
    target_store:=response_row.store_id;
    IF p_outcome='reversed' THEN
      UPDATE merch_seller_response SET status=decision_row.previous_visibility WHERE id=response_row.id;
    END IF;
  END IF;
  UPDATE merch_reputation_appeal SET state=p_outcome,reviewed_by=p_actor_party_id,
    outcome_reason=p_rationale,decided_at=NOW() WHERE id=p_appeal_id;
  UPDATE merch_reputation_moderation_case SET state='closed',updated_at=NOW()
    WHERE id=decision_row.case_id;
  INSERT INTO merch_reputation_audit_event(
    actor_party_id,action,record_type,record_id,reason,evidence
  ) VALUES (
    p_actor_party_id,'moderation_appeal_'||p_outcome,'appeal',p_appeal_id::TEXT,
    p_rationale,p_evidence_snapshot
  );
  INSERT INTO merch_reputation_event(
    event_key,subject_kind,store_id,event_type,review_id,payload
  ) VALUES (
    'appeal-resolution:'||p_appeal_id,'store',target_store,'moderation_decided',
    CASE WHEN report_row.target_type='review' THEN report_row.target_id END,
    jsonb_build_object('appealId',p_appeal_id,'outcome',p_outcome)
  );
  INSERT INTO merch_reputation_notification_outbox(
    notification_key,recipient_party_id,notification_type,safe_payload
  ) SELECT 'appeal-result:'||p_appeal_id,appeal_row.appellant_party_id,'appeal_result',
      jsonb_build_object('appealId',p_appeal_id,'outcome',p_outcome)
    FROM merch_reputation_notification_preference preference
    WHERE preference.party_id=appeal_row.appellant_party_id AND preference.appeal_result
    ON CONFLICT (notification_key) DO NOTHING;
  response:=jsonb_build_object('appealId',p_appeal_id,'state',p_outcome,
    'restored',p_outcome='reversed');
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'appeal_decision',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_appeal_decision(
  p_actor_party_id BIGINT,
  p_decision_id UUID,
  p_grounds TEXT,
  p_idempotency_key TEXT
) RETURNS JSONB LANGUAGE plpgsql AS $$
DECLARE
  decision_row merch_reputation_moderation_decision%ROWTYPE;
  report_row merch_reputation_report%ROWTYPE;
  review_row merch_review%ROWTYPE;
  response_row merch_seller_response%ROWTYPE;
  request_hash TEXT;
  prior merch_reputation_idempotency%ROWTYPE;
  appeal_id UUID;
  authorized BOOLEAN DEFAULT FALSE;
  response JSONB;
BEGIN
  request_hash:=md5(jsonb_build_object('actor',p_actor_party_id,
    'decision',p_decision_id,'grounds',p_grounds)::text);
  PERFORM pg_advisory_xact_lock(hashtextextended(p_idempotency_key,0));
  SELECT * INTO prior FROM merch_reputation_idempotency WHERE idempotency_key=p_idempotency_key;
  IF FOUND THEN
    IF prior.actor_party_id IS DISTINCT FROM p_actor_party_id
      OR prior.action<>'appeal' OR prior.request_hash<>request_hash THEN
      RAISE EXCEPTION 'Idempotency key cannot be reused for a different request';
    END IF;
    RETURN prior.response;
  END IF;
  SELECT * INTO decision_row FROM merch_reputation_moderation_decision
    WHERE id=p_decision_id;
  SELECT report.* INTO report_row FROM merch_reputation_moderation_case moderation_case
    JOIN merch_reputation_report report ON report.id=moderation_case.report_id
    WHERE moderation_case.id=decision_row.case_id;
  authorized:=report_row.reporter_party_id=p_actor_party_id;
  IF report_row.target_type='review' THEN
    SELECT * INTO review_row FROM merch_review WHERE id=report_row.target_id;
    authorized:=authorized OR review_row.author_party_id=p_actor_party_id
      OR EXISTS (SELECT 1 FROM merch_store_member WHERE store_id=review_row.store_id
        AND party_id=p_actor_party_id AND status='active');
  ELSE
    SELECT * INTO response_row FROM merch_seller_response WHERE id=report_row.target_id;
    authorized:=authorized OR EXISTS (SELECT 1 FROM merch_store_member
      WHERE store_id=response_row.store_id AND party_id=p_actor_party_id AND status='active');
  END IF;
  IF NOT authorized THEN RAISE EXCEPTION 'Appeal is outside actor scope'; END IF;
  INSERT INTO merch_reputation_appeal(decision_id,appellant_party_id,grounds)
    VALUES (p_decision_id,p_actor_party_id,p_grounds) RETURNING id INTO appeal_id;
  UPDATE merch_reputation_moderation_case SET state='appealed',updated_at=NOW()
    WHERE id=decision_row.case_id;
  INSERT INTO merch_reputation_audit_event(
    actor_party_id,action,record_type,record_id,reason,evidence
  ) VALUES (
    p_actor_party_id,'moderation_appealed','moderation_decision',
    p_decision_id::TEXT,p_grounds,jsonb_build_object('appealId',appeal_id)
  );
  response:=jsonb_build_object('appealId',appeal_id,'decisionId',p_decision_id,'state','open');
  INSERT INTO merch_reputation_idempotency(idempotency_key,actor_party_id,action,request_hash,response)
    VALUES (p_idempotency_key,p_actor_party_id,'appeal',request_hash,response);
  RETURN response;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_process_events(
  p_limit INTEGER DEFAULT 100,
  p_environment TEXT DEFAULT 'production'
)
RETURNS TABLE(processed INTEGER, failed INTEGER) LANGUAGE plpgsql AS $$
DECLARE event_row merch_reputation_event%ROWTYPE; ok_count INTEGER:=0; fail_count INTEGER:=0;
BEGIN
  FOR event_row IN
    SELECT event.* FROM merch_reputation_event event
    LEFT JOIN merch_reputation_projection_checkpoint checkpoint ON checkpoint.event_id=event.id
    WHERE checkpoint.processed_at IS NULL
    ORDER BY event.recorded_at,event.id
    LIMIT greatest(1,least(p_limit,1000))
    FOR UPDATE OF event SKIP LOCKED
  LOOP
    INSERT INTO merch_reputation_projection_checkpoint(event_id,attempt_count)
      VALUES (event_row.id,0) ON CONFLICT (event_id) DO NOTHING;
    BEGIN
      PERFORM merch_reputation_rebuild_aggregate(
        event_row.subject_kind,COALESCE(event_row.product_id,event_row.store_id),NOW(),event_row.id
      );
      PERFORM merch_reputation_recalculate_badges(event_row.store_id,NOW(),p_environment);
      UPDATE merch_reputation_projection_checkpoint
        SET processed_at=NOW(),attempt_count=attempt_count+1,last_error=NULL,updated_at=NOW()
        WHERE event_id=event_row.id;
      ok_count:=ok_count+1;
    EXCEPTION WHEN OTHERS THEN
      UPDATE merch_reputation_projection_checkpoint
        SET attempt_count=attempt_count+1,last_error=left(SQLERRM,1000),updated_at=NOW()
        WHERE event_id=event_row.id;
      fail_count:=fail_count+1;
    END;
  END LOOP;
  RETURN QUERY SELECT ok_count,fail_count;
END $$;

CREATE OR REPLACE FUNCTION merch_reputation_enqueue_review_invitation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.fulfillment_state IN ('delivered','picked_up')
    AND OLD.fulfillment_state IS DISTINCT FROM NEW.fulfillment_state THEN
    INSERT INTO merch_reputation_notification_outbox(
      notification_key,recipient_party_id,notification_type,safe_payload,available_at
    )
    SELECT 'review-invitation:'||NEW.id,NEW.buyer_party_id,'review_invitation',
      jsonb_build_object('orderId',NEW.id,'storeId',NEW.store_id),NOW()
    FROM merch_reputation_notification_preference preference
    JOIN merch_reputation_feature_flag flag
      ON flag.flag_key='notifications'
      AND flag.environment=COALESCE(NULLIF(current_setting('tdf.runtime_environment',true),''),'production')
      AND flag.enabled
    WHERE preference.party_id=NEW.buyer_party_id AND preference.review_invitation
    ON CONFLICT (notification_key) DO NOTHING;
    INSERT INTO merch_reputation_notification_outbox(
      notification_key,recipient_party_id,notification_type,safe_payload,available_at
    )
    SELECT 'review-reminder:'||NEW.id,NEW.buyer_party_id,'review_reminder',
      jsonb_build_object('orderId',NEW.id,'storeId',NEW.store_id),NOW()+INTERVAL '7 days'
    FROM merch_reputation_notification_preference preference
    JOIN merch_reputation_feature_flag flag
      ON flag.flag_key='notifications'
      AND flag.environment=COALESCE(NULLIF(current_setting('tdf.runtime_environment',true),''),'production')
      AND flag.enabled
    WHERE preference.party_id=NEW.buyer_party_id AND preference.review_reminder
    ON CONFLICT (notification_key) DO NOTHING;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS merch_reputation_review_invitation_trigger ON merch_order;
CREATE TRIGGER merch_reputation_review_invitation_trigger
  AFTER UPDATE OF fulfillment_state ON merch_order
  FOR EACH ROW EXECUTE FUNCTION merch_reputation_enqueue_review_invitation();

CREATE OR REPLACE FUNCTION merch_reputation_search_contribution(
  p_store_id UUID,
  p_base_score NUMERIC,
  p_environment TEXT DEFAULT 'production'
) RETURNS NUMERIC LANGUAGE plpgsql STABLE AS $$
DECLARE cap NUMERIC; normalized NUMERIC; enabled BOOLEAN;
BEGIN
  SELECT flag.enabled INTO enabled FROM merch_reputation_feature_flag flag
    WHERE flag.flag_key='search_influence' AND flag.environment=p_environment;
  IF NOT COALESCE(enabled,FALSE) THEN RETURN 0; END IF;
  SELECT (parameters->>'rankingContributionCap')::NUMERIC INTO cap
    FROM merch_reputation_formula_version WHERE status='active'
    ORDER BY activated_at DESC LIMIT 1;
  SELECT greatest(0,least(1,(aggregate.public_rating-1)/4)) INTO normalized
    FROM merch_reputation_aggregate aggregate
    WHERE aggregate.subject_kind='store' AND aggregate.subject_id=p_store_id
      AND aggregate.publication_state='published'
    ORDER BY aggregate.calculated_through DESC LIMIT 1;
  RETURN least(COALESCE(p_base_score,0)*cap,COALESCE(p_base_score,0)*cap*COALESCE(normalized,0));
END $$;

CREATE OR REPLACE VIEW merch_reputation_metrics AS
SELECT
  count(DISTINCT orders.id) FILTER (
    WHERE orders.fulfillment_state IN ('delivered','picked_up') OR orders.order_state='cancelled'
  ) AS eligible_orders,
  count(DISTINCT review.order_id) FILTER (WHERE review.review_kind='store') AS reviewed_orders,
  percentile_cont(0.5) WITHIN GROUP (
    ORDER BY EXTRACT(EPOCH FROM (review.created_at-
      COALESCE(orders.delivered_at,orders.pickup_confirmed_at,orders.cancellation_resolved_at,orders.cancelled_at)))/3600
  ) FILTER (WHERE review.id IS NOT NULL) AS median_hours_to_review,
  count(DISTINCT report.id) AS reported_reviews,
  count(DISTINCT moderation_case.id) FILTER (WHERE moderation_case.state IN ('decided','closed')) AS moderated_cases,
  count(DISTINCT appeal.id) AS appeals,
  count(DISTINCT appeal.id) FILTER (WHERE appeal.state='reversed') AS reversed_appeals
FROM merch_order orders
LEFT JOIN merch_review review ON review.order_id=orders.id
LEFT JOIN merch_reputation_report report
  ON report.target_type='review' AND report.target_id=review.id
LEFT JOIN merch_reputation_moderation_case moderation_case ON moderation_case.report_id=report.id
LEFT JOIN merch_reputation_moderation_decision decision ON decision.case_id=moderation_case.id
LEFT JOIN merch_reputation_appeal appeal ON appeal.decision_id=decision.id;

CREATE OR REPLACE VIEW merch_reputation_projection_alerts AS
SELECT event.id AS event_id,event.event_type,event.recorded_at,
  checkpoint.attempt_count,checkpoint.last_error,
  CASE
    WHEN checkpoint.attempt_count>=3 THEN 'repeated_failure'
    WHEN event.recorded_at<NOW()-INTERVAL '15 minutes' AND checkpoint.processed_at IS NULL THEN 'stale_projection'
    ELSE 'pending'
  END AS alert_reason
FROM merch_reputation_event event
LEFT JOIN merch_reputation_projection_checkpoint checkpoint ON checkpoint.event_id=event.id
WHERE checkpoint.processed_at IS NULL;

COMMENT ON TABLE merch_reputation_aggregate IS
  'Commercial projections only; never joined mathematically into person, artist, band or community reputation.';
COMMENT ON COLUMN merch_review.order_id IS
  'Private verified-purchase evidence. Never expose publicly.';
COMMENT ON TABLE merch_reputation_operational_signal IS
  'Durable server-originated signals. Courier, buyer, platform and unknown responsibility are excluded from store scoring.';
COMMENT ON TABLE merch_reputation_risk_measure IS
  'Material measures require evidence; ratings alone cannot create a measure. Settlement holds require independent human review.';

COMMIT;
