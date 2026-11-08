BEGIN;

-- ADR 0132: marketplace de demanda y oferta sobre clasificados (Fase 1).
-- Aditivo y compatible: anuncios y postulaciones existentes siguen siendo
-- válidos; los defaults y backfills los reinterpretan como anuncios con un
-- cupo y ofertas con una sola ronda inicial.

-- ---------------------------------------------------------------------------
-- 1. Intención de la categoría: busco (seeking) u ofrezco (offering).
-- ---------------------------------------------------------------------------
ALTER TABLE classified_category
  ADD COLUMN IF NOT EXISTS intent TEXT NOT NULL DEFAULT 'seeking';

ALTER TABLE classified_category
  DROP CONSTRAINT IF EXISTS classified_category_intent_check;
ALTER TABLE classified_category
  ADD CONSTRAINT classified_category_intent_check
  CHECK (intent IN ('seeking','offering'));

UPDATE classified_category
SET intent = 'offering'
WHERE code IN ('offering-services','equipment-sale-rental','room-studio-available','classes','artist-profile')
  AND intent <> 'offering';

-- ---------------------------------------------------------------------------
-- 2. Anuncio: cupos, modo de precio, fecha límite de ofertas y términos.
-- ---------------------------------------------------------------------------
ALTER TABLE classified
  ADD COLUMN IF NOT EXISTS slots INTEGER NOT NULL DEFAULT 1,
  ADD COLUMN IF NOT EXISTS slots_remaining INTEGER NOT NULL DEFAULT 1,
  ADD COLUMN IF NOT EXISTS price_mode TEXT NOT NULL DEFAULT 'open',
  ADD COLUMN IF NOT EXISTS offer_deadline_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS terms JSONB NOT NULL DEFAULT '{}'::jsonb;

-- Backfill del modo de precio a partir del presupuesto ya publicado.
UPDATE classified
SET price_mode = CASE
  WHEN budget_max_minor IS NULL THEN 'open'
  WHEN budget_negotiable THEN 'suggested'
  WHEN budget_min_minor IS NULL OR budget_min_minor = budget_max_minor THEN 'fixed'
  ELSE 'suggested'
END
WHERE price_mode = 'open' AND budget_max_minor IS NOT NULL;

-- Cupos restantes: un cupo menos las ofertas ya aceptadas (nunca negativo).
UPDATE classified c
SET slots_remaining = GREATEST(0, c.slots - accepted.count)
FROM (
  SELECT classified_id, count(*)::integer AS count
  FROM classified_application
  WHERE status IN ('accepted','conversation_open','converted')
  GROUP BY classified_id
) accepted
WHERE accepted.classified_id = c.id;

ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_slots_check;
ALTER TABLE classified ADD CONSTRAINT classified_slots_check
  CHECK (slots BETWEEN 1 AND 100);
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_slots_remaining_check;
ALTER TABLE classified ADD CONSTRAINT classified_slots_remaining_check
  CHECK (slots_remaining BETWEEN 0 AND slots);
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_price_mode_check;
ALTER TABLE classified ADD CONSTRAINT classified_price_mode_check
  CHECK (price_mode IN ('open','suggested','fixed'));
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_price_mode_budget_check;
ALTER TABLE classified ADD CONSTRAINT classified_price_mode_budget_check
  CHECK (price_mode = 'open' OR (budget_max_minor IS NOT NULL AND currency_id IS NOT NULL));
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_price_mode_fixed_check;
ALTER TABLE classified ADD CONSTRAINT classified_price_mode_fixed_check
  CHECK (price_mode <> 'fixed' OR budget_min_minor IS NULL OR budget_min_minor = budget_max_minor);
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_offer_deadline_check;
ALTER TABLE classified ADD CONSTRAINT classified_offer_deadline_check
  CHECK (offer_deadline_at IS NULL OR expires_at IS NULL OR offer_deadline_at <= expires_at);
ALTER TABLE classified DROP CONSTRAINT IF EXISTS classified_terms_check;
ALTER TABLE classified ADD CONSTRAINT classified_terms_check
  CHECK (jsonb_typeof(terms) = 'object');

-- ---------------------------------------------------------------------------
-- 3. Oferta: estado actual de la negociación.
-- ---------------------------------------------------------------------------
ALTER TABLE classified_application
  ADD COLUMN IF NOT EXISTS current_amount_minor BIGINT,
  ADD COLUMN IF NOT EXISTS current_round INTEGER NOT NULL DEFAULT 1,
  ADD COLUMN IF NOT EXISTS last_actor_role TEXT NOT NULL DEFAULT 'applicant',
  ADD COLUMN IF NOT EXISTS terms JSONB NOT NULL DEFAULT '{}'::jsonb,
  ADD COLUMN IF NOT EXISTS offer_expires_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS accepted_amount_minor BIGINT,
  ADD COLUMN IF NOT EXISTS accepted_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS rejection_reason TEXT;

UPDATE classified_application
SET current_amount_minor = proposed_amount_minor
WHERE current_amount_minor IS NULL AND proposed_amount_minor IS NOT NULL;

UPDATE classified_application
SET accepted_amount_minor = coalesce(current_amount_minor, proposed_amount_minor),
    accepted_at = coalesce(accepted_at, updated_at)
WHERE status IN ('accepted','conversation_open','converted') AND accepted_at IS NULL;

UPDATE classified_application
SET rejection_reason = 'author_decision'
WHERE status = 'rejected' AND rejection_reason IS NULL;

-- Estados nuevos: countered_by_author, countered_by_applicant, expired.
ALTER TABLE classified_application
  DROP CONSTRAINT IF EXISTS classified_application_status_check;
ALTER TABLE classified_application
  ADD CONSTRAINT classified_application_status_check
  CHECK (status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant',
                    'accepted','rejected','withdrawn','expired','conversation_open','converted'));

ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_round_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_round_check
  CHECK (current_round BETWEEN 1 AND 10);
ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_actor_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_actor_check
  CHECK (last_actor_role IN ('applicant','author'));
ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_terms_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_terms_check
  CHECK (jsonb_typeof(terms) = 'object');
ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_current_amount_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_current_amount_check
  CHECK (current_amount_minor IS NULL OR (current_amount_minor >= 0 AND currency_id IS NOT NULL));
ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_accepted_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_accepted_check
  CHECK ((accepted_at IS NULL AND accepted_amount_minor IS NULL)
         OR status IN ('accepted','conversation_open','converted','withdrawn'));
ALTER TABLE classified_application DROP CONSTRAINT IF EXISTS classified_application_rejection_check;
ALTER TABLE classified_application ADD CONSTRAINT classified_application_rejection_check
  CHECK (rejection_reason IS NULL
         OR (status = 'rejected' AND rejection_reason IN ('author_decision','slots_filled','classified_closed')));

CREATE INDEX IF NOT EXISTS classified_application_open_idx
  ON classified_application (classified_id, status)
  WHERE status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant');
CREATE INDEX IF NOT EXISTS classified_application_expiry_idx
  ON classified_application (offer_expires_at)
  WHERE offer_expires_at IS NOT NULL
    AND status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant');

-- ---------------------------------------------------------------------------
-- 4. Rondas de negociación (propuesta inicial, contraofertas, aceptación).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS classified_offer_round (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  application_id UUID NOT NULL REFERENCES classified_application(id) ON DELETE CASCADE,
  round_number INTEGER NOT NULL,
  actor_role TEXT NOT NULL,
  kind TEXT NOT NULL,
  amount_minor BIGINT,
  currency_id UUID REFERENCES currency_reference(id),
  terms JSONB NOT NULL DEFAULT '{}'::jsonb,
  message TEXT,
  expires_at TIMESTAMPTZ,
  created_by_party_id BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (application_id, round_number),
  CHECK (round_number BETWEEN 1 AND 10),
  CHECK (actor_role IN ('applicant','author')),
  CHECK (kind IN ('initial','counter','accept')),
  CHECK (amount_minor IS NULL OR amount_minor >= 0),
  CHECK (amount_minor IS NULL OR currency_id IS NOT NULL),
  CHECK (jsonb_typeof(terms) = 'object'),
  CHECK (message IS NULL OR length(trim(message)) BETWEEN 1 AND 5000)
);
CREATE INDEX IF NOT EXISTS classified_offer_round_application_idx
  ON classified_offer_round (application_id, round_number);

-- Ronda inicial para cada oferta existente (idempotente).
INSERT INTO classified_offer_round
  (application_id, round_number, actor_role, kind, amount_minor, currency_id, terms, message, created_at)
SELECT application.id, 1, 'applicant', 'initial', application.proposed_amount_minor,
       application.currency_id, '{}'::jsonb, NULL, application.submitted_at
FROM classified_application application
WHERE NOT EXISTS (
  SELECT 1 FROM classified_offer_round existing
  WHERE existing.application_id = application.id AND existing.round_number = 1
);

-- ---------------------------------------------------------------------------
-- 5. Tipos de notificación del marketplace. Igual que 2026-09-18 y 2026-10-25:
--    se conserva la expresión previa y se amplía; instalaciones sin allowlist
--    siguen sin allowlist. La expresión original se guarda para el rollback.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_offers_migration_state (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  notification_expression TEXT
);
DO $$ DECLARE previous TEXT; BEGIN
  SELECT pg_get_expr(conbin, conrelid) INTO previous FROM pg_constraint
  WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check' AND contype = 'c';
  IF NOT EXISTS (SELECT 1 FROM marketplace_offers_migration_state) THEN
    INSERT INTO marketplace_offers_migration_state(id, notification_expression) VALUES (1, previous);
  END IF;
  IF previous IS NOT NULL AND position('marketplace.offer.new' in previous) = 0 THEN
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format(
      'ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''marketplace.offer.new'',''marketplace.offer.countered'',''marketplace.offer.accepted'',''marketplace.offer.rejected'',''marketplace.offer.withdrawn'',''marketplace.offer.expiring'',''marketplace.classified.filled''))',
      previous);
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 6. Diagnóstico (solo lectura): duplicados históricos de ofertas activas por
--    (anuncio, oferente). El handler rechaza nuevos duplicados con 409; los
--    históricos se revisan a mano, por eso no se crea un índice único aquí.
-- ---------------------------------------------------------------------------
DO $$
DECLARE duplicate_count INTEGER;
BEGIN
  SELECT count(*) INTO duplicate_count FROM (
    SELECT classified_id, applicant_profile_id
    FROM classified_application
    WHERE status NOT IN ('rejected','withdrawn','expired')
    GROUP BY classified_id, applicant_profile_id
    HAVING count(*) > 1
  ) duplicates;
  IF duplicate_count > 0 THEN
    RAISE NOTICE 'marketplace_offers: % (anuncio, oferente) con más de una oferta activa; revisar manualmente', duplicate_count;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 7. Gate de runtime: la negociación (contraofertas) queda apagada en
--    producción hasta habilitarla explícitamente; encendida en sandbox y
--    staging para pruebas.
-- ---------------------------------------------------------------------------
INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('marketplace.offers', FALSE, 'production', 'ADR 0132: enable after Phase 1 manual gates and moderation review'),
  ('marketplace.offers', TRUE, 'staging', 'ADR 0132: Phase 1 rehearsal'),
  ('marketplace.offers', TRUE, 'sandbox', 'ADR 0132: local development')
ON CONFLICT (flag_key, environment) DO NOTHING;

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.apply', 'migration', '2026-11-08_marketplace_offers', NULL, 'applied',
        'migration-2026-11-08-marketplace-offers', '{"adr":"0132"}'::jsonb);

COMMIT;
