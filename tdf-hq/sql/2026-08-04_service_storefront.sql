-- Migration: Service Storefront (Public Mixing & Mastering Orders)
-- Date: 2026-08-04
-- Description: Adds public-facing service order tables for the mixing/mastering
--              storefront, enabling artists to purchase services online with
--              Datafast/PayPal checkout.

BEGIN;

-- Service storefront packages (pricing tiers for public purchase)
CREATE TABLE IF NOT EXISTS service_storefront_package (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    service_kind TEXT NOT NULL, -- 'Mixing', 'Mastering', 'Bundle'
    tier TEXT NOT NULL, -- 'Basic', 'Pro', 'Premium'
    name TEXT NOT NULL,
    description TEXT,
    price_usd_cents INT NOT NULL,
    currency TEXT NOT NULL DEFAULT 'USD',
    turnaround_days INT NOT NULL DEFAULT 7,
    revision_count INT NOT NULL DEFAULT 2,
    deliverables TEXT, -- JSON array of deliverable descriptions
    features TEXT, -- JSON array of feature descriptions
    active BOOLEAN NOT NULL DEFAULT TRUE,
    sort_order INT NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_service_storefront_package_active 
    ON service_storefront_package(service_kind, active) WHERE active = TRUE;

-- Public service orders (customer-facing, distinct from internal service_order)
CREATE TABLE IF NOT EXISTS service_storefront_order (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    order_number TEXT UNIQUE NOT NULL,
    buyer_name TEXT NOT NULL,
    buyer_email TEXT NOT NULL,
    buyer_phone TEXT,
    artist_name TEXT, -- Band/artist name for the project
    package_id UUID NOT NULL REFERENCES service_storefront_package(id),
    service_kind TEXT NOT NULL,
    tier TEXT NOT NULL,
    price_usd_cents INT NOT NULL,
    currency TEXT NOT NULL DEFAULT 'USD',
    status TEXT NOT NULL DEFAULT 'pending_payment',
    -- Status workflow: pending_payment -> paid -> in_progress -> v1_delivered -> revisions -> approved -> delivered -> completed
    payment_provider TEXT,
    stripe_payment_intent_id TEXT,
    stripe_idempotency_key TEXT,
    datafast_checkout_id TEXT,
    datafast_resource_path TEXT,
    datafast_payment_id TEXT,
    paypal_order_id TEXT,
    paypal_payer_email TEXT,
    paid_at TIMESTAMPTZ,
    -- Service details
    genre TEXT,
    song_count INT NOT NULL DEFAULT 1,
    notes TEXT,
    reference_track_url TEXT,
    deadline DATE,
    -- File delivery
    source_files_url TEXT, -- Private storage URL for uploaded tracks
    deliverables_url TEXT, -- Private storage URL for delivered files
    -- Pipeline integration
    pipeline_card_id UUID, -- Links to existing PipelineCard for Kanban visibility
    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT uq_service_storefront_order_stripe_pi UNIQUE (stripe_payment_intent_id)
);

CREATE INDEX IF NOT EXISTS idx_service_storefront_order_email 
    ON service_storefront_order(buyer_email);
CREATE INDEX IF NOT EXISTS idx_service_storefront_order_status 
    ON service_storefront_order(status);
CREATE INDEX IF NOT EXISTS idx_service_storefront_order_created 
    ON service_storefront_order(created_at DESC);

-- Order status changes (audit trail)
CREATE TABLE IF NOT EXISTS service_storefront_order_status_change (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    order_id UUID NOT NULL REFERENCES service_storefront_order(id) ON DELETE CASCADE,
    status TEXT NOT NULL,
    notes TEXT,
    changed_by TEXT, -- Party ID or 'system'
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_service_storefront_order_status_change_order 
    ON service_storefront_order_status_change(order_id);

-- Revision requests
CREATE TABLE IF NOT EXISTS service_storefront_revision (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    order_id UUID NOT NULL REFERENCES service_storefront_order(id) ON DELETE CASCADE,
    revision_number INT NOT NULL,
    feedback TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'pending', -- pending, in_progress, completed
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    completed_at TIMESTAMPTZ,
    CONSTRAINT uq_revision_order_number UNIQUE (order_id, revision_number)
);

CREATE INDEX IF NOT EXISTS idx_service_storefront_revision_order 
    ON service_storefront_revision(order_id);

-- Trigger for updated_at
DO $$ BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_service_storefront_package_updated_at') THEN
        CREATE TRIGGER trg_service_storefront_package_updated_at 
            BEFORE UPDATE ON service_storefront_package 
            FOR EACH ROW EXECUTE PROCEDURE trigger_set_timestamp();
    END IF;
    IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_service_storefront_order_updated_at') THEN
        CREATE TRIGGER trg_service_storefront_order_updated_at 
            BEFORE UPDATE ON service_storefront_order 
            FOR EACH ROW EXECUTE PROCEDURE trigger_set_timestamp();
    END IF;
END $$;

-- Seed initial packages
INSERT INTO service_storefront_package (service_kind, tier, name, description, price_usd_cents, turnaround_days, revision_count, deliverables, features, sort_order)
VALUES 
    ('Mixing', 'Basic', 'Mezcla Básica', 
     'Mezcla profesional de hasta 8 pistas. Ideal para demos y proyectos independientes.', 
     8000, 5, 1, 
     '["Archivo WAV mezclado (44.1kHz/16-bit)", "1 revisión incluida"]',
     '["Hasta 8 pistas", "EQ, compresión, efectos básicos", "Entrega en 5 días", "1 revisión"]',
     1),
    ('Mixing', 'Pro', 'Mezcla Profesional', 
     'Mezcla profesional de hasta 24 pistas con efectos avanzados. Para artistas serios.', 
     15000, 7, 2,
     '["Archivo WAV mezclado (48kHz/24-bit)", "Stems por sección", "2 revisiones incluidas"]',
     '["Hasta 24 pistas", "EQ, compresión, reverb, delay avanzados", "Automatización detallada", "Entrega en 7 días", "2 revisiones"]',
     2),
    ('Mixing', 'Premium', 'Mezcla Premium', 
     'Mezcla de alta gama de hasta 48 pistas con procesamiento analógico emulado. Para lanzamientos profesionales.', 
     25000, 10, 3,
     '["Archivo WAV mezclado (96kHz/24-bit)", "Stems completos", "Instrumental y a cappella", "3 revisiones incluidas"]',
     '["Hasta 48 pistas", "Procesamiento analógico emulado", "Automatización avanzada", "Entrega en 10 días", "3 revisiones", "Soporte prioritario"]',
     3),
    ('Mastering', 'Basic', 'Mastering Básico', 
     'Mastering profesional para lanzamiento digital. Ideal para singles.', 
     4000, 3, 1,
     '["Archivo WAV masterizado (44.1kHz/16-bit)", "Versión para streaming", "1 revisión incluida"]',
     '["1 canción", "Loudness optimization", "Formato para Spotify/Apple Music", "Entrega en 3 días", "1 revisión"]',
     4),
    ('Mastering', 'Pro', 'Mastering Profesional', 
     'Mastering profesional con múltiples formatos de entrega. Para EPs y álbumes.', 
     7000, 5, 2,
     '["Archivo WAV masterizado (48kHz/24-bit)", "Versión para streaming", "Versión para CD", "2 revisiones incluidas"]',
     '["Hasta 3 canciones", "Loudness optimization avanzado", "Múltiples formatos de entrega", "Entrega en 5 días", "2 revisiones"]',
     5),
    ('Mastering', 'Premium', 'Mastering Premium', 
     'Mastering de alta gama con procesamiento analógico emulado. Para lanzamientos profesionales.', 
     12000, 7, 3,
     '["Archivo WAV masterizado (96kHz/24-bit)", "Todos los formatos digitales", "Versión para vinilo", "3 revisiones incluidas"]',
     '["Hasta 5 canciones", "Procesamiento analógico emulado", "Todos los formatos digitales + vinilo", "Entrega en 7 días", "3 revisiones", "Soporte prioritario"]',
     6),
    ('Bundle', 'Basic', 'Paquete Básico', 
     'Mezcla + Mastering básico. Ideal para singles independientes.', 
     11000, 7, 1,
     '["Archivo WAV mezclado y masterizado", "Versión para streaming", "1 revisión incluida"]',
     '["Mezcla de hasta 8 pistas", "Mastering de 1 canción", "Entrega en 7 días", "1 revisión"]',
     7),
    ('Bundle', 'Pro', 'Paquete Profesional', 
     'Mezcla + Mastering profesional. Para artistas serios que buscan calidad de radio.', 
     20000, 10, 2,
     '["Archivos WAV mezclados y masterizados (48kHz/24-bit)", "Stems", "Versión para streaming y CD", "2 revisiones incluidas"]',
     '["Mezcla de hasta 24 pistas", "Mastering de hasta 3 canciones", "Stems incluidos", "Entrega en 10 días", "2 revisiones"]',
     8),
    ('Bundle', 'Premium', 'Paquete Premium', 
     'Mezcla + Mastering de alta gama. Para lanzamientos profesionales de alta calidad.', 
     35000, 14, 3,
     '["Archivos WAV mezclados y masterizados (96kHz/24-bit)", "Stems completos", "Instrumental y a cappella", "Todos los formatos", "3 revisiones incluidas"]',
     '["Mezcla de hasta 48 pistas", "Mastering de hasta 5 canciones", "Procesamiento analógico emulado", "Todos los formatos + vinilo", "Entrega en 14 días", "3 revisiones", "Soporte prioritario"]',
     9);

COMMIT;
