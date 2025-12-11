-- TDF Records Platform - Complete Database Schema
-- Generated for clean deployment without migrations
-- PostgreSQL 16+

-- Enable extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- ============================================================================
-- CORE PARTY & IDENTITY TABLES
-- ============================================================================

CREATE TABLE IF NOT EXISTS party (
    id                BIGSERIAL PRIMARY KEY,
    legal_name        TEXT,
    display_name      TEXT NOT NULL,
    is_org            BOOLEAN NOT NULL DEFAULT FALSE,
    tax_id            TEXT,
    primary_email     TEXT,
    primary_phone     TEXT,
    whatsapp          TEXT,
    instagram         TEXT,
    emergency_contact TEXT,
    notes             TEXT,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS party_role (
    id        BIGSERIAL PRIMARY KEY,
    party_id  BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    role      TEXT NOT NULL,
    active    BOOLEAN NOT NULL DEFAULT TRUE,
    CONSTRAINT unique_party_role UNIQUE (party_id, role)
);

CREATE INDEX idx_party_role_party ON party_role(party_id);
CREATE INDEX idx_party_role_active ON party_role(party_id, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS user_credential (
    id            BIGSERIAL PRIMARY KEY,
    party_id      BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    username      TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    active        BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_user_credential_party ON user_credential(party_id);
CREATE INDEX idx_user_credential_active ON user_credential(username, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS api_token (
    id       BIGSERIAL PRIMARY KEY,
    token    TEXT NOT NULL UNIQUE,
    party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    label    TEXT,
    active   BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_api_token_party ON api_token(party_id);
CREATE INDEX idx_api_token_lookup ON api_token(token) WHERE active = TRUE;

-- ============================================================================
-- ARTIST & FAN PROFILES
-- ============================================================================

CREATE TABLE IF NOT EXISTS artist_profile (
    id                 BIGSERIAL PRIMARY KEY,
    artist_party_id    BIGINT NOT NULL UNIQUE REFERENCES party(id) ON DELETE CASCADE,
    slug               TEXT,
    bio                TEXT,
    city               TEXT,
    hero_image_url     TEXT,
    spotify_artist_id  TEXT,
    spotify_url        TEXT,
    youtube_channel_id TEXT,
    youtube_url        TEXT,
    website_url        TEXT,
    featured_video_url TEXT,
    genres             TEXT,
    highlights         TEXT,
    created_at         TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at         TIMESTAMPTZ
);

CREATE INDEX idx_artist_profile_slug ON artist_profile(slug);

CREATE TABLE IF NOT EXISTS artist_release (
    id              BIGSERIAL PRIMARY KEY,
    artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    title           TEXT NOT NULL,
    release_date    DATE,
    description     TEXT,
    cover_image_url TEXT,
    spotify_url     TEXT,
    youtube_url     TEXT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_artist_release_artist ON artist_release(artist_party_id);

CREATE TABLE IF NOT EXISTS fan_profile (
    id              BIGSERIAL PRIMARY KEY,
    fan_party_id    BIGINT NOT NULL UNIQUE REFERENCES party(id) ON DELETE CASCADE,
    display_name    TEXT,
    avatar_url      TEXT,
    favorite_genres TEXT,
    bio             TEXT,
    city            TEXT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS fan_follow (
    id              BIGSERIAL PRIMARY KEY,
    fan_party_id    BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_fan_follow UNIQUE (fan_party_id, artist_party_id)
);

CREATE INDEX idx_fan_follow_fan ON fan_follow(fan_party_id);
CREATE INDEX idx_fan_follow_artist ON fan_follow(artist_party_id);

-- ============================================================================
-- SERVICE CATALOG & ORDERS
-- ============================================================================

CREATE TABLE IF NOT EXISTS service_catalog (
    id                  BIGSERIAL PRIMARY KEY,
    name                TEXT NOT NULL,
    kind                TEXT NOT NULL,
    pricing_model       TEXT NOT NULL,
    default_rate_cents  INT,
    tax_bps             INT,
    active              BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_service_catalog_active ON service_catalog(kind, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS service_order (
    id                 BIGSERIAL PRIMARY KEY,
    customer_id        BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
    artist_id          BIGINT REFERENCES party(id) ON DELETE SET NULL,
    catalog_id         BIGINT NOT NULL REFERENCES service_catalog(id) ON DELETE RESTRICT,
    service_kind       TEXT NOT NULL,
    title              TEXT,
    description        TEXT,
    status             TEXT NOT NULL DEFAULT 'draft',
    price_quoted_cents INT,
    quote_sent_at      TIMESTAMPTZ,
    scheduled_start    TIMESTAMPTZ,
    scheduled_end      TIMESTAMPTZ,
    created_at         TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_service_order_customer ON service_order(customer_id);
CREATE INDEX idx_service_order_artist ON service_order(artist_id);
CREATE INDEX idx_service_order_status ON service_order(status);

CREATE TABLE IF NOT EXISTS service_status_change (
    id               BIGSERIAL PRIMARY KEY,
    service_order_id BIGINT NOT NULL REFERENCES service_order(id) ON DELETE CASCADE,
    status           TEXT NOT NULL,
    notes            TEXT,
    changed_by       BIGINT REFERENCES party(id) ON DELETE SET NULL,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_service_status_order ON service_status_change(service_order_id);

-- ============================================================================
-- RESOURCES & BOOKINGS
-- ============================================================================

CREATE TABLE IF NOT EXISTS resource (
    id            BIGSERIAL PRIMARY KEY,
    name          TEXT NOT NULL,
    slug          TEXT NOT NULL UNIQUE,
    resource_type TEXT NOT NULL,
    capacity      INT,
    active        BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_resource_type ON resource(resource_type, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS booking (
    id               BIGSERIAL PRIMARY KEY,
    title            TEXT NOT NULL,
    service_order_id BIGINT REFERENCES service_order(id) ON DELETE SET NULL,
    party_id         BIGINT REFERENCES party(id) ON DELETE SET NULL,
    service_type     TEXT,
    starts_at        TIMESTAMPTZ NOT NULL,
    ends_at          TIMESTAMPTZ NOT NULL,
    status           TEXT NOT NULL DEFAULT 'Tentative',
    created_by       BIGINT REFERENCES party(id) ON DELETE SET NULL,
    notes            TEXT,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_booking_time ON booking(starts_at, ends_at);
CREATE INDEX idx_booking_status ON booking(status);
CREATE INDEX idx_booking_party ON booking(party_id);

CREATE TABLE IF NOT EXISTS booking_resource (
    id          BIGSERIAL PRIMARY KEY,
    booking_id  BIGINT NOT NULL REFERENCES booking(id) ON DELETE CASCADE,
    resource_id BIGINT NOT NULL REFERENCES resource(id) ON DELETE CASCADE,
    role        TEXT NOT NULL,
    CONSTRAINT unique_booking_resource UNIQUE (booking_id, resource_id, role)
);

CREATE INDEX idx_booking_resource_booking ON booking_resource(booking_id);
CREATE INDEX idx_booking_resource_resource ON booking_resource(resource_id);

CREATE TABLE IF NOT EXISTS attendance (
    id         BIGSERIAL PRIMARY KEY,
    booking_id BIGINT NOT NULL REFERENCES booking(id) ON DELETE CASCADE,
    party_id   BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    status     TEXT NOT NULL DEFAULT 'Enrolled',
    notes      TEXT,
    CONSTRAINT unique_attendance UNIQUE (booking_id, party_id)
);

CREATE INDEX idx_attendance_booking ON attendance(booking_id);
CREATE INDEX idx_attendance_party ON attendance(party_id);

-- ============================================================================
-- PACKAGES & LEDGER
-- ============================================================================

CREATE TABLE IF NOT EXISTS package_product (
    id            BIGSERIAL PRIMARY KEY,
    name          TEXT NOT NULL,
    service_kind  TEXT NOT NULL,
    units_kind    TEXT NOT NULL,
    units_qty     INT NOT NULL,
    price_cents   INT NOT NULL,
    expires_days  INT,
    transferable  BOOLEAN NOT NULL DEFAULT FALSE,
    refund_policy TEXT NOT NULL DEFAULT 'None',
    active        BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_package_product_active ON package_product(service_kind, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS package_purchase (
    id              BIGSERIAL PRIMARY KEY,
    buyer_id        BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
    product_id      BIGINT NOT NULL REFERENCES package_product(id) ON DELETE RESTRICT,
    purchased_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    price_cents     INT NOT NULL,
    expires_at      TIMESTAMPTZ,
    remaining_units INT NOT NULL,
    status          TEXT NOT NULL DEFAULT 'active'
);

CREATE INDEX idx_package_purchase_buyer ON package_purchase(buyer_id);
CREATE INDEX idx_package_purchase_status ON package_purchase(buyer_id, status) WHERE status = 'active';

CREATE TABLE IF NOT EXISTS package_ledger (
    id           BIGSERIAL PRIMARY KEY,
    purchase_id  BIGINT NOT NULL REFERENCES package_purchase(id) ON DELETE CASCADE,
    booking_id   BIGINT REFERENCES booking(id) ON DELETE SET NULL,
    delta_units  INT NOT NULL,
    notes        TEXT,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_package_ledger_purchase ON package_ledger(purchase_id);
CREATE INDEX idx_package_ledger_booking ON package_ledger(booking_id);

-- ============================================================================
-- INVOICING & PAYMENTS
-- ============================================================================

CREATE TABLE IF NOT EXISTS invoice (
    id              BIGSERIAL PRIMARY KEY,
    customer_id     BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
    issue_date      DATE NOT NULL,
    due_date        DATE NOT NULL,
    number          TEXT UNIQUE,
    status          TEXT NOT NULL DEFAULT 'Draft',
    currency        TEXT NOT NULL DEFAULT 'USD',
    subtotal_cents  INT NOT NULL,
    tax_cents       INT NOT NULL,
    total_cents     INT NOT NULL,
    sri_document_id TEXT,
    notes           TEXT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_invoice_customer ON invoice(customer_id);
CREATE INDEX idx_invoice_status ON invoice(status);
CREATE INDEX idx_invoice_number ON invoice(number);

CREATE TABLE IF NOT EXISTS invoice_line (
    id                  BIGSERIAL PRIMARY KEY,
    invoice_id          BIGINT NOT NULL REFERENCES invoice(id) ON DELETE CASCADE,
    service_order_id    BIGINT REFERENCES service_order(id) ON DELETE SET NULL,
    package_purchase_id BIGINT REFERENCES package_purchase(id) ON DELETE SET NULL,
    description         TEXT NOT NULL,
    quantity            INT NOT NULL,
    unit_cents          INT NOT NULL,
    tax_bps             INT NOT NULL,
    total_cents         INT NOT NULL
);

CREATE INDEX idx_invoice_line_invoice ON invoice_line(invoice_id);

CREATE TABLE IF NOT EXISTS receipt (
    id             BIGSERIAL PRIMARY KEY,
    invoice_id     BIGINT NOT NULL REFERENCES invoice(id) ON DELETE RESTRICT,
    number         TEXT NOT NULL UNIQUE,
    issue_date     DATE NOT NULL,
    issued_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    buyer_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
    buyer_name     TEXT NOT NULL,
    buyer_email    TEXT,
    currency       TEXT NOT NULL DEFAULT 'USD',
    subtotal_cents INT NOT NULL,
    tax_cents      INT NOT NULL,
    total_cents    INT NOT NULL,
    notes          TEXT,
    created_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_receipt_invoice ON receipt(invoice_id);
CREATE INDEX idx_receipt_number ON receipt(number);

CREATE TABLE IF NOT EXISTS receipt_line (
    id          BIGSERIAL PRIMARY KEY,
    receipt_id  BIGINT NOT NULL REFERENCES receipt(id) ON DELETE CASCADE,
    description TEXT NOT NULL,
    quantity    INT NOT NULL,
    unit_cents  INT NOT NULL,
    tax_bps     INT,
    total_cents INT NOT NULL
);

CREATE INDEX idx_receipt_line_receipt ON receipt_line(receipt_id);

CREATE TABLE IF NOT EXISTS payment (
    id           BIGSERIAL PRIMARY KEY,
    invoice_id   BIGINT NOT NULL REFERENCES invoice(id) ON DELETE RESTRICT,
    method       TEXT NOT NULL,
    amount_cents INT NOT NULL,
    received_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    reference    TEXT,
    created_by   BIGINT REFERENCES party(id) ON DELETE SET NULL
);

CREATE INDEX idx_payment_invoice ON payment(invoice_id);

CREATE TABLE IF NOT EXISTS payment_split (
    id           BIGSERIAL PRIMARY KEY,
    payment_id   BIGINT NOT NULL REFERENCES payment(id) ON DELETE CASCADE,
    payer_id     BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
    amount_cents INT NOT NULL
);

CREATE INDEX idx_payment_split_payment ON payment_split(payment_id);
CREATE INDEX idx_payment_split_payer ON payment_split(payer_id);

-- ============================================================================
-- CALENDAR INTEGRATION
-- ============================================================================

CREATE TABLE IF NOT EXISTS external_calendar_mapping (
    id                 BIGSERIAL PRIMARY KEY,
    resource_id        BIGINT NOT NULL UNIQUE REFERENCES resource(id) ON DELETE CASCADE,
    google_calendar_id TEXT NOT NULL,
    direction          TEXT NOT NULL DEFAULT 'both'
);

-- ============================================================================
-- LEADS & WHATSAPP (Course Registration)
-- ============================================================================

CREATE TABLE IF NOT EXISTS course_edition (
    id               SERIAL PRIMARY KEY,
    slug             TEXT UNIQUE NOT NULL,
    name             TEXT NOT NULL,
    registration_url TEXT,
    starts_on        DATE NOT NULL,
    ends_on          DATE NOT NULL,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS lead (
    id                BIGSERIAL PRIMARY KEY,
    phone_e164        TEXT NOT NULL,
    display_name      TEXT,
    email             TEXT,
    source            TEXT NOT NULL DEFAULT 'whatsapp',
    course_edition_id INTEGER NOT NULL REFERENCES course_edition(id) ON DELETE RESTRICT,
    status            TEXT NOT NULL DEFAULT 'NEW',
    token             TEXT NOT NULL,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX idx_lead_token ON lead(token);
CREATE INDEX idx_lead_course ON lead(course_edition_id);
CREATE INDEX idx_lead_phone ON lead(phone_e164);

CREATE TABLE IF NOT EXISTS whatsapp_message_log (
    id          BIGSERIAL PRIMARY KEY,
    direction   TEXT NOT NULL,
    wa_msg_id   TEXT,
    phone_e164  TEXT NOT NULL,
    payload     JSONB NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_whatsapp_log_phone ON whatsapp_message_log(phone_e164);
CREATE INDEX idx_whatsapp_log_time ON whatsapp_message_log(created_at DESC);

-- ============================================================================
-- AUDIT LOG
-- ============================================================================

CREATE TABLE IF NOT EXISTS audit_log (
    id         BIGSERIAL PRIMARY KEY,
    actor_id   BIGINT REFERENCES party(id) ON DELETE SET NULL,
    entity     TEXT NOT NULL,
    entity_id  TEXT NOT NULL,
    action     TEXT NOT NULL,
    diff       TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_audit_log_entity ON audit_log(entity, entity_id);
CREATE INDEX idx_audit_log_time ON audit_log(created_at DESC);
CREATE INDEX idx_audit_log_actor ON audit_log(actor_id);

-- ============================================================================
-- EXTENDED MODELS (UUID-based tables)
-- ============================================================================

CREATE TABLE IF NOT EXISTS dropdown_option (
    id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    category   TEXT NOT NULL,
    value      TEXT NOT NULL,
    label      TEXT,
    active     BOOLEAN NOT NULL DEFAULT TRUE,
    sort_order INT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_dropdown_option UNIQUE (category, value)
);

CREATE INDEX idx_dropdown_category ON dropdown_option(category, active) WHERE active = TRUE;

CREATE TABLE IF NOT EXISTS room (
    id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name                 TEXT NOT NULL UNIQUE,
    is_bookable          BOOLEAN NOT NULL DEFAULT TRUE,
    capacity             INT,
    channel_count        INT,
    default_sample_rate  INT,
    patchbay_notes       TEXT
);

CREATE TABLE IF NOT EXISTS room_feature (
    id      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    room_id UUID NOT NULL REFERENCES room(id) ON DELETE CASCADE,
    key     TEXT NOT NULL,
    value   TEXT NOT NULL,
    CONSTRAINT unique_room_feature UNIQUE (room_id, key)
);

CREATE INDEX idx_room_feature_room ON room_feature(room_id);

-- ============================================================================
-- ASSETS & INVENTORY
-- ============================================================================

CREATE TABLE IF NOT EXISTS asset (
    id                      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name                    TEXT NOT NULL,
    category                TEXT NOT NULL,
    brand                   TEXT,
    model                   TEXT,
    serial_number           TEXT,
    purchase_date           DATE,
    purchase_price_usd_cents INT,
    condition               TEXT NOT NULL DEFAULT 'Good',
    status                  TEXT NOT NULL DEFAULT 'Active',
    location_id             UUID REFERENCES room(id) ON DELETE SET NULL,
    owner                   TEXT NOT NULL DEFAULT 'TDF',
    qr_code                 TEXT,
    photo_url               TEXT,
    notes                   TEXT,
    warranty_expires        DATE,
    maintenance_policy      TEXT NOT NULL DEFAULT 'None',
    next_maintenance_due    DATE,
    CONSTRAINT unique_asset_serial UNIQUE (serial_number),
    CONSTRAINT unique_asset_qr UNIQUE (qr_code)
);

CREATE INDEX idx_asset_category ON asset(category);
CREATE INDEX idx_asset_status ON asset(status);
CREATE INDEX idx_asset_location ON asset(location_id);

CREATE TABLE IF NOT EXISTS room_default_gear (
    id       UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    room_id  UUID NOT NULL REFERENCES room(id) ON DELETE CASCADE,
    asset_id UUID NOT NULL REFERENCES asset(id) ON DELETE CASCADE,
    CONSTRAINT unique_room_default_gear UNIQUE (room_id, asset_id)
);

CREATE INDEX idx_room_default_gear_room ON room_default_gear(room_id);

CREATE TABLE IF NOT EXISTS asset_kit_member (
    id        UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    kit_id    UUID NOT NULL REFERENCES asset(id) ON DELETE CASCADE,
    member_id UUID NOT NULL REFERENCES asset(id) ON DELETE CASCADE,
    qty       INT NOT NULL DEFAULT 1,
    CONSTRAINT unique_kit_member UNIQUE (kit_id, member_id)
);

CREATE INDEX idx_asset_kit_member_kit ON asset_kit_member(kit_id);

CREATE TABLE IF NOT EXISTS asset_checkout (
    id                  UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    asset_id            UUID NOT NULL REFERENCES asset(id) ON DELETE RESTRICT,
    target_kind         TEXT NOT NULL,
    target_session_id   UUID,
    target_party_ref    TEXT,
    target_room_id      UUID REFERENCES room(id) ON DELETE SET NULL,
    checked_out_by_ref  TEXT NOT NULL,
    checked_out_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    due_at              TIMESTAMPTZ,
    condition_out       TEXT,
    photo_drive_file_id TEXT,
    returned_at         TIMESTAMPTZ,
    condition_in        TEXT,
    notes               TEXT
);

CREATE INDEX idx_asset_checkout_asset ON asset_checkout(asset_id);
CREATE INDEX idx_asset_checkout_returned ON asset_checkout(asset_id, returned_at) WHERE returned_at IS NULL;

CREATE TABLE IF NOT EXISTS asset_audit (
    id       UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    asset_id UUID NOT NULL REFERENCES asset(id) ON DELETE CASCADE,
    at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    event    TEXT NOT NULL,
    detail   TEXT
);

CREATE INDEX idx_asset_audit_asset ON asset_audit(asset_id);

CREATE TABLE IF NOT EXISTS maintenance_ticket (
    id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    asset_id          UUID NOT NULL REFERENCES asset(id) ON DELETE RESTRICT,
    status            TEXT NOT NULL,
    opened_at         TIMESTAMPTZ NOT NULL DEFAULT now(),
    closed_at         TIMESTAMPTZ,
    vendor_party_ref  TEXT,
    summary           TEXT NOT NULL,
    details           TEXT
);

CREATE INDEX idx_maintenance_ticket_asset ON maintenance_ticket(asset_id);
CREATE INDEX idx_maintenance_ticket_status ON maintenance_ticket(status);

CREATE TABLE IF NOT EXISTS maintenance_attachment (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    ticket_id     UUID NOT NULL REFERENCES maintenance_ticket(id) ON DELETE CASCADE,
    drive_file_id TEXT NOT NULL,
    label         TEXT
);

CREATE INDEX idx_maintenance_attachment_ticket ON maintenance_attachment(ticket_id);

-- ============================================================================
-- STOCK MANAGEMENT
-- ============================================================================

CREATE TABLE IF NOT EXISTS stock_item (
    id               UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name             TEXT NOT NULL,
    sku              TEXT NOT NULL UNIQUE,
    unit             TEXT NOT NULL DEFAULT 'Pcs',
    bin_location     TEXT,
    on_hand          INT NOT NULL DEFAULT 0,
    reorder_point    INT,
    vendor_party_ref TEXT,
    notes            TEXT
);

CREATE INDEX idx_stock_item_sku ON stock_item(sku);

CREATE TABLE IF NOT EXISTS stock_movement (
    id               UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    stock_item_id    UUID NOT NULL REFERENCES stock_item(id) ON DELETE RESTRICT,
    change_qty       INT NOT NULL,
    reason           TEXT NOT NULL DEFAULT 'OtherMove',
    at               TIMESTAMPTZ NOT NULL DEFAULT now(),
    ref_checkout_id  UUID REFERENCES asset_checkout(id) ON DELETE SET NULL,
    ref_session_id   UUID,
    notes            TEXT
);

CREATE INDEX idx_stock_movement_item ON stock_movement(stock_item_id);
CREATE INDEX idx_stock_movement_time ON stock_movement(at DESC);

-- ============================================================================
-- BANDS & MUSICIANS
-- ============================================================================

CREATE TABLE IF NOT EXISTS band (
    id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    party_id       BIGINT NOT NULL UNIQUE REFERENCES party(id) ON DELETE CASCADE,
    name           TEXT NOT NULL UNIQUE,
    label_artist   BOOLEAN NOT NULL DEFAULT FALSE,
    primary_genre  TEXT,
    home_city      TEXT,
    photo_url      TEXT,
    contract_flags TEXT
);

CREATE INDEX idx_band_name ON band(name);

CREATE TABLE IF NOT EXISTS band_member (
    id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    band_id      UUID NOT NULL REFERENCES band(id) ON DELETE CASCADE,
    party_id     BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    role_in_band TEXT,
    CONSTRAINT unique_band_member UNIQUE (band_id, party_id)
);

CREATE INDEX idx_band_member_band ON band_member(band_id);
CREATE INDEX idx_band_member_party ON band_member(party_id);

CREATE TABLE IF NOT EXISTS live_session_intake (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    band_name     TEXT NOT NULL,
    contact_email TEXT,
    contact_phone TEXT,
    session_date  DATE,
    rider_path    TEXT,
    created_by    BIGINT REFERENCES party(id) ON DELETE SET NULL,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS live_session_musician (
    id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    intake_id   UUID NOT NULL REFERENCES live_session_intake(id) ON DELETE CASCADE,
    party_id    BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    name        TEXT NOT NULL,
    email       TEXT,
    instrument  TEXT,
    role        TEXT,
    notes       TEXT,
    is_existing BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE INDEX idx_live_session_musician_intake ON live_session_musician(intake_id);

-- ============================================================================
-- SESSIONS & DELIVERABLES
-- ============================================================================

CREATE TABLE IF NOT EXISTS session (
    id                        UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    booking_ref               TEXT,
    band_id                   UUID REFERENCES band(id) ON DELETE SET NULL,
    client_party_ref          TEXT,
    service                   TEXT NOT NULL,
    start_at                  TIMESTAMPTZ NOT NULL,
    end_at                    TIMESTAMPTZ NOT NULL,
    engineer_ref              TEXT NOT NULL,
    assistant_ref             TEXT,
    status                    TEXT NOT NULL DEFAULT 'InPrep',
    sample_rate               INT,
    bit_depth                 INT,
    daw                       TEXT,
    session_folder_drive_id   TEXT,
    notes                     TEXT
);

CREATE INDEX idx_session_time ON session(start_at);
CREATE INDEX idx_session_band ON session(band_id);

CREATE TABLE IF NOT EXISTS session_room (
    id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID NOT NULL REFERENCES session(id) ON DELETE CASCADE,
    room_id    UUID NOT NULL REFERENCES room(id) ON DELETE CASCADE,
    CONSTRAINT unique_session_room UNIQUE (session_id, room_id)
);

CREATE INDEX idx_session_room_session ON session_room(session_id);

CREATE TABLE IF NOT EXISTS session_deliverable (
    id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id   UUID NOT NULL REFERENCES session(id) ON DELETE CASCADE,
    kind         TEXT NOT NULL,
    name         TEXT NOT NULL,
    drive_file_id TEXT,
    external_url TEXT,
    delivered_at TIMESTAMPTZ,
    approved_at  TIMESTAMPTZ,
    notes        TEXT
);

CREATE INDEX idx_session_deliverable_session ON session_deliverable(session_id);

-- ============================================================================
-- INPUT LISTS
-- ============================================================================

CREATE TABLE IF NOT EXISTS input_list_template (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name          TEXT NOT NULL,
    genre         TEXT,
    channel_count INT,
    notes         TEXT
);

CREATE TABLE IF NOT EXISTS input_list_template_row (
    id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    template_id          UUID NOT NULL REFERENCES input_list_template(id) ON DELETE CASCADE,
    channel_number       INT NOT NULL,
    track_name           TEXT,
    instrument           TEXT,
    mic_id               UUID REFERENCES asset(id) ON DELETE SET NULL,
    stand_id             UUID REFERENCES asset(id) ON DELETE SET NULL,
    cable_id             UUID REFERENCES asset(id) ON DELETE SET NULL,
    preamp_id            UUID REFERENCES asset(id) ON DELETE SET NULL,
    insert_outboard_id   UUID REFERENCES asset(id) ON DELETE SET NULL,
    converter_channel    TEXT,
    phantom              BOOLEAN,
    polarity             BOOLEAN,
    hpf                  BOOLEAN,
    pad                  BOOLEAN,
    notes                TEXT,
    CONSTRAINT unique_template_channel UNIQUE (template_id, channel_number)
);

CREATE INDEX idx_input_list_template_row_template ON input_list_template_row(template_id);

CREATE TABLE IF NOT EXISTS input_list (
    id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID NOT NULL REFERENCES session(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS input_list_version (
    id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    input_list_id  UUID NOT NULL REFERENCES input_list(id) ON DELETE CASCADE,
    version        INT NOT NULL,
    created_at     TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by_ref TEXT,
    notes          TEXT,
    CONSTRAINT unique_list_version UNIQUE (input_list_id, version)
);

CREATE INDEX idx_input_list_version_list ON input_list_version(input_list_id);

CREATE TABLE IF NOT EXISTS input_row (
    id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    version_id           UUID NOT NULL REFERENCES input_list_version(id) ON DELETE CASCADE,
    channel_number       INT NOT NULL,
    track_name           TEXT,
    instrument           TEXT,
    mic_id               UUID REFERENCES asset(id) ON DELETE SET NULL,
    stand_id             UUID REFERENCES asset(id) ON DELETE SET NULL,
    cable_id             UUID REFERENCES asset(id) ON DELETE SET NULL,
    preamp_id            UUID REFERENCES asset(id) ON DELETE SET NULL,
    insert_outboard_id   UUID REFERENCES asset(id) ON DELETE SET NULL,
    converter_channel    TEXT,
    phantom              BOOLEAN,
    polarity             BOOLEAN,
    hpf                  BOOLEAN,
    pad                  BOOLEAN,
    notes                TEXT,
    CONSTRAINT unique_row_per_channel UNIQUE (version_id, channel_number)
);

CREATE INDEX idx_input_row_version ON input_row(version_id);

-- ============================================================================
-- LABEL OPERATIONS
-- ============================================================================

CREATE TABLE IF NOT EXISTS label_track (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    title TEXT NOT NULL,
    note TEXT,
    status TEXT NOT NULL DEFAULT 'open',
    owner_party_id BIGINT REFERENCES party(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_label_track_owner ON label_track(owner_party_id);
CREATE INDEX IF NOT EXISTS idx_label_track_status ON label_track(status);

-- ============================================================================
-- PIPELINE/KANBAN
-- ============================================================================

CREATE TABLE IF NOT EXISTS pipeline_card (
    id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    service_kind TEXT NOT NULL,
    title        TEXT NOT NULL,
    artist       TEXT,
    stage        TEXT NOT NULL,
    sort_order   INT NOT NULL DEFAULT 0,
    notes        TEXT,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_pipeline_card_kind ON pipeline_card(service_kind);
CREATE INDEX idx_pipeline_card_stage ON pipeline_card(service_kind, stage, sort_order);

-- ============================================================================
-- END OF SCHEMA
-- ============================================================================
