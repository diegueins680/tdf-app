-- Migration: Party and Booking enhancements
-- Date: 2025-11-06

BEGIN;

-- Rename existing party columns to the new schema when needed
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'party' AND column_name = 'name'
    ) THEN
        ALTER TABLE party RENAME COLUMN name TO display_name;
    END IF;

    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'party' AND column_name = 'email'
    ) THEN
        ALTER TABLE party RENAME COLUMN email TO primary_email;
    END IF;

    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'party' AND column_name = 'phone'
    ) THEN
        ALTER TABLE party RENAME COLUMN phone TO primary_phone;
    END IF;
END $$;

-- Add new party attributes if they are missing
ALTER TABLE party ADD COLUMN IF NOT EXISTS legal_name TEXT;
ALTER TABLE party ADD COLUMN IF NOT EXISTS is_org BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE party ADD COLUMN IF NOT EXISTS notes TEXT;
ALTER TABLE party ADD COLUMN IF NOT EXISTS status TEXT NOT NULL DEFAULT 'Active';
ALTER TABLE party ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP NOT NULL DEFAULT NOW();

-- Ensure instagram / whatsapp / tax_id / emergency_contact columns exist for older snapshots
ALTER TABLE party ADD COLUMN IF NOT EXISTS instagram TEXT;
ALTER TABLE party ADD COLUMN IF NOT EXISTS whatsapp TEXT;
ALTER TABLE party ADD COLUMN IF NOT EXISTS tax_id TEXT;
ALTER TABLE party ADD COLUMN IF NOT EXISTS emergency_contact TEXT;

-- Rename unique constraint to match new column name when necessary
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_name = 'party' AND constraint_name = 'unique_email'
    ) THEN
        ALTER TABLE party RENAME CONSTRAINT unique_email TO unique_primary_email;
    END IF;
END $$;

-- Booking table adjustments
ALTER TABLE booking ADD COLUMN IF NOT EXISTS title TEXT;
UPDATE booking SET title = COALESCE(title, 'Booking');
ALTER TABLE booking ALTER COLUMN title SET NOT NULL;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'booking' AND column_name = 'party_id'
    ) THEN
        EXECUTE 'ALTER TABLE booking ALTER COLUMN party_id DROP NOT NULL';
    END IF;

    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'booking' AND column_name = 'resource_id'
    ) THEN
        EXECUTE 'ALTER TABLE booking ALTER COLUMN resource_id DROP NOT NULL';
    END IF;
END $$;

ALTER TABLE booking ADD COLUMN IF NOT EXISTS service_type TEXT;
ALTER TABLE booking ADD COLUMN IF NOT EXISTS notes TEXT;

COMMIT;
