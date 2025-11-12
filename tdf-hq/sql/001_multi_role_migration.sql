-- Migration: Multi-role user management
-- Description: Refactor from single role to many-to-many roles
-- Date: 2025-11-05

BEGIN;

-- Step 1: Create the new PartyRoleAssignment join table only if it isn't present
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name   = 'party_role_assignment'
    ) THEN
        IF EXISTS (
            SELECT 1
            FROM information_schema.tables
            WHERE table_schema = 'public'
              AND table_name   = 'party_role'
        ) THEN
            -- Legacy table name from prior migrations; rename it instead of recreating.
            ALTER TABLE party_role RENAME TO party_role_assignment;
        ELSE
            CREATE TABLE party_role_assignment (
                id SERIAL PRIMARY KEY,
                party_id INTEGER NOT NULL REFERENCES party(id) ON DELETE CASCADE,
                role VARCHAR(50) NOT NULL,
                assigned_at TIMESTAMP NOT NULL DEFAULT NOW(),
                assigned_by INTEGER REFERENCES party(id) ON DELETE SET NULL,
                CONSTRAINT unique_party_role UNIQUE (party_id, role)
            );
        END IF;
    END IF;
END $$;

-- Step 2: Create indexes when missing
CREATE INDEX IF NOT EXISTS idx_party_role_assignment_party_id
    ON party_role_assignment(party_id);

CREATE INDEX IF NOT EXISTS idx_party_role_assignment_role
    ON party_role_assignment(role);

-- Step 4: Migrate existing role data (if the role column exists)
-- This assumes there's an existing 'role' column on the 'party' table
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM information_schema.columns 
        WHERE table_name = 'party' AND column_name = 'role'
    ) THEN
        -- Copy existing single roles to the new join table
        INSERT INTO party_role_assignment (party_id, role, assigned_at)
        SELECT id, role, created_at
        FROM party
        WHERE role IS NOT NULL;
        
        -- Drop the old role column
        ALTER TABLE party DROP COLUMN role;
    END IF;
END $$;

COMMIT;

-- Rollback script (in case of issues):
-- BEGIN;
-- ALTER TABLE party ADD COLUMN role VARCHAR(50);
-- UPDATE party p SET role = (
--     SELECT pra.role FROM party_role_assignment pra 
--     WHERE pra.party_id = p.id 
--     LIMIT 1
-- );
-- DROP TABLE party_role_assignment;
-- COMMIT;
