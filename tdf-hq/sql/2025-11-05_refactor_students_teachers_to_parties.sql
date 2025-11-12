-- Migration to refactor students and teachers tables to use party table as single source of truth
-- This eliminates data duplication by using party_id foreign keys instead of repeating contact information
--
-- Phase 1: Add party_id columns and migrate existing data (this script)
-- Phase 2: Update application code to use party data via joins/views
-- Phase 3: Remove duplicate columns (name, email, phone) after transition period
--
-- Safe to run multiple times (idempotent)

-- =============================================================================
-- PHASE 1: Add party_id columns to students and teachers tables
-- =============================================================================

-- Add party_id column to students table if not exists
DO $$ 
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns 
        WHERE table_name = 'students' AND column_name = 'party_id'
    ) THEN
        ALTER TABLE students ADD COLUMN party_id INTEGER REFERENCES party(id);
        CREATE INDEX IF NOT EXISTS idx_students_party_id ON students(party_id);
    END IF;
END $$;

-- Add party_id column to teachers table if not exists
DO $$ 
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns 
        WHERE table_name = 'teachers' AND column_name = 'party_id'
    ) THEN
        ALTER TABLE teachers ADD COLUMN party_id INTEGER REFERENCES party(id);
        CREATE INDEX IF NOT EXISTS idx_teachers_party_id ON teachers(party_id);
    END IF;
END $$;

-- =============================================================================
-- Migrate existing students to party table
-- =============================================================================

CREATE OR REPLACE FUNCTION migrate_students_to_party() RETURNS void AS $$
DECLARE
    student_rec RECORD;
    new_party_id INTEGER;
BEGIN
    FOR student_rec IN 
        SELECT * FROM students WHERE party_id IS NULL
    LOOP
        -- Check if a party already exists with matching email or phone
        SELECT id INTO new_party_id 
        FROM party 
        WHERE primary_email = student_rec.email 
           OR primary_phone = student_rec.phone
        LIMIT 1;
        
        -- If no matching party found, create a new one
        IF new_party_id IS NULL THEN
            INSERT INTO party (
                display_name,
                legal_name,
                primary_email,
                primary_phone,
                whatsapp,
                instagram,
                emergency_contact,
                notes,
                is_org,
                created_at
            ) VALUES (
                student_rec.name,
                student_rec.name,
                student_rec.email,
                student_rec.phone,
                student_rec.whatsapp,
                student_rec.instagram,
                student_rec.emergency_contact,
                student_rec.notes,
                FALSE,
                COALESCE(student_rec.created_at, NOW())
            ) RETURNING id INTO new_party_id;
        END IF;
        
        -- Update student record with party_id
        UPDATE students SET party_id = new_party_id WHERE id = student_rec.id;
        
        -- Add Student role to party_role table
        INSERT INTO party_role (party_id, role, active)
        VALUES (new_party_id, 'Student', TRUE)
        ON CONFLICT (party_id, role) DO UPDATE SET active = TRUE;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- =============================================================================
-- Migrate existing teachers to party table
-- =============================================================================

CREATE OR REPLACE FUNCTION migrate_teachers_to_party() RETURNS void AS $$
DECLARE
    teacher_rec RECORD;
    new_party_id INTEGER;
BEGIN
    FOR teacher_rec IN 
        SELECT * FROM teachers WHERE party_id IS NULL
    LOOP
        -- Check if a party already exists with matching email or phone
        SELECT id INTO new_party_id 
        FROM party 
        WHERE primary_email = teacher_rec.email 
           OR primary_phone = teacher_rec.phone
        LIMIT 1;
        
        -- If no matching party found, create a new one
        IF new_party_id IS NULL THEN
            INSERT INTO party (
                display_name,
                legal_name,
                primary_email,
                primary_phone,
                whatsapp,
                instagram,
                emergency_contact,
                notes,
                is_org,
                created_at
            ) VALUES (
                teacher_rec.name,
                teacher_rec.name,
                teacher_rec.email,
                teacher_rec.phone,
                teacher_rec.whatsapp,
                teacher_rec.instagram,
                teacher_rec.emergency_contact,
                teacher_rec.notes,
                FALSE,
                COALESCE(teacher_rec.created_at, NOW())
            ) RETURNING id INTO new_party_id;
        END IF;
        
        -- Update teacher record with party_id
        UPDATE teachers SET party_id = new_party_id WHERE id = teacher_rec.id;
        
        -- Add Teacher role to party_role table
        INSERT INTO party_role (party_id, role, active)
        VALUES (new_party_id, 'Teacher', TRUE)
        ON CONFLICT (party_id, role) DO UPDATE SET active = TRUE;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Run the migration functions
SELECT migrate_students_to_party();
SELECT migrate_teachers_to_party();

-- =============================================================================
-- PHASE 2: Create views for backward compatibility
-- =============================================================================

-- View that combines student data with party data
CREATE OR REPLACE VIEW v_students_with_party AS
SELECT 
    s.id,
    s.party_id,
    p.display_name as name,
    p.primary_email as email,
    p.primary_phone as phone,
    p.whatsapp,
    p.instagram,
    p.emergency_contact,
    s.level,
    s.status,
    s.enrollment_date,
    s.birth_date,
    COALESCE(s.notes, p.notes) as notes,
    s.created_at,
    s.updated_at
FROM students s
JOIN party p ON s.party_id = p.id;

-- View that combines teacher data with party data
CREATE OR REPLACE VIEW v_teachers_with_party AS
SELECT 
    t.id,
    t.party_id,
    p.display_name as name,
    p.primary_email as email,
    p.primary_phone as phone,
    p.whatsapp,
    p.instagram,
    p.emergency_contact,
    t.hire_date,
    t.status,
    t.hourly_rate_cents,
    COALESCE(t.notes, p.notes) as notes,
    t.created_at,
    t.updated_at
FROM teachers t
JOIN party p ON t.party_id = p.id;

-- =============================================================================
-- PHASE 3: Cleanup (commented out - run after transition period)
-- =============================================================================

-- After confirming all application code uses party data via joins/views:
-- 
-- ALTER TABLE students 
--     DROP COLUMN name,
--     DROP COLUMN email,
--     DROP COLUMN phone,
--     DROP COLUMN whatsapp,
--     DROP COLUMN instagram,
--     DROP COLUMN emergency_contact;
--
-- ALTER TABLE teachers 
--     DROP COLUMN name,
--     DROP COLUMN email,
--     DROP COLUMN phone,
--     DROP COLUMN whatsapp,
--     DROP COLUMN instagram,
--     DROP COLUMN emergency_contact;
--
-- ALTER TABLE students ALTER COLUMN party_id SET NOT NULL;
-- ALTER TABLE teachers ALTER COLUMN party_id SET NOT NULL;
