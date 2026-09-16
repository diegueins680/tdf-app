-- Disposable PostgreSQL fixture for exercising the real Haskell activation transaction.

CREATE TABLE party (
  id bigserial PRIMARY KEY,
  legal_name text,
  display_name text NOT NULL,
  is_org boolean NOT NULL,
  tax_id text,
  primary_email text,
  primary_phone text,
  whatsapp text,
  instagram text,
  emergency_contact text,
  notes text,
  stripe_customer_id text,
  country_code text,
  country_id uuid,
  created_at timestamptz NOT NULL
);

CREATE TABLE user_credential (
  id bigserial PRIMARY KEY,
  party_id bigint NOT NULL,
  username text NOT NULL,
  password_hash text NOT NULL,
  active boolean NOT NULL,
  UNIQUE (username)
);

CREATE TABLE artist_profile (
  id bigserial PRIMARY KEY,
  artist_party_id bigint NOT NULL,
  slug text,
  bio text,
  city text,
  country_code text,
  country_id uuid,
  hero_image_url text,
  spotify_artist_id text,
  spotify_url text,
  youtube_channel_id text,
  youtube_url text,
  website_url text,
  featured_video_url text,
  genres text,
  highlights text,
  stripe_account_id text,
  created_at timestamptz NOT NULL,
  updated_at timestamptz,
  UNIQUE (artist_party_id)
);

CREATE TABLE artist_profile_enrichment (
  id bigserial PRIMARY KEY,
  artist_party_id bigint NOT NULL,
  official_name text,
  country text,
  instagram_url text,
  social_links text,
  discography text,
  achievements text,
  hero_original_url text,
  hero_square_url text,
  hero_landscape_url text,
  hero_responsive_urls text,
  hero_focal_point text,
  last_verified_at timestamptz,
  confidence double precision,
  review_status text NOT NULL DEFAULT 'unverified',
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  UNIQUE (artist_party_id)
);

CREATE TABLE artist_profile_genre_membership (
  artist_party_id bigint NOT NULL,
  genre_id uuid NOT NULL,
  sort_order bigint NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (artist_party_id, genre_id)
);

CREATE TABLE fan_follow (
  id bigserial PRIMARY KEY,
  fan_party_id bigint NOT NULL,
  artist_party_id bigint NOT NULL,
  created_at timestamptz NOT NULL,
  UNIQUE (fan_party_id, artist_party_id)
);

CREATE TABLE security_role (
  id uuid NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
  code text NOT NULL,
  name_es text NOT NULL,
  name_en text NOT NULL,
  description_es text,
  description_en text,
  sort_order bigint NOT NULL DEFAULT 0,
  system_role boolean NOT NULL DEFAULT False,
  emergency_administrator boolean NOT NULL DEFAULT False,
  self_assignable boolean NOT NULL DEFAULT False,
  automatic_assignable boolean NOT NULL DEFAULT False,
  active boolean NOT NULL DEFAULT True,
  workflow_state_id uuid NOT NULL,
  created_by bigint,
  updated_by bigint,
  approved_by bigint,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  published_revision bigint NOT NULL DEFAULT 1,
  version bigint NOT NULL DEFAULT 1,
  UNIQUE (code)
);

CREATE TABLE security_role_assignment_policy (
  id uuid NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
  code text NOT NULL,
  trigger_code text NOT NULL,
  role_id uuid NOT NULL,
  name_es text NOT NULL,
  name_en text NOT NULL,
  description_es text,
  description_en text,
  requires_verified_email boolean NOT NULL DEFAULT False,
  active boolean NOT NULL DEFAULT True,
  effective_from timestamptz,
  effective_to timestamptz,
  created_by bigint,
  updated_by bigint,
  approved_by bigint,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  version bigint NOT NULL DEFAULT 1,
  UNIQUE (code),
  UNIQUE (trigger_code, role_id)
);

CREATE TABLE party_security_role (
  id uuid NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
  party_id bigint NOT NULL,
  role_id uuid NOT NULL,
  granted_by bigint,
  approved_by bigint,
  approval_mode text NOT NULL DEFAULT 'bootstrap',
  emergency_reason text,
  source_revision_id uuid,
  source_policy_id uuid,
  active boolean NOT NULL DEFAULT True,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  revoked_at timestamptz,
  version bigint NOT NULL DEFAULT 1,
  UNIQUE (party_id, role_id)
);

CREATE TABLE security_audit_event (
  id uuid NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
  revision_id uuid,
  source_policy_id uuid,
  entity_kind text NOT NULL,
  party_id bigint,
  role_id uuid NOT NULL,
  permission_id uuid,
  operation text NOT NULL,
  previous_active boolean,
  new_active boolean,
  actor_id bigint,
  reviewer_id bigint,
  approver_id bigint,
  occurred_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  source_platform text NOT NULL,
  reason text,
  correlation_id text NOT NULL,
  approval_mode text NOT NULL,
  result text NOT NULL
);

CREATE OR REPLACE FUNCTION security_validate_assignment_policy() RETURNS trigger LANGUAGE plpgsql AS $$ DECLARE role_allowed boolean; BEGIN IF NEW.trigger_code NOT IN ('account-signup','google-account-create','verified-artist-claim','generated-account-create','course-registration','trial-inquiry','teacher-subject-configured','teacher-student-linked','student-created','artist-profile-created') THEN RAISE EXCEPTION 'unknown automatic security policy trigger' USING ERRCODE='23514'; END IF; IF NEW.effective_from IS NOT NULL AND NEW.effective_to IS NOT NULL AND NEW.effective_to<=NEW.effective_from THEN RAISE EXCEPTION 'automatic security policy effective period is invalid' USING ERRCODE='23514'; END IF; SELECT active AND automatic_assignable AND NOT emergency_administrator INTO role_allowed FROM security_role WHERE id=NEW.role_id; IF NOT COALESCE(role_allowed,FALSE) THEN RAISE EXCEPTION 'automatic security policies require an active, explicitly automatic, non-emergency role' USING ERRCODE='42501'; END IF; IF NEW.created_by IS NOT NULL AND (NEW.approved_by IS NULL OR NEW.approved_by=NEW.created_by) THEN RAISE EXCEPTION 'automatic security policy changes require a distinct approver' USING ERRCODE='42501'; END IF; RETURN NEW; END $$;

CREATE OR REPLACE FUNCTION security_validate_party_role_approval() RETURNS trigger LANGUAGE plpgsql AS $$ DECLARE is_sensitive boolean; actor_is_emergency boolean; valid_revision boolean; valid_policy boolean; BEGIN IF NEW.approval_mode NOT IN ('normal','emergency','bootstrap','system-policy') THEN RAISE EXCEPTION 'invalid security grant approval mode' USING ERRCODE='23514'; END IF; IF NEW.approval_mode='system-policy' THEN IF NEW.source_policy_id IS NULL OR NEW.source_revision_id IS NOT NULL OR NEW.granted_by IS NOT NULL OR NEW.approved_by IS NOT NULL OR NEW.emergency_reason IS NOT NULL THEN RAISE EXCEPTION 'system-policy grants require policy-only provenance' USING ERRCODE='23514'; END IF; SELECT EXISTS (SELECT 1 FROM security_role_assignment_policy policy JOIN security_role role ON role.id=policy.role_id WHERE policy.id=NEW.source_policy_id AND policy.role_id=NEW.role_id AND policy.active AND role.active AND role.automatic_assignable AND NOT role.emergency_administrator AND (policy.effective_from IS NULL OR policy.effective_from<=CURRENT_TIMESTAMP) AND (policy.effective_to IS NULL OR policy.effective_to>CURRENT_TIMESTAMP)) INTO valid_policy; IF NOT COALESCE(valid_policy,FALSE) OR NOT NEW.active THEN RAISE EXCEPTION 'system-policy grant does not match an effective automatic policy' USING ERRCODE='23514'; END IF; RETURN NEW; END IF; IF NEW.source_policy_id IS NOT NULL THEN RAISE EXCEPTION 'only system-policy grants may reference an assignment policy' USING ERRCODE='23514'; END IF; IF NEW.granted_by IS NOT NULL AND NEW.source_revision_id IS NULL THEN RAISE EXCEPTION 'administrative security grants require an approved source revision' USING ERRCODE='23514'; END IF; IF NEW.granted_by IS NOT NULL THEN SELECT EXISTS (SELECT 1 FROM security_grant_revision revision JOIN workflow_state state ON state.id=revision.workflow_state_id WHERE revision.id=NEW.source_revision_id AND state.code='published' AND revision.approved_at IS NOT NULL AND revision.created_by=NEW.granted_by AND revision.approved_by=NEW.approved_by AND revision.approval_mode=NEW.approval_mode AND COALESCE(revision.emergency_reason,'')=COALESCE(NEW.emergency_reason,'') AND revision.role_id=NEW.role_id AND revision.desired_active=NEW.active AND revision.change_kind='party-role' AND revision.party_id=NEW.party_id AND revision.permission_id IS NULL) INTO valid_revision; IF NOT COALESCE(valid_revision,FALSE) THEN RAISE EXCEPTION 'security grant does not match an approved source revision' USING ERRCODE='23514'; END IF; END IF; SELECT (r.emergency_administrator OR r.system_role) INTO is_sensitive FROM security_role r WHERE r.id=NEW.role_id; IF COALESCE(is_sensitive,FALSE) AND NEW.granted_by IS NOT NULL THEN IF NEW.approved_by IS NULL THEN RAISE EXCEPTION 'sensitive security grants require approval' USING ERRCODE='42501'; ELSIF NEW.approved_by=NEW.granted_by THEN IF NEW.approval_mode<>'emergency' OR length(btrim(COALESCE(NEW.emergency_reason,'')))<20 THEN RAISE EXCEPTION 'sensitive security grants require a distinct approver or documented emergency override' USING ERRCODE='42501'; END IF; SELECT security_is_coherent_emergency_administrator(NEW.approved_by) INTO actor_is_emergency; IF NOT COALESCE(actor_is_emergency,FALSE) THEN RAISE EXCEPTION 'emergency security approver is not active' USING ERRCODE='42501'; END IF; ELSIF NEW.approval_mode<>'normal' OR NEW.emergency_reason IS NOT NULL THEN RAISE EXCEPTION 'non-emergency security approval must use normal mode' USING ERRCODE='23514'; END IF; ELSIF NEW.approval_mode='emergency' THEN RAISE EXCEPTION 'emergency mode is valid only for sensitive grants' USING ERRCODE='23514'; END IF; RETURN NEW; END $$;

CREATE TRIGGER policy_guard BEFORE INSERT OR UPDATE ON security_role_assignment_policy FOR EACH ROW EXECUTE FUNCTION security_validate_assignment_policy();

CREATE TRIGGER grant_guard BEFORE INSERT OR UPDATE ON party_security_role FOR EACH ROW EXECUTE FUNCTION security_validate_party_role_approval();

INSERT INTO security_role(id,code,name_es,name_en,workflow_state_id,automatic_assignable) VALUES ('331c2422-89e0-4cfa-ad65-8dc57f27d5e5','artist','Artista','Artist','00000000-0000-4000-8000-000000000215',true);

INSERT INTO security_role_assignment_policy(code,trigger_code,role_id,name_es,name_en) VALUES ('live-session.artist-profile.artist','artist-profile-created','331c2422-89e0-4cfa-ad65-8dc57f27d5e5','Artista','Artist');

INSERT INTO party(id,display_name,is_org,created_at) SELECT n,'Artist '||n,false,now() FROM generate_series(1,8) n;

INSERT INTO user_credential(party_id,username,password_hash,active) SELECT n,'artist-'||n,'not-a-login-hash',n<>5 FROM generate_series(1,7) n;
BEGIN;

CREATE TABLE IF NOT EXISTS feature_access_requests (
  id BIGSERIAL PRIMARY KEY,
  requester_party_id BIGINT NOT NULL REFERENCES party(id),
  feature_id TEXT NOT NULL,
  action TEXT NOT NULL,
  role_context TEXT NOT NULL,
  module_context TEXT NOT NULL,
  justification TEXT,
  status TEXT NOT NULL DEFAULT 'pending',
  reviewer_group TEXT NOT NULL,
  reviewer_party_id BIGINT REFERENCES party(id),
  reviewer_notes TEXT,
  requested_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  decided_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  CONSTRAINT feature_access_requests_status_check
    CHECK (status IN ('pending', 'approved', 'rejected', 'cancelled', 'expired')),
  CONSTRAINT feature_access_requests_action_check
    CHECK (action IN (
      'discover', 'view', 'create', 'edit', 'delete', 'archive', 'deactivate',
      'import', 'export', 'submit', 'validate', 'approve', 'reject', 'assign',
      'publish', 'report', 'administer'
    )),
  CONSTRAINT feature_access_requests_justification_length_check
    CHECK (justification IS NULL OR char_length(justification) <= 2000),
  CONSTRAINT feature_access_requests_reviewer_notes_length_check
    CHECK (reviewer_notes IS NULL OR char_length(reviewer_notes) <= 2000)
);

CREATE INDEX IF NOT EXISTS feature_access_requests_requester_idx
  ON feature_access_requests (requester_party_id, requested_at DESC);
CREATE INDEX IF NOT EXISTS feature_access_requests_queue_idx
  ON feature_access_requests (status, reviewer_group, requested_at);
CREATE INDEX IF NOT EXISTS feature_access_requests_duplicate_idx
  ON feature_access_requests (requester_party_id, feature_id, action, status);
CREATE UNIQUE INDEX IF NOT EXISTS feature_access_requests_one_pending_idx
  ON feature_access_requests (requester_party_id, feature_id, action)
  WHERE status = 'pending';

CREATE TABLE IF NOT EXISTS feature_access_request_history (
  id BIGSERIAL PRIMARY KEY,
  request_id BIGINT NOT NULL REFERENCES feature_access_requests(id),
  actor_party_id BIGINT REFERENCES party(id),
  transition TEXT NOT NULL,
  from_status TEXT,
  to_status TEXT NOT NULL,
  note TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT feature_access_request_history_note_length_check
    CHECK (note IS NULL OR char_length(note) <= 2000)
);

CREATE INDEX IF NOT EXISTS feature_access_request_history_request_idx
  ON feature_access_request_history (request_id, created_at);

COMMIT;

-- Rollback (run only after exporting both tables):
-- BEGIN;
-- DROP TABLE IF EXISTS feature_access_request_history;
-- DROP TABLE IF EXISTS feature_access_requests;
-- COMMIT;
