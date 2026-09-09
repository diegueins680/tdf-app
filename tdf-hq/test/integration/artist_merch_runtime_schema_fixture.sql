\set ON_ERROR_STOP on

-- Minimal directory surface required before applying the merch migration.
-- The canonical directory migration is deliberately not duplicated here;
-- this isolated runtime fixture declares only the referenced contract.
CREATE TABLE directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_kind TEXT NOT NULL,
  public_name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  profile_status TEXT NOT NULL,
  visibility TEXT NOT NULL,
  moderation_status TEXT NOT NULL
);

CREATE TABLE directory_profile_manager (
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  active BOOLEAN NOT NULL,
  can_manage BOOLEAN NOT NULL,
  source_claim_id UUID,
  PRIMARY KEY(profile_id,account_party_id)
);

CREATE TABLE directory_verification (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  status TEXT NOT NULL
);
