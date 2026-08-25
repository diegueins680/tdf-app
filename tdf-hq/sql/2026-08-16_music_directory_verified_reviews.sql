\set ON_ERROR_STOP on

-- Incremental, retry-safe reputation support for the immutable music-directory
-- core migration. Only verified, completed interactions can contribute public
-- reviews or derived reputation counters.
BEGIN;

CREATE INDEX IF NOT EXISTS directory_review_subject_public_idx
  ON directory_review(subject_profile_id,created_at DESC,id DESC)
  WHERE status='published';
CREATE INDEX IF NOT EXISTS directory_review_author_subject_idx
  ON directory_review(author_profile_id,subject_profile_id);
CREATE INDEX IF NOT EXISTS directory_interaction_profile_a_status_idx
  ON directory_interaction(profile_a_id,status,verified_at DESC,id);
CREATE INDEX IF NOT EXISTS directory_interaction_profile_b_status_idx
  ON directory_interaction(profile_b_id,status,verified_at DESC,id);

ALTER TABLE directory_moderation_report
  DROP CONSTRAINT IF EXISTS directory_moderation_report_target_kind_check;
ALTER TABLE directory_moderation_report
  ADD CONSTRAINT directory_moderation_report_target_kind_check
  CHECK (target_kind IN ('profile','classified','application','invitation','event','venue','message','review'));

ALTER TABLE directory_rate_limit
  DROP CONSTRAINT IF EXISTS directory_rate_limit_scope_check;
ALTER TABLE directory_rate_limit
  ADD CONSTRAINT directory_rate_limit_scope_check
  CHECK (scope IN ('search','profile_create','classified_publish','application','invitation','contact','report','review'));

-- Clients never write these aggregates. Canonical resolution preserves
-- reputation after non-destructive profile merges, and a review only counts
-- while its author remains a public profile.
CREATE OR REPLACE FUNCTION directory_refresh_profile_reputation(profile_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  UPDATE directory_profile profile
  SET completed_interactions = (
        SELECT count(*)::integer
        FROM directory_interaction interaction
        JOIN directory_profile profile_a ON profile_a.id=interaction.profile_a_id
        JOIN directory_profile profile_b ON profile_b.id=interaction.profile_b_id
        WHERE interaction.status='completed'
          AND interaction.verified_at IS NOT NULL
          AND (
            coalesce(profile_a.canonical_profile_id,profile_a.id)=profile.id
            OR coalesce(profile_b.canonical_profile_id,profile_b.id)=profile.id
          )
      ),
      review_count = (
        SELECT count(*)::integer
        FROM directory_review review
        JOIN directory_interaction interaction ON interaction.id=review.interaction_id
        JOIN directory_profile raw_subject ON raw_subject.id=review.subject_profile_id
        JOIN directory_profile raw_author ON raw_author.id=review.author_profile_id
        JOIN directory_public_profile public_author
          ON public_author.id=coalesce(raw_author.canonical_profile_id,raw_author.id)
        WHERE coalesce(raw_subject.canonical_profile_id,raw_subject.id)=profile.id
          AND coalesce(raw_author.canonical_profile_id,raw_author.id)<>profile.id
          AND review.status='published'
          AND interaction.status='completed'
          AND interaction.verified_at IS NOT NULL
          AND (
            (interaction.profile_a_id=review.author_profile_id AND interaction.profile_b_id=review.subject_profile_id)
            OR
            (interaction.profile_b_id=review.author_profile_id AND interaction.profile_a_id=review.subject_profile_id)
          )
      ),
      review_average = (
        SELECT round(avg(review.rating)::numeric,2)
        FROM directory_review review
        JOIN directory_interaction interaction ON interaction.id=review.interaction_id
        JOIN directory_profile raw_subject ON raw_subject.id=review.subject_profile_id
        JOIN directory_profile raw_author ON raw_author.id=review.author_profile_id
        JOIN directory_public_profile public_author
          ON public_author.id=coalesce(raw_author.canonical_profile_id,raw_author.id)
        WHERE coalesce(raw_subject.canonical_profile_id,raw_subject.id)=profile.id
          AND coalesce(raw_author.canonical_profile_id,raw_author.id)<>profile.id
          AND review.status='published'
          AND interaction.status='completed'
          AND interaction.verified_at IS NOT NULL
          AND (
            (interaction.profile_a_id=review.author_profile_id AND interaction.profile_b_id=review.subject_profile_id)
            OR
            (interaction.profile_b_id=review.author_profile_id AND interaction.profile_a_id=review.subject_profile_id)
          )
      ),
      version=profile.version+1
  WHERE profile.id=profile_id_value;
  PERFORM directory_refresh_profile_search(profile_id_value);
END
$$;

CREATE OR REPLACE FUNCTION directory_refresh_review_reputation()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  old_subject UUID;
  new_subject UUID;
BEGIN
  IF TG_OP<>'INSERT' THEN
    SELECT coalesce(canonical_profile_id,id) INTO old_subject
    FROM directory_profile WHERE id=OLD.subject_profile_id;
    IF old_subject IS NOT NULL THEN
      PERFORM directory_refresh_profile_reputation(old_subject);
    END IF;
  END IF;
  IF TG_OP<>'DELETE' THEN
    SELECT coalesce(canonical_profile_id,id) INTO new_subject
    FROM directory_profile WHERE id=NEW.subject_profile_id;
    IF new_subject IS NOT NULL AND new_subject IS DISTINCT FROM old_subject THEN
      PERFORM directory_refresh_profile_reputation(new_subject);
    END IF;
  END IF;
  RETURN coalesce(NEW,OLD);
END
$$;

DROP TRIGGER IF EXISTS directory_review_reputation_trigger ON directory_review;
CREATE TRIGGER directory_review_reputation_trigger
AFTER INSERT OR UPDATE OR DELETE ON directory_review
FOR EACH ROW EXECUTE FUNCTION directory_refresh_review_reputation();

CREATE OR REPLACE FUNCTION directory_refresh_interaction_reputation()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  profile_id_value UUID;
BEGIN
  FOR profile_id_value IN
    SELECT DISTINCT coalesce(profile.canonical_profile_id,profile.id)
    FROM directory_profile profile
    WHERE profile.id IN (
      CASE WHEN TG_OP='INSERT' THEN NEW.profile_a_id ELSE OLD.profile_a_id END,
      CASE WHEN TG_OP='INSERT' THEN NEW.profile_b_id ELSE OLD.profile_b_id END,
      CASE WHEN TG_OP='DELETE' THEN OLD.profile_a_id ELSE NEW.profile_a_id END,
      CASE WHEN TG_OP='DELETE' THEN OLD.profile_b_id ELSE NEW.profile_b_id END
    )
  LOOP
    PERFORM directory_refresh_profile_reputation(profile_id_value);
  END LOOP;
  RETURN coalesce(NEW,OLD);
END
$$;

DROP TRIGGER IF EXISTS directory_interaction_reputation_trigger ON directory_interaction;
CREATE TRIGGER directory_interaction_reputation_trigger
AFTER INSERT OR UPDATE OR DELETE ON directory_interaction
FOR EACH ROW EXECUTE FUNCTION directory_refresh_interaction_reputation();

-- A profile becoming non-public must also remove reviews it authored from the
-- reputation of every subject. Canonical-author matching covers merged aliases.
CREATE OR REPLACE FUNCTION directory_refresh_profile_status_reputation()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  affected_subject UUID;
  canonical_profile UUID;
BEGIN
  IF OLD.profile_status IS NOT DISTINCT FROM NEW.profile_status THEN
    RETURN NEW;
  END IF;
  canonical_profile := coalesce(NEW.canonical_profile_id,NEW.id);
  FOR affected_subject IN
    SELECT DISTINCT coalesce(raw_subject.canonical_profile_id,raw_subject.id)
    FROM directory_review review
    JOIN directory_profile raw_subject ON raw_subject.id=review.subject_profile_id
    JOIN directory_profile raw_author ON raw_author.id=review.author_profile_id
    WHERE coalesce(raw_author.canonical_profile_id,raw_author.id)=canonical_profile
  LOOP
    PERFORM directory_refresh_profile_reputation(affected_subject);
  END LOOP;
  PERFORM directory_refresh_profile_reputation(canonical_profile);
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS directory_profile_status_reputation_trigger ON directory_profile;
CREATE TRIGGER directory_profile_status_reputation_trigger
AFTER UPDATE OF profile_status ON directory_profile
FOR EACH ROW EXECUTE FUNCTION directory_refresh_profile_status_reputation();

-- Auditable reconciliation: every current profile is recomputed from source
-- rows. This intentionally invents no interactions, reviews, or identities.
DO $$
DECLARE
  profile_id_value UUID;
BEGIN
  FOR profile_id_value IN SELECT id FROM directory_profile LOOP
    PERFORM directory_refresh_profile_reputation(profile_id_value);
  END LOOP;
END
$$;

COMMIT;
