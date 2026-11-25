BEGIN;
CREATE TABLE directory_feed_post (
  profile_id uuid NOT NULL REFERENCES directory_profile(id),
  id uuid NOT NULL,
  caption text NOT NULL CHECK (length(trim(caption)) BETWEEN 1 AND 2000),
  published boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id,id)
);
CREATE TABLE directory_feed_image (
  profile_id uuid NOT NULL,
  post_id uuid NOT NULL,
  position smallint NOT NULL CHECK (position BETWEEN 1 AND 12),
  file_id uuid NOT NULL REFERENCES content_storage_upload(id),
  PRIMARY KEY (profile_id,post_id,position),
  UNIQUE (file_id),
  FOREIGN KEY (profile_id,post_id) REFERENCES directory_feed_post(profile_id,id)
);
CREATE VIEW directory_public_feed AS
SELECT p.profile_id,p.id,p.caption,p.created_at,array_agg(i.file_id ORDER BY i.position) AS images
FROM directory_feed_post p JOIN directory_public_profile visible ON visible.id=p.profile_id
JOIN directory_feed_image i ON (i.profile_id,i.post_id)=(p.profile_id,p.id)
WHERE p.published GROUP BY p.profile_id,p.id;
CREATE FUNCTION directory_publish_feed(profile uuid, post uuid, body text, files uuid[], actor bigint)
RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE owned_count integer;
BEGIN
  PERFORM 1 FROM directory_profile_manager m JOIN directory_profile p ON p.id=m.profile_id
    WHERE p.id=profile AND p.profile_kind='studio' AND m.account_party_id=actor
      AND m.active AND m.can_edit AND m.can_publish FOR SHARE OF m,p;
  IF NOT FOUND THEN RETURN false; END IF;
  SELECT count(*) INTO owned_count FROM (SELECT id FROM content_storage_upload
    WHERE id=ANY(files) AND owner_party_id=actor AND destination='tdf' AND status='completed'
      AND visibility='private' AND media_type IN ('image/png','image/jpeg','image/gif','image/webp') AND byte_size BETWEEN 1 AND 8388608 FOR SHARE) owned;
  IF owned_count <> cardinality(files) OR owned_count NOT BETWEEN 1 AND 12 THEN RETURN false; END IF;
  INSERT INTO directory_feed_post(profile_id,id,caption) VALUES(profile,post,body)
    ON CONFLICT DO NOTHING;
  IF NOT FOUND THEN
    RETURN EXISTS(SELECT 1 FROM directory_feed_post p WHERE p.profile_id=profile AND p.id=post
      AND p.caption=body AND p.published AND files=(SELECT array_agg(file_id ORDER BY position)
        FROM directory_feed_image WHERE profile_id=profile AND post_id=post));
  END IF;
  INSERT INTO directory_feed_image(profile_id,post_id,position,file_id)
    SELECT profile,post,n,file FROM unnest(files) WITH ORDINALITY AS input(file,n);
  RETURN true;
EXCEPTION WHEN unique_violation THEN RETURN false;
END;
$$;
CREATE FUNCTION directory_edit_feed(profile uuid, post uuid, body text, visible boolean, actor bigint)
RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
  PERFORM 1 FROM directory_profile_manager WHERE profile_id=profile AND account_party_id=actor
    AND active AND can_edit AND can_publish FOR SHARE;
  IF NOT FOUND THEN RETURN false; END IF;
  UPDATE directory_feed_post SET caption=body,published=visible WHERE profile_id=profile AND id=post;
  RETURN FOUND;
END;
$$;
COMMIT;
