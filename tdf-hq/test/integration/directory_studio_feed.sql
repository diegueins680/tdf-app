-- Isolated scratch database only. Run migration before this transaction.
BEGIN;
DO $$
DECLARE studio uuid := gen_random_uuid(); other_profile uuid := gen_random_uuid();
  post uuid := gen_random_uuid(); files uuid[]; id uuid;
BEGIN
  INSERT INTO directory_profile(id,profile_kind) VALUES(studio,'studio'),(other_profile,'studio');
  INSERT INTO directory_profile_manager VALUES(studio,11,true,true,true),(other_profile,22,true,true,true);
  FOR i IN 1..6 LOOP
    id := gen_random_uuid();
    INSERT INTO content_storage_upload VALUES(id,11,'tdf','completed','private',64);
    files := array_append(files,id);
  END LOOP;
  IF directory_publish_feed(other_profile,post,'fixture',files,11) OR
     directory_publish_feed(other_profile,post,'fixture',files,22) OR
     directory_publish_feed(studio,post,'fixture',array_append(files,gen_random_uuid()),11)
    THEN RAISE EXCEPTION 'cross-profile or unknown file accepted'; END IF;
  UPDATE content_storage_upload SET byte_size=8388609 WHERE content_storage_upload.id=files[1];
  IF directory_publish_feed(studio,post,'fixture',files,11) THEN RAISE EXCEPTION 'oversized'; END IF;
  UPDATE content_storage_upload SET byte_size=64;
  IF NOT directory_publish_feed(studio,post,'fixture',files,11) OR NOT directory_publish_feed(studio,post,'fixture',files,11)
    THEN RAISE EXCEPTION 'publish/retry failed'; END IF;
  IF directory_publish_feed(studio,post,'changed',files,11) THEN RAISE EXCEPTION 'conflicting retry'; END IF;
  IF EXISTS(SELECT FROM directory_public_feed) THEN RAISE EXCEPTION 'unlisted leak'; END IF;
  INSERT INTO directory_public_profile VALUES(studio);
  IF (SELECT images FROM directory_public_feed) IS DISTINCT FROM files THEN RAISE EXCEPTION 'ordering'; END IF;
  UPDATE directory_feed_post SET published=false;
  IF EXISTS(SELECT FROM directory_public_feed) THEN RAISE EXCEPTION 'withdrawn leak'; END IF;
END;
$$;
ROLLBACK;
