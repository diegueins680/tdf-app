-- Real canonical identity functions and Party schema in an owned disposable DB.
-- Synthetic historical archive evidence is inserted explicitly; this does not
-- claim execution of a real operator merge or authorize any production identity.
BEGIN;
DO $$
DECLARE actor bigint; canonical bigint; historical bigint; case_id uuid:=gen_random_uuid();
        operation_id uuid:=gen_random_uuid(); dependencies jsonb;
BEGIN
  INSERT INTO party(display_name,is_org,created_at) VALUES('Watch archived actor fixture',false,now()) RETURNING id INTO actor;
  INSERT INTO party(display_name,is_org,created_at) VALUES('Watch canonical actor fixture',false,now()) RETURNING id INTO canonical;
  INSERT INTO party(display_name,is_org,created_at) VALUES('Watch historical actor fixture',false,now()) RETURNING id INTO historical;
  INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at)
    VALUES('historical-watch',repeat('a',64),actor,clock_timestamp()+interval '1 day');
  dependencies:=identity_party_dependencies(actor);
  IF NOT EXISTS(SELECT 1 FROM jsonb_array_elements(dependencies) item
    WHERE item->>'table'='event_watch_client' AND item->>'column'='actor_party_id' AND (item->>'count')::bigint=1) THEN
    RAISE EXCEPTION 'dynamic identity dependencies must include the actual service credential FK';
  END IF;
  INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)
    VALUES(case_id,ARRAY[actor,canonical],'{}','[]','Synthetic historical archive fixture');
  INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)
    VALUES(operation_id,case_id,canonical,ARRAY[actor],'[]','{}');
  INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id) VALUES(actor,canonical,operation_id);
  BEGIN
    INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at)
      VALUES('archived-watch',repeat('a',64),actor,clock_timestamp()+interval '1 day');
    RAISE EXCEPTION 'archived actor acquired a new credential';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL;
  END;
  INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at)
    VALUES('reassign-watch',repeat('a',64),historical,clock_timestamp()+interval '1 day');
  BEGIN
    UPDATE event_watch_client SET actor_party_id=actor WHERE client_id='reassign-watch';
    RAISE EXCEPTION 'credential was reassigned to an archived actor';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL;
  END;
  UPDATE event_watch_client SET active=false WHERE client_id='historical-watch';
  IF (SELECT actor_party_id FROM event_watch_client WHERE client_id='historical-watch') IS DISTINCT FROM actor THEN
    RAISE EXCEPTION 'unrelated historical update changed actor reference';
  END IF;
END $$;
ROLLBACK;
