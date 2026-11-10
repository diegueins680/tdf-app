-- Run only in a disposable DB after the migration, with Party id 910001 present.
BEGIN;
INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at)
VALUES ('test-watch',repeat('a',64),910001,clock_timestamp()+interval '1 day');
INSERT INTO event_watch_session(token_hash,client_id,issued_at,expires_at)
VALUES (repeat('b',64),'test-watch',now(),now()+interval '15 minutes');

DO $$ BEGIN
  BEGIN
    INSERT INTO event_watch_session(token_hash,client_id,issued_at,expires_at)
    VALUES (repeat('c',64),'test-watch',now(),now()+interval '16 minutes');
    RAISE EXCEPTION 'overlong session accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  BEGIN
    INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at)
    VALUES ('bad-watch','plaintext',910001,now()+interval '1 day');
    RAISE EXCEPTION 'unhashed credential accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
END $$;

UPDATE event_watch_client SET secret_hash=repeat('d',64) WHERE client_id='test-watch';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_watch_session WHERE client_id='test-watch') THEN
    RAISE EXCEPTION 'rotation did not revoke sessions';
  END IF;
END $$;
INSERT INTO event_watch_session(token_hash,client_id,issued_at,expires_at)
VALUES (repeat('e',64),'test-watch',now(),now()+interval '1 minute');
UPDATE event_watch_client SET allow_publish=true WHERE client_id='test-watch';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_watch_session WHERE client_id='test-watch') THEN
    RAISE EXCEPTION 'publication scope change did not revoke sessions';
  END IF;
END $$;
INSERT INTO event_watch_session(token_hash,client_id,issued_at,expires_at)
VALUES (repeat('f',64),'test-watch',now(),now()+interval '1 minute');
UPDATE event_watch_client SET active=false WHERE client_id='test-watch';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_watch_session WHERE client_id='test-watch') THEN
    RAISE EXCEPTION 'deactivation did not revoke sessions';
  END IF;
END $$;
ROLLBACK;
