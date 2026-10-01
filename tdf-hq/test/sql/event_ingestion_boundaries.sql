INSERT INTO party VALUES(1);
INSERT INTO event_discovery_source(source_key,name,source_type,feed_url) VALUES('fixture-structured','Fixture','json','https://official.example/feed');
INSERT INTO event_research_run(run_key,status,started_at,updated_at,created_by_party_id)
 VALUES('boundary-test','running',now(),now(),'1');
INSERT INTO event_research_candidate(provider,external_id,run_id,review_state,title,timezone,country_code,source_url,payload,evidence,confidence,content_hash,verified_at,created_at,updated_at,is_pilot)
 SELECT 'fixture','research-'||n,1,'draft','Fixture','America/Guayaquil','EC','https://official.example/event','{}','[{}]','medium',repeat('a',64),now(),now(),now(),false FROM generate_series(1,19) n;
INSERT INTO social_event VALUES(1),(2),(3);
INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('fixture','discovery-1',1,'draft:on_sale');
DO $$ BEGIN
 IF (SELECT count(*) FROM tdf_event_pilot_keys())<>20 THEN RAISE EXCEPTION 'shared cap did not count both entry points'; END IF;
 IF EXISTS(SELECT 1 FROM event_research_candidate WHERE NOT is_pilot) THEN RAISE EXCEPTION 'pilot flag bypass'; END IF;
 BEGIN
  INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('fixture','discovery-over',2,'draft:on_sale');
  RAISE EXCEPTION 'TEST: discovery exceeded cap';
 EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
 BEGIN
  INSERT INTO event_research_candidate(provider,external_id,run_id,review_state,title,timezone,country_code,source_url,payload,evidence,confidence,content_hash,verified_at,created_at,updated_at)
   VALUES('fixture','research-over',1,'draft','Fixture','America/Guayaquil','EC','https://official.example/event','{}','[{}]','medium',repeat('a',64),now(),now(),now());
  RAISE EXCEPTION 'TEST: research exceeded shared cap';
 EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
END $$;
-- Attach another source to the same canonical event without consuming a slot.
INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('second','discovery-1',1,'draft:on_sale');
-- Materializing a candidate replaces its identity, not a second allowance.
INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('fixture','research-1',2,'draft:on_sale');
UPDATE event_research_candidate SET event_id=2 WHERE external_id='research-1';
UPDATE event_research_candidate SET review_state='discarded' WHERE external_id='research-2';
INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('fixture','replacement',3,'draft:on_sale');
DO $$ BEGIN
 IF (SELECT count(*) FROM tdf_event_pilot_keys())<>20 THEN RAISE EXCEPTION 'deduplication or discard semantics failed'; END IF;
 IF EXISTS(SELECT 1 FROM event_discovery_publication_approval) THEN RAISE EXCEPTION 'migration approved publication'; END IF;
END $$;
UPDATE event_research_pilot_control SET approved=true,approved_at=now(),approved_by_party_id='1',approval_reference='fixture pilot only' WHERE control_key='default';
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM event_discovery_publication_approval) THEN RAISE EXCEPTION 'pilot approval granted publication'; END IF;
END $$;
INSERT INTO event_discovery_publication_approval(source_id,approval_reference,approved_by_party_id)
 SELECT id,'explicit fixture publication approval',1 FROM event_discovery_source WHERE source_key='fixture-structured';
DO $$ BEGIN
 BEGIN
  UPDATE event_discovery_publication_approval SET approval_reference='changed';
  RAISE EXCEPTION 'TEST: approval evidence replaced';
 EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
END $$;
UPDATE event_discovery_publication_approval SET revoked_at=now();
DO $$ BEGIN
 BEGIN
  UPDATE event_discovery_publication_approval SET revoked_at=NULL;
  RAISE EXCEPTION 'TEST: approval silently restored';
 EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
END $$;

INSERT INTO event_discovery_publication_approval(source_id,approval_reference,approved_by_party_id)
 SELECT id,'second explicit approval',1 FROM event_discovery_source WHERE source_key='fixture-structured';
UPDATE event_discovery_source SET feed_url='https://official.example/replacement' WHERE source_key='fixture-structured';
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM event_discovery_publication_approval WHERE revoked_at IS NULL) THEN
  RAISE EXCEPTION 'changed source retained previous publication authority'; END IF;
END $$;
