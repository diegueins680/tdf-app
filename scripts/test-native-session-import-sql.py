#!/usr/bin/env python3
"""Generate synthetic PostgreSQL regressions; only the owned DB wrapper executes them."""
import copy
import importlib.util
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('native_importer', ROOT / 'prepare-native-session.py')
native = importlib.util.module_from_spec(spec)
spec.loader.exec_module(native)
test_spec = importlib.util.spec_from_file_location('native_manifest_fixture', ROOT / 'test-prepare-native-session.py')
fixtures = importlib.util.module_from_spec(test_spec)
test_spec.loader.exec_module(fixtures)

m = fixtures.manifest()
m['native']['artistPartyIds'] = [991718]
parent = m['entity']['id']
child = m['native']['chapters'][0]['id']
actual_parent = '44444444-4444-4444-8444-444444444444'
actual_child = '55555555-5555-4555-8555-555555555555'
lit = native.literal

print("""BEGIN;
SET LOCAL statement_timeout='45s';
SET LOCAL lock_timeout='3s';
DO $$ BEGIN
 IF current_database()<>'tdf_native_session_import_test' THEN
   RAISE EXCEPTION 'Requires the database created by the native import fixture wrapper';
 END IF;
END $$;
CREATE FUNCTION pg_temp.native_check(condition boolean,message text) RETURNS void LANGUAGE plpgsql AS $$
BEGIN IF condition IS DISTINCT FROM true THEN RAISE EXCEPTION '%',message; END IF; END $$;
CREATE FUNCTION pg_temp.native_snapshot() RETURNS jsonb LANGUAGE sql AS $$
 SELECT jsonb_build_object(
  'recordings',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM recording t),
  'sessions',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM recording_session t),
  'resources',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM record_external_resource t),
  'relations',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM recording_external_resource t),
  'sessionRelations',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM session_external_resource t),
  'sessionContributors',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM session_contributor t),
  'contributors',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM recording_contributor t),
  'collection',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM collection_recording t),
  'providers',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM external_provider t),
  'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text) FROM audit_log t));
$$;
CREATE TEMP TABLE native_before(label text PRIMARY KEY,value jsonb);
INSERT INTO party(id,display_name,is_org,created_at) VALUES(991718,'Synthetic native correction artist',false,clock_timestamp());
INSERT INTO artist_profile(id,artist_party_id,created_at) VALUES(991718,991718,clock_timestamp());
""")


def seed_recording(identity, code, title, catalog='records-recordings'):
    return f"""INSERT INTO recording(id,catalog_id,code,recording_type_id,title_es,title_en,workflow_state_id,published_revision)
 SELECT {lit(identity)}::uuid,c.id,{lit(code)},r.id,{lit(title)},{lit(title)},s.id,1
 FROM catalog_definition c JOIN workflow_state s ON s.workflow_id=c.workflow_id AND s.code='published'
 CROSS JOIN recording_type_reference r WHERE c.code={lit(catalog)} AND r.code='music-video' AND r.active;"""


print(seed_recording(parent, 'synthetic-source', 'Synthetic canonical parent'))
print(f"""
INSERT INTO external_provider(id,code,name_es,name_en,provider_kind,public_metadata,active)
 VALUES('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa','tdf','Synthetic TDF','Synthetic TDF','native-media',true,true) ON CONFLICT(code) DO NOTHING;
INSERT INTO record_external_resource(id,provider_id,external_code,resource_kind,canonical_url)
 SELECT {lit(actual_parent)}::uuid,id,{lit('native:'+parent)},'video','https://example.invalid/old-parent.mp4' FROM external_provider WHERE code='tdf';
INSERT INTO record_external_resource(id,provider_id,external_code,resource_kind,canonical_url)
 SELECT {lit(actual_child)}::uuid,id,{lit('native:'+child)},'video','https://example.invalid/old-child.mp4' FROM external_provider WHERE code='tdf';
""")
print(native.render_sql(m))
print(f"""
SELECT pg_temp.native_check((SELECT title_es={lit(m['native']['chapters'][0]['title'])} AND duration_ms=10000 FROM recording WHERE id={lit(child)}::uuid),'initial native chapter created');
UPDATE recording SET current_slug='synthetic-editorial-slug',explicit_content=true,sort_order=17,usage_count=9 WHERE id={lit(child)}::uuid;
INSERT INTO native_before SELECT 'child',to_jsonb(r) FROM recording r WHERE id={lit(child)}::uuid;
INSERT INTO native_before SELECT 'resource',to_jsonb(r) FROM record_external_resource r WHERE id={lit(actual_child)}::uuid;
SELECT 'PASS native import initial stable parent/child identities';
""")

corrected = copy.deepcopy(m)
corrected['title'] = 'Corrected synthetic session'
corrected['posterUrl'] = 'https://example.invalid/corrected-poster.jpg'
corrected['native']['artist'] = 'Corrected synthetic artist'
corrected['native']['sources'][0]['url'] = 'https://example.invalid/corrected-video.mp4'
corrected['native']['sources'][1]['url'] = 'https://example.invalid/corrected-audio.m4a'
corrected['native']['chapters'][0].update(title='Corrected synthetic song', startMs=12000, endMs=27000)
print(native.render_sql(corrected))
print(f"""
SELECT pg_temp.native_check((SELECT title_es='Corrected synthetic song' AND title_en='Corrected synthetic song'
 AND description_es='Interpretación de Corrected synthetic artist en Corrected synthetic session'
 AND description_en='Performance from Corrected synthetic session' AND duration_ms=15000
 FROM recording WHERE id={lit(child)}::uuid),'corrected importer-owned recording presentation');
SELECT pg_temp.native_check((SELECT version=(b.value->>'version')::bigint+1
 AND published_revision=(b.value->>'published_revision')::bigint+1
 AND (to_jsonb(r)-ARRAY['title_es','title_en','description_es','description_en','duration_ms','updated_at','version','published_revision'])
     =(b.value-ARRAY['title_es','title_en','description_es','description_en','duration_ms','updated_at','version','published_revision'])
 FROM recording r CROSS JOIN native_before b WHERE r.id={lit(child)}::uuid AND b.label='child'),
 'corrected recording invalidates revisions without changing editorial/workflow fields');
SELECT pg_temp.native_check((SELECT canonical_url='https://example.invalid/corrected-video.mp4'
 AND label_es='Corrected synthetic song' AND label_en='Corrected synthetic song' AND duration_ms=15000
 AND thumbnail_url='https://example.invalid/corrected-poster.jpg'
 AND provider_metadata#>>'{{tdfNative,startMs}}'='12000' AND provider_metadata#>>'{{tdfNative,endMs}}'='27000'
 AND version=(b.value->>'version')::bigint+1
 FROM record_external_resource r CROSS JOIN native_before b WHERE r.id={lit(actual_child)}::uuid AND b.label='resource'),
 'corrected resource presentation and playback metadata agree');
SELECT pg_temp.native_check((SELECT r.title_es=p.provider_metadata#>>'{{tdfNative,chapters,0,title}}'
 FROM recording r CROSS JOIN record_external_resource p WHERE r.id={lit(child)}::uuid AND p.id={lit(actual_parent)}::uuid),
 'recording feed title agrees with full-session chapter title');
SELECT pg_temp.native_check((SELECT title_es='Synthetic canonical parent' FROM recording WHERE id={lit(parent)}::uuid),
 'canonical parent presentation is not importer-owned');
SELECT 'PASS native import corrected recording/resource presentation and revision invalidation';
INSERT INTO native_before VALUES('replay',pg_temp.native_snapshot());
""")
print(native.render_sql(corrected))
print("""
SELECT pg_temp.native_check(pg_temp.native_snapshot()=(SELECT value FROM native_before WHERE label='replay'),
 'exact replay changes no imported row, resource, revision or audit');
SELECT 'PASS native import exact corrected replay';
""")


def expect_identity_denial(name, candidate):
    rendered = native.render_sql(candidate)
    print(f"""DO $case$ DECLARE before_rows jsonb:=pg_temp.native_snapshot(); denied boolean:=false; BEGIN
 BEGIN EXECUTE {lit(rendered)};
 EXCEPTION WHEN SQLSTATE 'P0001' THEN
   IF SQLERRM<>'Expected native chapter identity' THEN RAISE; END IF;
   denied:=true;
 END;
 PERFORM pg_temp.native_check(denied,{lit(name+' must refuse explicitly')});
 PERFORM pg_temp.native_check(pg_temp.native_snapshot()=before_rows,{lit(name+' must roll back all importer rows')});
END $case$;
SELECT {lit('PASS native import '+name+' atomic refusal')};""")

def seed_native_binding(identity):
    resource = native.stable_id('native:'+identity)
    metadata = lit('{"tdfNative":{"parentId":"'+parent+'","isTrack":true}}')
    print(f"""INSERT INTO record_external_resource(id,provider_id,external_code,resource_kind,canonical_url,provider_metadata)
 SELECT {lit(resource)}::uuid,id,{lit('native:'+identity)},'video','https://example.invalid/synthetic-binding.mp4',{metadata}::jsonb
 FROM external_provider WHERE code='tdf';
 INSERT INTO recording_external_resource(recording_id,resource_id,relation_kind,primary_resource,sort_order)
 VALUES({lit(identity)}::uuid,{lit(resource)}::uuid,'native-stream',true,0);""")


code_collision = copy.deepcopy(corrected)
code_collision['native']['chapters'][0]['id'] = '33333333-3333-4333-8333-333333333333'
print(seed_recording('99999999-9999-4999-8999-999999999999', 'tdf-session-track:'+code_collision['native']['chapters'][0]['id'], 'Unrelated code owner'))
expect_identity_denial('code/UUID mismatch', code_collision)
uuid_collision = copy.deepcopy(corrected)
uuid_collision['native']['chapters'][0]['id'] = '77777777-7777-4777-8777-777777777777'
print(seed_recording(uuid_collision['native']['chapters'][0]['id'], 'canonical-unrelated-recording', 'Unrelated UUID owner'))
seed_native_binding(uuid_collision['native']['chapters'][0]['id'])
expect_identity_denial('UUID/code mismatch', uuid_collision)
catalog_collision = copy.deepcopy(corrected)
catalog_collision['native']['chapters'][0]['id'] = '66666666-6666-4666-8666-666666666666'
print(seed_recording(catalog_collision['native']['chapters'][0]['id'], 'tdf-session-track:'+catalog_collision['native']['chapters'][0]['id'], 'Different catalog owner', 'records-sessions'))
seed_native_binding(catalog_collision['native']['chapters'][0]['id'])
expect_identity_denial('catalog mismatch', catalog_collision)
type_collision = copy.deepcopy(corrected)
type_collision['native']['chapters'][0]['id'] = 'aaaaaaaa-1111-4111-8111-111111111111'
print(seed_recording(type_collision['native']['chapters'][0]['id'], 'tdf-session-track:'+type_collision['native']['chapters'][0]['id'], 'Different recording type'))
print(f"UPDATE recording SET recording_type_id=(SELECT id FROM recording_type_reference WHERE code='sound-recording' AND active) WHERE id={lit(type_collision['native']['chapters'][0]['id'])}::uuid;")
seed_native_binding(type_collision['native']['chapters'][0]['id'])
expect_identity_denial('recording type mismatch', type_collision)
other_parent = '88888888-8888-4888-8888-888888888888'
print(seed_recording(other_parent, 'second-synthetic-parent', 'Other published parent'))
parent_collision = copy.deepcopy(corrected)
parent_collision['entity'].update(id=other_parent, expectedCode='second-synthetic-parent')
expect_identity_denial('different native parent binding', parent_collision)

print(f"""
UPDATE recording SET active=false WHERE id={lit(child)}::uuid;
INSERT INTO native_before SELECT 'inactive',to_jsonb(r) FROM recording r WHERE id={lit(child)}::uuid;
""")
inactive = copy.deepcopy(corrected)
inactive['native']['chapters'][0]['title'] = 'Corrected but not reactivated'
print(native.render_sql(inactive))
print(f"""
SELECT pg_temp.native_check((SELECT NOT active AND title_es='Corrected but not reactivated'
 AND workflow_state_id::text=(b.value->>'workflow_state_id') AND current_slug='synthetic-editorial-slug'
 FROM recording r CROSS JOIN native_before b WHERE r.id={lit(child)}::uuid AND b.label='inactive'),
 'metadata correction never reactivates or changes workflow');
SELECT 'PASS native import correction preserves explicit inactive/editorial state';
""")

session_parent = 'bbbbbbbb-1111-4111-8111-111111111111'
session_child = 'cccccccc-1111-4111-8111-111111111111'
session = copy.deepcopy(m)
session['entity'].update(kind='session', id=session_parent, expectedCode='synthetic-session-parent')
session['native']['chapters'][0].update(id=session_child, title='Session original chapter')
print(f"""INSERT INTO recording_session(id,catalog_id,code,session_type_id,title_es,title_en,workflow_state_id,published_revision)
 SELECT {lit(session_parent)}::uuid,c.id,'synthetic-session-parent',t.id,'Canonical session parent','Canonical session parent',s.id,1
 FROM catalog_definition c JOIN workflow_state s ON s.workflow_id=c.workflow_id AND s.code='published'
 CROSS JOIN recording_session_type t WHERE c.code='records-sessions' AND t.code='band-recording' AND t.active;""")
print(native.render_sql(session))
print(f"INSERT INTO native_before SELECT 'session-child',to_jsonb(r) FROM recording r WHERE id={lit(session_child)}::uuid;")
session_corrected = copy.deepcopy(session)
session_corrected['native']['chapters'][0].update(title='Corrected session chapter', startMs=7000, endMs=22000)
session_corrected['native']['sources'][0]['url'] = 'https://example.invalid/corrected-session-parent.mp4'
session_corrected['posterUrl'] = 'https://example.invalid/session-parent-poster.jpg'
print(native.render_sql(session_corrected))
print(f"""SELECT pg_temp.native_check((SELECT title_es='Corrected session chapter' AND title_en='Corrected session chapter'
 AND duration_ms=15000 AND version=(b.value->>'version')::bigint+1
 AND published_revision=(b.value->>'published_revision')::bigint+1
 FROM recording r CROSS JOIN native_before b WHERE r.id={lit(session_child)}::uuid AND b.label='session-child'),
 'session-parent corrected chapter fields and revisions');
SELECT pg_temp.native_check((SELECT canonical_url='https://example.invalid/corrected-session-parent.mp4'
 AND duration_ms=15000 AND label_es='Corrected session chapter'
 AND thumbnail_url='https://example.invalid/session-parent-poster.jpg'
 AND provider_metadata#>>'{{tdfNative,parentId}}'={lit(session_parent)}
 AND provider_metadata#>>'{{tdfNative,startMs}}'='7000' AND provider_metadata#>>'{{tdfNative,endMs}}'='22000'
 FROM record_external_resource WHERE external_code={lit('native:'+session_child)}),
 'session-parent corrected resource fields and binding');
SELECT pg_temp.native_check((SELECT title_es='Canonical session parent' FROM recording_session WHERE id={lit(session_parent)}::uuid),
 'session canonical parent presentation preserved');
SELECT pg_temp.native_check((SELECT count(*)=1 FROM session_external_resource link
 JOIN record_external_resource resource ON resource.id=link.resource_id
 WHERE link.session_id={lit(session_parent)}::uuid AND resource.external_code={lit('native:'+session_parent)}),
 'actual session parent relation used');
INSERT INTO native_before VALUES('session-replay',pg_temp.native_snapshot());""")
print(native.render_sql(session_corrected))
print("""SELECT pg_temp.native_check(pg_temp.native_snapshot()=(SELECT value FROM native_before WHERE label='session-replay'),
 'exact corrected session-parent replay');
SELECT 'PASS native import session-parent correction and exact replay';
ROLLBACK;
""")
