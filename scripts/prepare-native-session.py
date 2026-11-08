#!/usr/bin/env python3
"""Prepare an auditable Records-native import; never connects to production.

The caller uploads only the inspected derivatives, verifies their public URLs,
then applies the rendered SQL through the normal authorized operator workflow.
Existing published Records identities remain the catalog source of truth.
"""
import argparse, hashlib, json, os, pathlib, re, subprocess, tempfile, uuid
from urllib.parse import urlparse

NAMESPACE = uuid.UUID('7a2ac340-5653-40c1-886a-65b31b4c06b3')
STATES = {'awaiting_master', 'processing', 'ready', 'failed'}

def require(value, message):
    if not value:
        raise ValueError(message)

def stable_id(value):
    return str(uuid.uuid5(NAMESPACE, value))

def literal(value):
    return "'" + str(value).replace("'", "''") + "'"

def checksum(path):
    sha = hashlib.sha256()
    with path.open('rb') as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b''):
            sha.update(chunk)
    return sha.hexdigest()

def validate_sql_output(output, input_path):
    require(output.resolve() != input_path.resolve()
            and not (output.exists() and output.samefile(input_path)),
            'SQL output must not replace the input manifest')

def write_private_sql(output, sql, input_path):
    validate_sql_output(output, input_path)
    fd, temporary = tempfile.mkstemp(prefix='.' + output.name + '.', suffix='.tmp', dir=output.parent)
    temporary = pathlib.Path(temporary)
    try:
        with os.fdopen(fd, 'w', encoding='utf-8') as stream:
            os.fchmod(stream.fileno(), 0o600)
            stream.write(sql)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, output)
    finally:
        temporary.unlink(missing_ok=True)

def inspect_file(path):
    require(path.is_file() and not path.is_symlink(), 'Media must be a regular local file')
    data = json.loads(subprocess.check_output(['ffprobe', '-v', 'error', '-show_entries',
        'format=duration,size:stream=codec_type,codec_name,width,height', '-of', 'json', str(path)], text=True))
    return {'path': str(path.resolve()), 'sha256': checksum(path), 'bytes': path.stat().st_size,
            'durationMs': round(float(data['format'].get('duration', 0)) * 1000), 'streams': data['streams']}

def validate(manifest):
    require(manifest['schemaVersion'] == 1, 'Unknown manifest schema')
    require(manifest['entity']['kind'] in {'recording', 'session'}, 'Expected existing Records entity')
    uuid.UUID(manifest['entity']['id'])
    require(manifest['entity'].get('expectedCode'), 'Expected existing canonical entity code')
    native = manifest['native']
    require(native['schemaVersion'] == 1 and native['kind'] == 'session', 'Invalid native session metadata')
    require(native['state'] in STATES, 'Unknown processing state')
    require(native['artistPartyIds'] and all(type(i) is int and i > 0 for i in native['artistPartyIds']), 'Artist identities must be explicit')
    require(type(native['durationMs']) is int and native['durationMs'] > 0, 'Expected measured session duration')
    previous = 0
    ids = {uuid.UUID(manifest['entity']['id'])}
    for chapter in native.get('chapters', []):
        chapter_id = uuid.UUID(chapter['id'])
        require(chapter_id not in ids and chapter['title'].strip(), 'Invalid chapter identity/title')
        require(0 <= previous <= chapter['startMs'] < chapter['endMs'] <= native['durationMs'], 'Overlapping/out-of-source chapter')
        previous = chapter['endMs']; ids.add(chapter_id)
    for source in native.get('sources', []):
        url = urlparse(source['url'])
        require(url.scheme == 'https' and url.hostname and not url.username and not url.password, 'Expected HTTPS delivery URL')
        require(not re.search(r'(^|\.)(youtube\.com|youtu\.be|ytimg\.com|googlevideo\.com)$', url.hostname), 'YouTube is not a native source')
        require(source['mediaType'] in {'video/mp4', 'audio/mp4', 'audio/mpeg'}, 'Unsupported native format')
        require(source['quality'] in {'low', 'medium', 'high', 'lossless'}, 'Unknown delivery quality')
    if native['state'] == 'ready':
        require(all(any(s['mediaType'].startswith(kind + '/') for s in native['sources']) for kind in ('video', 'audio')), 'Ready sessions require video AND independent audio')
    return native

def render_sql(manifest):
    native = validate(manifest)
    parent_id = manifest['entity']['id']; kind = manifest['entity']['kind']
    table = 'recording' if kind == 'recording' else 'recording_session'
    relation = 'recording_external_resource' if kind == 'recording' else 'session_external_resource'
    parent_column = 'recording_id' if kind == 'recording' else 'session_id'
    fingerprint = hashlib.sha256(json.dumps(manifest, sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    provider = stable_id('provider:tdf')
    resource = stable_id('native:' + parent_id)
    metadata = literal(json.dumps({'tdfNative': native}, ensure_ascii=False)) + '::jsonb'
    preparation = literal(json.dumps({'tool': 'prepare-native-session.py', 'declaredState': native['state']})) + '::jsonb'
    source_url = next((s['url'] for s in native.get('sources', []) if s['mediaType'].startswith('video/')), manifest.get('publicUrl', 'https://www.tdfrecords.net/records'))
    poster = literal(manifest['posterUrl']) if manifest.get('posterUrl') else 'NULL'
    sql = f'''SET LOCAL lock_timeout='3s'; SET LOCAL statement_timeout='45s';
SELECT pg_advisory_xact_lock(hashtextextended('records-native-session:{parent_id}',0));
LOCK TABLE {table},record_external_resource,external_provider,{relation},recording,recording_external_resource,recording_contributor,collection_recording IN SHARE ROW EXCLUSIVE MODE;
DO $import$ DECLARE parent_resource_key uuid; resource_key uuid; BEGIN
IF EXISTS(SELECT 1 FROM audit_log WHERE action='records_native_session_imported' AND diff::jsonb->>'manifestSha256'={literal(fingerprint)}) THEN RETURN; END IF;
IF NOT EXISTS(SELECT 1 FROM {table} item JOIN workflow_state state ON state.id=item.workflow_state_id WHERE item.id={literal(parent_id)}::uuid AND item.code={literal(manifest['entity']['expectedCode'])} AND item.active AND state.code='published') THEN RAISE EXCEPTION 'Expected existing published Records entity'; END IF;
IF EXISTS(SELECT 1 FROM party p WHERE p.id=ANY(ARRAY{native['artistPartyIds']}::bigint[]) AND NOT EXISTS(SELECT 1 FROM artist_profile a WHERE a.artist_party_id=p.id)) OR (SELECT count(*) FROM party WHERE id=ANY(ARRAY{native['artistPartyIds']}::bigint[]))<>{len(set(native['artistPartyIds']))} THEN RAISE EXCEPTION 'Expected canonical artist profiles'; END IF;
INSERT INTO external_provider(id,code,name_es,name_en,provider_kind,public_metadata,active) VALUES({literal(provider)}::uuid,'tdf','TDF Records','TDF Records','native-media',true,true) ON CONFLICT(code) DO NOTHING;
INSERT INTO record_external_resource(id,provider_id,external_code,resource_kind,canonical_url,label_es,label_en,duration_ms,thumbnail_url,availability,verified_at,provider_metadata)
SELECT {literal(resource)}::uuid,id,{literal('native:'+parent_id)},'video',{literal(source_url)},'Reproducir en TDF','Play on TDF',{native['durationMs']},{poster},NULL,NULL,{metadata} FROM external_provider WHERE code='tdf'
ON CONFLICT(provider_id,resource_kind,external_code) DO UPDATE SET canonical_url=excluded.canonical_url,thumbnail_url=excluded.thumbnail_url,duration_ms=excluded.duration_ms,provider_metadata=excluded.provider_metadata,updated_at=now(),version=record_external_resource.version+1 RETURNING id INTO parent_resource_key;
INSERT INTO {relation}({parent_column},resource_id,relation_kind,primary_resource,sort_order) VALUES({literal(parent_id)}::uuid,parent_resource_key,'native-stream',true,0) ON CONFLICT({parent_column},resource_id,relation_kind) DO NOTHING;
UPDATE {table} SET updated_at=now(),version=version+1,published_revision=published_revision+1 WHERE id={literal(parent_id)}::uuid;
'''
    # Chapters remain linked to the original published Records resource. They do
    # not fabricate new masters, purchases, music entitlements or account grants.
    for chapter in native.get('chapters', []):
        code = 'tdf-session-track:' + chapter['id']
        child_resource = stable_id('native:' + chapter['id'])
        child = {**native, 'isTrack': True, 'parentId': parent_id, 'chapters': [], 'startMs': chapter['startMs'], 'endMs': chapter['endMs']}
        child_json = literal(json.dumps({'tdfNative': child}, ensure_ascii=False)) + '::jsonb'
        sql += f'''IF EXISTS(SELECT 1 FROM recording WHERE id={literal(chapter['id'])}::uuid AND
 (code IS DISTINCT FROM {literal(code)}
 OR catalog_id IS DISTINCT FROM (SELECT id FROM catalog_definition WHERE code='records-recordings')
 OR recording_type_id IS DISTINCT FROM (SELECT id FROM recording_type_reference WHERE code='music-video' AND active)))
 OR EXISTS(SELECT 1 FROM recording WHERE code={literal(code)} AND id<>{literal(chapter['id'])}::uuid)
 OR (EXISTS(SELECT 1 FROM recording WHERE id={literal(chapter['id'])}::uuid) AND NOT EXISTS(
 SELECT 1 FROM recording_external_resource link JOIN record_external_resource resource ON resource.id=link.resource_id
 JOIN external_provider provider ON provider.id=resource.provider_id
 WHERE link.recording_id={literal(chapter['id'])}::uuid AND link.relation_kind='native-stream'
 AND provider.code='tdf' AND resource.resource_kind='video' AND resource.external_code={literal('native:'+chapter['id'])}
 AND resource.provider_metadata#>>'{{tdfNative,parentId}}'={literal(parent_id)}
 AND resource.provider_metadata#>>'{{tdfNative,isTrack}}'='true'))
 THEN RAISE EXCEPTION 'Expected native chapter identity'; END IF;
INSERT INTO recording(id,catalog_id,code,recording_type_id,title_es,title_en,description_es,description_en,duration_ms,workflow_state_id,sort_order,active,published_revision)
SELECT {literal(chapter['id'])}::uuid,(SELECT id FROM catalog_definition WHERE code='records-recordings'),{literal(code)},(SELECT id FROM recording_type_reference WHERE code='music-video' AND active),{literal(chapter['title'])},{literal(chapter['title'])},{literal('Interpretación de '+native['artist']+' en '+manifest['title'])},{literal('Performance from '+manifest['title'])},{chapter['endMs']-chapter['startMs']},workflow_state_id,0,true,1 FROM {table} WHERE id={literal(parent_id)}::uuid
ON CONFLICT(code) DO UPDATE SET title_es=excluded.title_es,title_en=excluded.title_en,
 description_es=excluded.description_es,description_en=excluded.description_en,duration_ms=excluded.duration_ms,
 updated_at=now(),version=recording.version+1,published_revision=recording.published_revision+1
 WHERE recording.id=excluded.id;
INSERT INTO recording_contributor(recording_id,contributor_id,credit_role,sort_order,primary_credit)
SELECT {literal(chapter['id'])}::uuid,contributor_id,credit_role,sort_order,primary_credit FROM {'recording_contributor' if kind == 'recording' else 'session_contributor'} WHERE {parent_column}={literal(parent_id)}::uuid ON CONFLICT DO NOTHING;
INSERT INTO collection_recording(collection_id,recording_id,sort_order) SELECT collection.id,{literal(chapter['id'])}::uuid,(SELECT coalesce(max(sort_order),0)+1 FROM collection_recording WHERE collection_id=collection.id) FROM editorial_collection collection WHERE collection.code='tdf-records-recordings' AND collection.active ON CONFLICT DO NOTHING;
INSERT INTO record_external_resource(id,provider_id,external_code,resource_kind,canonical_url,label_es,label_en,duration_ms,thumbnail_url,provider_metadata)
SELECT {literal(child_resource)}::uuid,id,{literal('native:'+chapter['id'])},'video',{literal(source_url)},{literal(chapter['title'])},{literal(chapter['title'])},{chapter['endMs']-chapter['startMs']},{poster},{child_json} FROM external_provider WHERE code='tdf'
ON CONFLICT(provider_id,resource_kind,external_code) DO UPDATE SET canonical_url=excluded.canonical_url,
 label_es=excluded.label_es,label_en=excluded.label_en,duration_ms=excluded.duration_ms,
 thumbnail_url=excluded.thumbnail_url,provider_metadata=excluded.provider_metadata,
 updated_at=now(),version=record_external_resource.version+1 RETURNING id INTO resource_key;
INSERT INTO recording_external_resource(recording_id,resource_id,relation_kind,primary_resource,sort_order) VALUES({literal(chapter['id'])}::uuid,resource_key,'native-stream',true,0) ON CONFLICT DO NOTHING;
'''
    sql += f'''INSERT INTO audit_log(actor_id,entity,entity_id,action,diff,created_at) VALUES(NULL,'{table}',{literal(parent_id)},'records_native_session_imported',jsonb_build_object('manifestSha256',{literal(fingerprint)},'entityId',{literal(parent_id)},'resourceId',parent_resource_key,'native',{literal(json.dumps(native,ensure_ascii=False))}::jsonb,'preparation',{preparation}),clock_timestamp());
END $import$;
SELECT json_build_object('entityId',{literal(parent_id)},'manifestSha256',{literal(fingerprint)},'imported',EXISTS(SELECT 1 FROM audit_log WHERE action='records_native_session_imported' AND diff::jsonb->>'manifestSha256'={literal(fingerprint)}));
'''
    return sql

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('manifest', type=pathlib.Path)
    parser.add_argument('--sql', type=pathlib.Path)
    parser.add_argument('--inspect-media', action='store_true')
    args = parser.parse_args()
    if args.sql:
        validate_sql_output(args.sql, args.manifest)
    manifest = json.loads(args.manifest.read_text()); validate(manifest)
    if args.inspect_media:
        print(json.dumps([inspect_file(pathlib.Path(f)) for f in manifest.get('localFiles', [])], indent=2))
    if args.sql:
        write_private_sql(args.sql, render_sql(manifest), args.manifest)
        print(json.dumps({'sql': str(args.sql), 'sha256': checksum(args.sql), 'productionWritten': False}))

if __name__ == '__main__':
    main()
