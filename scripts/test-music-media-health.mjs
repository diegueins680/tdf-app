import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { disposableMusicDatabase, musicTestEnvironment } from './lib/music-test-database.mjs';
const root = dirname(dirname(fileURLToPath(import.meta.url)));
const env = musicTestEnvironment({ source: process.env, username: process.env.USER, runtime: process.env.TMPDIR ?? '/tmp' });
const database = `tdf_music_worker_${process.pid}_${randomUUID().replaceAll('-', '')}`;
const command = (program, args, options = {}) => execFileSync(program, args, { env, encoding: 'utf8', timeout: 90000, maxBuffer: 8*1024*1024, ...options }).trim();
const owner = disposableMusicDatabase({ database, command, env });
const args = ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', database];
const sql = input => command('psql', args, { input });
const apply = name => command('psql', [...args, '-f', join(root,'tdf-hq/sql',name)]);
const quote = value => "'"+value.replaceAll("'","''")+"'";
const health = (kind,id) => JSON.parse(sql(`SELECT row_to_json(h) FROM music_media_health h JOIN music_media_source s USING(source_kind,source_id,source_version) WHERE h.source_kind=${quote(kind)} AND h.source_id=${quote(id)}`));
const eligible = (kind,id) => sql(`SELECT music_media_eligible(${quote(kind)},${quote(id)})`) === 't';
const token = () => randomUUID();
const claim = t => JSON.parse(sql(`SELECT COALESCE(music_media_claim('${t}'),'null'::jsonb)`));
const observe = (t,outcome,reason) => sql(`SELECT music_media_observe('${t}',${quote(outcome)},${quote(reason)})`);
function due(kind,id) {
  sql(`UPDATE music_media_health SET next_check_at=now()+interval '1 day',lease_token=NULL,lease_until=NULL; UPDATE music_media_health SET next_check_at=now()-interval '1 second' WHERE source_kind=${quote(kind)} AND source_id=${quote(id)}`);
}
try {
  owner.create();
  for (const file of ['init_schema.sql','2026-07-12_notification_table.sql','2026-08-05_artist_enrichment.sql',
    '2026-08-13_unified_checkout_core.sql','2026-09-04_access_request_notification_types.sql','2026-09-11_music_release_platform.sql',
    '2026-09-15_music_preview_ranges.sql','2026-09-15_music_version_parties.sql','2026-09-15_music_party_details.sql',
    '2026-09-16_music_correction_asset_graph.sql','2026-09-16_music_correction_concurrency.sql','2026-09-16_music_resource_graph_validation.sql',
    '2026-10-11_music_publication_isolation.sql','2026-10-15_music_exact_minor_units.sql']) apply(file);
  const fixtureSource = readFileSync(join(root,'tdf-hq/test/TDF/MusicPublicationSpec.hs'),'utf8');
  const list = fixtureSource.split('[ "CREATE OR REPLACE FUNCTION music_spec_publishable')[1].split(']) []')[0];
  const fixture = ('"CREATE OR REPLACE FUNCTION music_spec_publishable'+list).match(/"(?:[^"\\]|\\.)*"/g).map(JSON.parse).join('\n');
  sql("INSERT INTO party(display_name,is_org,created_at) VALUES ('Music publication spec',FALSE,NOW());\n"+fixture);
  const [release,version] = sql("SELECT release_id,version_id FROM music_spec_publishable('health')").split('|');
  const artist = sql(`SELECT artist_party_id FROM music_release WHERE id='${release}'`);
  sql(`UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='${version}'; SELECT pg_sleep(0.02); SELECT music_publish_due(100);`);
  const asset = sql(`SELECT id FROM music_asset WHERE release_version_id='${version}' AND asset_role='stream_audio' LIMIT 1`);
  assert.equal(sql(`SELECT music_public_asset_accessible('${asset}',NULL)`),'t');
  apply('2026-11-06_music_media_health.sql'); apply('2026-11-06_music_media_health.sql');
  apply('2026-11-06_music_legacy_source_compatibility.sql'); apply('2026-11-06_music_legacy_source_compatibility.sql');
  assert.equal(sql('SELECT music_media_reconcile(1)'),'1');
  assert.equal(sql('SELECT count(*) FROM music_media_health'),'1');
  sql('SELECT music_media_reconcile(100)');
  assert.equal(sql('SELECT music_media_reconcile(100)'),'0');
  due('asset',asset);
  let t=token(); assert.equal(claim(t).id,asset);
  assert.equal(observe(t,'permanent','missing'),'t');
  assert.equal(eligible('asset',asset),true,'One missing response must not hide content');
  assert.equal(sql(`SELECT music_public_asset_accessible('${asset}',NULL)`),'t');
  sql(`UPDATE music_media_health SET first_failure_at=now()-interval '31 minutes' WHERE source_id='${asset}'`);
  due('asset',asset); t=token();claim(t);observe(t,'permanent','missing');
  assert.equal(health('asset',asset).status,'invalid');
  assert.equal(sql(`SELECT music_public_asset_accessible('${asset}',NULL)`),'f','Public signature uses the health gate');
  due('asset',asset); t=token();claim(t);observe(t,'temporary','server');
  assert.equal(health('asset',asset).status,'invalid','Transient checks cannot resurrect confirmed bad content');
  due('asset',asset);t=token();claim(t);observe(t,'valid','decodable');
  assert.equal(health('asset',asset).status,'valid');
  assert.equal(sql(`SELECT music_public_asset_accessible('${asset}',NULL)`),'t','Successful revalidation restores eligibility');
  sql(`UPDATE music_release_version SET state='suspended' WHERE id='${version}'`);
  assert.equal(sql(`SELECT music_public_asset_accessible('${asset}',NULL)`),'f','Health never grants publication rights');
  const legacy=sql(`INSERT INTO artist_release(artist_party_id,title,spotify_url) VALUES (${artist},'Health legacy','https://media.example/a.mp3') RETURNING id`);
  const id=legacy+':spotify';
  apply('2026-10-30_artist_featured_track.sql');
  assert.equal(sql(`SELECT music_set_artist_featured_track(${artist},NULL,${legacy},NULL)`),'t');
  assert.equal(health('legacy',id).status,'pending','Ingestion queues its exact source');
  for (const reason of ['timeout','server','authentication','offline','transport']) {
    due('legacy',id);t=token();claim(t);observe(t,'temporary',reason);
    assert.equal(eligible('legacy',id),true,reason+' is not a permanent verdict');
  }
  due('legacy',id);t=token();claim(t);observe(t,'permanent','missing');
  sql(`UPDATE music_media_health SET first_failure_at=now()-interval '31 minutes' WHERE source_kind='legacy' AND source_id='${id}'`);
  due('legacy',id);t=token();claim(t);observe(t,'permanent','missing');
  assert.equal(eligible('legacy',id),false);
  assert.equal(sql(`SELECT music_artist_featured_track_eligible(${artist},NULL,${legacy},NULL)`),'t','Original preference function reproduces stale availability');
  apply('2026-11-07_music_featured_source_health.sql');apply('2026-11-07_music_featured_source_health.sql');
  assert.equal(sql(`SELECT music_artist_featured_track_eligible(${artist},NULL,${legacy},NULL)`),'f','Featured references obey the same confirmed-invalid gate');
  assert.equal(sql(`SELECT legacy_release_id FROM music_artist_featured_track WHERE artist_party_id=${artist}`),legacy,'Owner preference is preserved');
  due('legacy',id);t=token();claim(t);observe(t,'valid','decodable');
  assert.equal(sql(`SELECT music_artist_featured_track_eligible(${artist},NULL,${legacy},NULL)`),'t','Successful validation restores featured eligibility');
  due('legacy',id);t=token();const old=claim(t);
  sql(`UPDATE artist_release SET spotify_url='https://media.example/b.mp3' WHERE id=${legacy}`);
  assert.equal(observe(t,'permanent','missing'),'f','Old source result cannot overwrite replacement');
  assert.notEqual(health('legacy',id).source_version,old.version);
  assert.equal(eligible('legacy',id),true);
  due('legacy',id);t=token();claim(t);
  const other=token();assert.equal(claim(other),null,'A live claim deduplicates concurrent workers');
  sql(`UPDATE music_media_health SET lease_until=now()-interval '1 second' WHERE lease_token='${t}'`);
  const newer=token();assert.equal(claim(newer).id,id);
  assert.equal(observe(t,'valid','decodable'),'f','Expired worker cannot commit over a new lease');
  assert.equal(observe(newer,'valid','decodable'),'t');
  for (const url of ['http://media.example/audio.mp3','/uploads/authorized.mp3','media.example/audio.mp3','//media.example/audio.mp3']) {
    sql(`UPDATE artist_release SET spotify_url='${url}' WHERE id=${legacy}`);
    assert.equal(eligible('legacy',id),true,'Supported browser URL retains an authorized source: '+url);
    const expected = url.startsWith('/') || url.startsWith('http:') ? url : 'https://'+url;
    assert.equal(sql(`SELECT locator->>'url' FROM music_media_source WHERE source_kind='legacy' AND source_id='${id}'`),expected);
  }
  for (const url of ['javascript:alert(1)','data:audio/mpeg,unsafe','https://user:secret@media.example/a.mp3','https://youtube.com/watch?v=provider']) {
    sql(`UPDATE artist_release SET spotify_url='${url}' WHERE id=${legacy}`);
    assert.equal(eligible('legacy',id),false,'Unsafe/provider URL never becomes native candidate');
  }
  sql(`UPDATE artist_release SET spotify_url='https://media.example/after.mp3' WHERE id=${legacy}`);
  due('legacy',id);t=token();claim(t);observe(t,'valid','decodable');
  apply('2026-11-06_music_media_health_rollback.sql');
  assert.equal(health('legacy',id).status,'valid','Application recovery retains diagnosis');
  console.log('PASS PostgreSQL media health: ingestion, bounded resumable reconciliation, confirmation, temporary failures, source replacement, deduplication, lease fencing, restoration and publication gate');
} finally { await owner.cleanup(); }
