import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

// Run after the isolated notification migration fixture. Execute the handler's
// actual SQL against a public projection that deliberately omits private flags.
const source = readFileSync(new URL('../tdf-hq/src/TDF/Server/Directory.hs', import.meta.url), 'utf8');
const queries = source.split('\n').filter((line) => line.includes("SELECT jsonb_build_object('id',d.id,'searchName'"));
assert.equal(queries.length, 1, 'Find exactly one production alert-context lookup');
let parameter = 0;
const lookup = JSON.parse(queries[0].trim()).replaceAll('?', () => `$${++parameter}`);
assert.equal(parameter, 3);
assert.ok(process.argv[2], 'The isolated fixture database is required');

execFileSync('psql', ['-X', '-v', 'ON_ERROR_STOP=1', '-d', process.argv[2]], {
  stdio: ['pipe', 'inherit', 'inherit'],
  input: `
BEGIN;
ALTER TABLE directory_search_document ADD COLUMN slug text, ADD COLUMN title text;
CREATE TABLE directory_profile(id uuid PRIMARY KEY, canonical_profile_id uuid);
CREATE TABLE external_event_ref(event_id bigint, source_status text);
CREATE VIEW directory_public_search_document AS
 SELECT entity_kind,entity_id,slug,title,expires_at FROM directory_search_document
 WHERE source_status='published' AND visibility='public' AND moderation_status='allowed'
 AND (expires_at IS NULL OR expires_at>now());
CREATE FUNCTION test_alert_context(uuid,bigint,text) RETURNS jsonb LANGUAGE sql AS $lookup$${lookup}$lookup$;
UPDATE directory_search_document SET slug='exact-profile',title='Specific result';
DO $test$
DECLARE delivery uuid; result jsonb;
BEGIN
 SELECT id INTO STRICT delivery FROM directory_alert_delivery WHERE result_id='00000000-0000-0000-0000-000000000021';
 result := test_alert_context(delivery,5,'suppressed');
 IF result->>'title' IS DISTINCT FROM 'Specific result' THEN RAISE EXCEPTION 'owned alert did not resolve exact public result'; END IF;
 IF test_alert_context(delivery,6,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'foreign account saw alert'; END IF;
 IF test_alert_context('00000000-0000-0000-0000-000000000099',5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'missing alert resolved'; END IF;
 UPDATE directory_search_document SET visibility='private';
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'private result leaked'; END IF;
 UPDATE directory_search_document SET visibility='public',moderation_status='blocked';
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'blocked result leaked'; END IF;
 UPDATE directory_search_document SET moderation_status='allowed',source_status='draft';
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'draft result leaked'; END IF;
 UPDATE directory_search_document SET source_status='published',expires_at=now()-interval '1 day';
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'expired result leaked'; END IF;
 UPDATE directory_search_document SET expires_at=NULL,entity_id='00000000-0000-0000-0000-000000000022';
 INSERT INTO directory_profile VALUES('00000000-0000-0000-0000-000000000021','00000000-0000-0000-0000-000000000022');
 result := test_alert_context(delivery,5,'suppressed');
 IF result->>'resultId' IS DISTINCT FROM '00000000-0000-0000-0000-000000000022' THEN RAISE EXCEPTION 'canonical result identity was lost'; END IF;
 UPDATE directory_alert_delivery SET result_kind='event',result_id='42' WHERE id=delivery;
 UPDATE directory_search_document SET entity_kind='event',entity_id='42';
 IF test_alert_context(delivery,5,'suppressed') IS NULL THEN RAISE EXCEPTION 'public event did not resolve'; END IF;
 INSERT INTO external_event_ref VALUES(42,' SUPPRESSED ');
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'live suppressed event leaked'; END IF;
 DELETE FROM external_event_ref;
 DELETE FROM directory_search_document;
 IF test_alert_context(delivery,5,'suppressed') IS NOT NULL THEN RAISE EXCEPTION 'deleted result resolved'; END IF;
END $test$;
ROLLBACK;
`,
});
console.log('Alert-context SQL: exact result, owner scope, canonical identity, and private/expired/suppressed/deleted fallbacks passed.');
