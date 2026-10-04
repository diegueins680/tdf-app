import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { mkdtempSync, openSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
const db = process.env.TDF_IDENTITY_HTTP_DATABASE_URL;
const binary = process.env.TDF_IDENTITY_HTTP_SERVER_BIN;
assert.ok(binary && db, 'isolated database and tested backend binary required');
const url = new URL(db);
assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname) || (process.env.CI === 'true' && url.hostname === 'postgres'));
assert.match(url.pathname, /_test$/);
const serverDb = new URL(db);
serverDb.searchParams.set('application_name', 'tdf_identity_http_fixture');
const port = Number(process.env.TDF_IDENTITY_HTTP_PORT ?? 18631);
assert.ok(Number.isInteger(port) && port > 1024 && port < 65536);
const base = `http://127.0.0.1:${port}`;
const sql = query => {
  const result = spawnSync('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8' });
  assert.equal(result.status, 0, result.stderr);
  return result.stdout.trim();
};
assert.equal(sql("SELECT count(*) FROM party WHERE display_name LIKE 'Identity HTTP %'"), '0', 'requires unused synthetic fixtures');
let occupied = false;
try { await fetch(`${base}/health`); occupied = true; } catch { /* unoccupied */ }
assert.equal(occupied, false, 'refusing occupied test port');
const runtime = mkdtempSync(join(tmpdir(), 'tdf-identity-http-'));
const logPath = join(runtime, 'backend.log');
const log = openSync(logPath, 'w', 0o600);
const server = spawn(binary, [], { env: {
  PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: serverDb.toString(), APP_PORT: String(port),
  RESET_DB: 'false', RUN_MIGRATIONS: 'false', SEED_DB: 'false', DEFAULT_LOCALE: 'es',
  HQ_ASSETS_DIR: runtime, EVENT_DISCOVERY_ENABLED: 'false', ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
}, stdio: ['ignore', log, log] });
try {
  let healthy = false;
  for (let i = 0; i < 90; i++) {
    try { healthy = (await (await fetch(`${base}/health`)).json()).db === 'ok'; } catch { /* starting */ }
    if (healthy) break;
    assert.equal(server.exitCode, null, readFileSync(logPath, 'utf8'));
    await new Promise(resolve => setTimeout(resolve, 500));
  }
  assert.ok(healthy, readFileSync(logPath, 'utf8'));
  sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('Identity HTTP operator A',false,now()),('Identity HTTP operator B',false,now()),('Identity HTTP denied',false,now());
    INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT p.id,r.id,'bootstrap',true FROM party p CROSS JOIN security_role r WHERE p.display_name IN ('Identity HTTP operator A','Identity HTTP operator B') AND r.code='admin' AND r.active;
    INSERT INTO api_token(token,party_id,label,active) SELECT 'synthetic-identity-'||CASE display_name WHEN 'Identity HTTP operator A' THEN 'a' WHEN 'Identity HTTP operator B' THEN 'b' ELSE 'denied' END,id,'Synthetic identity HTTP fixture',true FROM party WHERE display_name LIKE 'Identity HTTP %';`);
  const actor = Number(sql("SELECT id FROM party WHERE display_name='Identity HTTP operator A'"));
  const headers = who => who ? { Authorization: `Bearer synthetic-identity-${who}` } : {};
  const body = { cDisplayName: 'Identity HTTP contact', cIsOrg: false, cPrimaryEmail: 'shared@example.test' };
  const key = 'identity-http-request-0001';
  const post = (who, requestKey = key, payload = body) => fetch(`${base}/parties`, { method: 'POST', headers: { ...headers(who), 'Content-Type': 'application/json', 'Idempotency-Key': requestKey }, body: JSON.stringify(payload) });
  assert.equal((await post(null)).status, 401);
  assert.equal((await post('denied')).status, 403);
  assert.equal((await fetch(`${base}/parties`, { method: 'POST', headers: { ...headers('a'), 'Content-Type': 'application/json' }, body: JSON.stringify(body) })).status, 400, 'legacy keyless creation must not bypass replay protection');
  const created = await Promise.all(Array.from({ length: 8 }, async () => {
    const response = await post('a'); assert.equal(response.status, 200); return response.json();
  }));
  const survivor = created[0].partyId;
  assert.equal(new Set(created.map(p => p.partyId)).size, 1, 'concurrent HTTP retries must create one contact');
  assert.equal((await post('a', key, { ...body, cDisplayName: 'changed payload' })).status, 409);
  const otherActor = await post('b'); assert.equal(otherActor.status, 200);
  assert.notEqual((await otherActor.json()).partyId, survivor, 'request keys must be actor-scoped');
  const distinct = await post('a', 'identity-http-request-0002'); assert.equal(distinct.status, 200);
  const retired = (await distinct.json()).partyId;
  assert.notEqual(retired, survivor, 'shared contact details do not establish identity');
  const operation = randomUUID(), caseId = randomUUID();
  sql(`INSERT INTO identity_reconciliation_case(id,member_ids,status,evidence,before_parties,reason,reviewed_by,reviewed_at)
    SELECT '${caseId}',ARRAY[${survivor},${retired}],'confirmed',jsonb_build_object('basis','verified-source-subject','issuer','synthetic-http','scope','synthetic-only','subject','synthetic-contact','evidence_reference','synthetic-http-fixture-only','external_reference_review','no-unresolved-references','member_ids',ARRAY[${survivor},${retired}]),jsonb_agg(to_jsonb(p) ORDER BY p.id),'Synthetic whole-group identity fixture',${actor},now() FROM party p WHERE id IN (${survivor},${retired});`);
  const plan = JSON.parse(sql(`SELECT identity_merge_plan('${caseId}');`));
  assert.equal(plan.can_execute, true, JSON.stringify(plan.blockers));
  assert.equal(plan.survivor, survivor);
  const execute = () => JSON.parse(sql(`SELECT identity_execute_merge('${operation}','${caseId}','${plan.fingerprint}');`));
  assert.equal(execute().status, 'applied'); assert.equal(execute().status, 'already-applied');
  assert.equal((await post('a', 'identity-http-request-0002')).status, 409, 'retired request cannot silently reuse canonical identity');
  const edit = (id, who, notes) => fetch(`${base}/parties/${id}`, { method: 'PUT', headers: { ...headers(who), 'Content-Type': 'application/json' }, body: JSON.stringify({ uNotes: notes }) });
  assert.equal((await edit(retired, 'a', 'must not write')).status, 409, 'archived edits need an understandable conflict');
  assert.equal((await edit(retired, 'denied', 'must not write')).status, 403);

  const listed = await (await fetch(`${base}/parties?limit=200`, { headers: headers('a') })).json();
  assert.ok(!listed.some(p => p.partyId === retired));
  assert.equal((await (await fetch(`${base}/parties/${retired}`, { headers: headers('a') })).json()).partyId, survivor);
  assert.equal((await fetch(`${base}/parties/${retired}`, { headers: headers('denied') })).status, 403);
  assert.equal((await fetch(`${base}/parties/${retired}`)).status, 401);
  assert.equal((await edit(survivor, 'a', 'unrelated later edit')).status, 200);
  assert.equal(JSON.parse(sql(`SELECT identity_rollback_merge('${operation}');`)).status, 'reverted');
  assert.equal(JSON.parse(sql(`SELECT identity_rollback_merge('${operation}');`)).status, 'already-reverted');
  assert.equal(sql(`SELECT notes FROM party WHERE id=${survivor}`), 'unrelated later edit');
  assert.equal((await (await fetch(`${base}/parties/${retired}`, { headers: headers('a') })).json()).partyId, retired);
  assert.equal(sql(`SELECT count(*) FROM identity_party_archive WHERE party_id=${retired}`), '0');
  // ID-ARTIST-CLAIM-001: contact data, including a matching public email,
  // is never proof that an anonymous registrant controls an existing Party.
  sql(`INSERT INTO party(display_name,is_org,primary_email,created_at) VALUES
    ('Identity HTTP artist no email',false,NULL,now()),
    ('Identity HTTP artist matching email',false,'claimant@example.test',now());
    INSERT INTO artist_profile(artist_party_id,slug,created_at)
      SELECT id,'identity-http-claim-'||id,now() FROM party
      WHERE display_name IN ('Identity HTTP artist no email','Identity HTTP artist matching email');`);
  const artistIds = JSON.parse(sql(`SELECT json_agg(id ORDER BY id) FROM party
    WHERE display_name IN ('Identity HTTP artist no email','Identity HTTP artist matching email')`));
  const signupBody = { firstName: 'Identity HTTP registrant', lastName: '', email: 'claimant@example.test',
    password: 'synthetic-claim-password-42', termsAccepted: true, termsVersion: 'tdf-account-terms-v1' };
  const signup = payload => fetch(`${base}/signup`, { method: 'POST',
    headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(payload) });
  const identitySnapshot = () => sql(`SELECT jsonb_build_object(
    'parties',(SELECT jsonb_agg(to_jsonb(p) ORDER BY id) FROM party p),
    'credentials',(SELECT jsonb_agg(to_jsonb(c) ORDER BY id) FROM user_credential c),
    'tokens',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM api_token t),
    'roles',(SELECT jsonb_agg(to_jsonb(r) ORDER BY id) FROM party_security_role r))`);
  const beforeRejectedClaim = identitySnapshot();
  for (const claimArtistId of artistIds) {
    const response = await signup({ ...signupBody, claimArtistId });
    assert.equal(response.status, 403, 'artist claim must fail regardless of stored contact data');
    assert.equal(response.headers.get('set-cookie'), null, 'rejected claim must not issue a session');
  }
  assert.equal(identitySnapshot(), beforeRejectedClaim, 'rejected signup must not mutate identity, roles or sessions');
  const independentResponse = await signup(signupBody);
  assert.equal(independentResponse.status, 200, await independentResponse.clone().text());
  const independent = await independentResponse.json();
  assert.ok(!artistIds.includes(independent.partyId), 'independent signup must never adopt an existing artist');
  assert.ok(!independent.roles.includes('Artist'), 'signup must not self-assign the artist role');
  const claimantHeaders = { Authorization: `Bearer ${independent.token}`, 'Content-Type': 'application/json' };
  const targetResponse = await fetch(`${base}/directory/artist-claim-targets/${artistIds[0]}`, {
    method: 'PUT', headers: claimantHeaders,
  });
  assert.equal(targetResponse.status, 200, await targetResponse.clone().text());
  const target = await targetResponse.json();
  const claimResponse = await fetch(`${base}/directory/claims`, { method: 'POST',
    headers: { ...claimantHeaders, 'Idempotency-Key': 'identity-http-reviewed-claim' },
    body: JSON.stringify({ profileId: target.id, claimType: 'profile', evidence: [{ note: 'Synthetic review request' }] }),
  });
  assert.equal(claimResponse.status, 201, await claimResponse.clone().text());
  assert.equal((await claimResponse.json()).status, 'submitted');
  assert.equal(sql(`SELECT count(*) FROM directory_profile_manager WHERE account_party_id=${independent.partyId}`), '0',
    'submission is not approval and must not grant management');
  assert.equal(sql(`SELECT count(*) FROM user_credential WHERE party_id IN (${artistIds.join(',')})`), '0',
    'review request must not transfer artist identity');
  console.log('Artist signup HTTP: anonymous claims denied without effects; independent account submits a claim without ownership.');
  // ID-SESSION-001/002: exercise actual handlers, with synthetic local credentials.
  const credentialId = Number(sql(`SELECT id FROM user_credential WHERE party_id=${independent.partyId}`));
  const session = async token => {
    const response = await fetch(`${base}/session`, { headers: { Authorization: `Bearer ${token}` } });
    assert.equal(response.status, 200);
    return response.json();
  };
  const adminUpdate = async payload => fetch(`${base}/admin/users/${credentialId}`, {
    method: 'PATCH', headers: { ...headers('a'), 'Content-Type': 'application/json' }, body: JSON.stringify(payload),
  });
  const login = (password, username = signupBody.email) => fetch(`${base}/login`, { method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ username, password }), signal: AbortSignal.timeout(30000) });
  const addTokens = suffix => {
    const google = `synthetic-google-${suffix}`, service = `synthetic-service-${suffix}`;
    sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
      ('${google}',${independent.partyId},'google-login:claimant@example.test',true),
      ('${service}',${independent.partyId},'Synthetic service scope',true);`);
    return { google, service };
  };
  const forbiddenAdmin = await fetch(`${base}/admin/users/${credentialId}`, {
    method: 'PATCH', headers: { ...headers('denied'), 'Content-Type': 'application/json' },
    body: JSON.stringify({ uauActive: false }),
  });
  assert.equal(forbiddenAdmin.status, 403, 'admin credential mutation requires backend authorization');
  sql(`INSERT INTO user_credential(party_id,username,password_hash,active)
    SELECT party_id,'identity-http-alternate',password_hash,true FROM user_credential WHERE id=${credentialId}`);
  const families = addTokens('disable');
  assert.ok(await session(independent.token)); assert.ok(await session(families.google));
  assert.equal((await adminUpdate({ uauActive: false })).status, 200);
  assert.equal(await session(independent.token), null);
  assert.equal(await session(families.google), null);
  assert.ok(await session(families.service), 'service tokens retain their separate policy');
  assert.equal((await login(signupBody.password)).status, 401, 'disabled credentials cannot log in');
  const alternate = await login(signupBody.password, 'identity-http-alternate');
  assert.equal(alternate.status, 200, 'a distinct active credential can authenticate independently');
  assert.ok(await session((await alternate.json()).token));
  assert.equal((await adminUpdate({ uauActive: true })).status, 200);
  assert.equal(await session(independent.token), null, 're-enable must not revive revoked sessions');
  const replacement = await login(signupBody.password);
  assert.equal(replacement.status, 200); const replacementSession = await replacement.json();
  const passwordFamilies = addTokens('password');
  assert.equal((await adminUpdate({ uauPassword: 'synthetic-admin-password-42' })).status, 200);
  assert.equal(await session(replacementSession.token), null);
  assert.equal(await session(passwordFamilies.google), null);
  assert.ok(await session(passwordFamilies.service));

  let winningPassword;
  const resetToken = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${resetToken}',${independent.partyId},'password-reset:claimant@example.test',true);`);
  const confirm = (token, password) => fetch(`${base}/v1/password-reset/confirm`, {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ token, newPassword: password }), signal: AbortSignal.timeout(30000),
  });
  // Hold both rows so the unfixed implementation reads the challenge twice and
  // blocks at UPDATE; the corrected implementation blocks before validation.
  const barrier = spawn('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
  const barrierOutput = [];
  try {
    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('lifecycle barrier did not acquire locks')), 10000);
      barrier.stdout.on('data', data => {
        barrierOutput.push(data.toString());
        if (barrierOutput.join('').includes('lifecycle-barrier-ready')) { clearTimeout(timeout); resolve(); }
      });
      barrier.once('error', error => { clearTimeout(timeout); reject(error); });
      barrier.stdin.write(`BEGIN; SELECT id FROM party WHERE id=${independent.partyId} FOR NO KEY UPDATE;
        SELECT id FROM user_credential WHERE id=${credentialId} FOR UPDATE;
        SELECT 'lifecycle-barrier-ready';\n`);
    });
    const pending = [confirm(resetToken, 'synthetic-concurrent-a-42'), confirm(resetToken, 'synthetic-concurrent-b-42')];
    let blocked = false;
    for (let attempt = 0; attempt < 100; attempt++) {
      if (Number(sql("SELECT count(*) FROM pg_stat_activity WHERE application_name='tdf_identity_http_fixture' AND wait_event_type='Lock'")) >= 2) { blocked = true; break; }
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    assert.ok(blocked, 'both actual HTTP confirmations must reach the controlled lock barrier');
    barrier.stdin.end('COMMIT;\n');
    const outcomes = await Promise.all(pending);
    assert.deepEqual(outcomes.map(response => response.status).sort(), [200, 400], 'exactly one reset may consume the challenge');
    winningPassword = ['synthetic-concurrent-a-42', 'synthetic-concurrent-b-42'][outcomes.findIndex(response => response.status === 200)];
    const winner = await outcomes.find(response => response.status === 200).json();
    assert.ok(await session(winner.token));
    assert.equal((await confirm(resetToken, 'synthetic-replay-password-42')).status, 400);
  } finally { barrier.kill('SIGTERM'); }

  const changeFamilies = addTokens('change');
  const changeLogin = await login(winningPassword); assert.equal(changeLogin.status, 200);
  const oldSession = await changeLogin.json();
  const changeResponse = await fetch(`${base}/password/change`, {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ username: signupBody.email, currentPassword: winningPassword, newPassword: 'synthetic-changed-password-42' }),
  });
  assert.equal(changeResponse.status, 200, await changeResponse.clone().text());
  assert.equal(await session(oldSession.token), null);
  assert.equal(await session(changeFamilies.google), null);
  assert.ok(await session(changeFamilies.service));
  assert.ok(await session((await changeResponse.json()).token));

  // Database failure after credential mutation must roll back challenge and hash.
  const failureToken = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${failureToken}',${independent.partyId},'password-reset:claimant@example.test',true);
    CREATE FUNCTION identity_http_reject_session() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
      IF NEW.party_id=${independent.partyId} AND NEW.label LIKE 'password-login:%' THEN
        RAISE EXCEPTION 'synthetic session issuance failure'; END IF; RETURN NEW; END $$;
    CREATE TRIGGER identity_http_reject_session BEFORE INSERT ON api_token
      FOR EACH ROW EXECUTE FUNCTION identity_http_reject_session();`);
  const beforeFailedIssuance = identitySnapshot();
  try {
    assert.equal((await confirm(failureToken, 'synthetic-must-rollback-42')).status, 500);
    assert.equal(identitySnapshot(), beforeFailedIssuance, 'failed issuance must roll back hash, challenge and revocations');
  } finally {
    sql('DROP TRIGGER identity_http_reject_session ON api_token; DROP FUNCTION identity_http_reject_session();');
  }
  console.log('Credential lifecycle HTTP: disable, re-enable, password replacement, scoped revocation, deterministic reset race and issuance rollback passed.');
  console.log('Identity HTTP: authorization, concurrent replay, actor scope, shared details, archival, canonical access and rollback passed.');
} finally {
  server.kill('SIGTERM');
  await new Promise(resolve => { if (server.exitCode !== null) resolve(); else server.once('exit', resolve); });
}
