import assert from 'node:assert/strict';
import yaml from 'yaml';
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
    'recovery',(SELECT jsonb_agg(to_jsonb(c) ORDER BY api_token_id) FROM auth_recovery_challenge c),
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
  const submittedClaim = await claimResponse.json();
  assert.equal(submittedClaim.status, 'submitted');
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
  const disabledReset = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES ('${disabledReset}',${independent.partyId},'password-reset:claimant@example.test',true)`);
  assert.ok(await session(independent.token)); assert.ok(await session(families.google));
  assert.equal((await adminUpdate({ uauActive: false })).status, 200);
  assert.equal(await session(independent.token), null);
  assert.equal(await session(families.google), null);
  assert.equal(sql(`SELECT active FROM api_token WHERE token='${disabledReset}'`), 'f', 'disable revokes recovery challenges');
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

  const bindRecovery = (token, secondsRemaining = 900, boundCredential = credentialId) => {
    assert.ok(Number.isInteger(secondsRemaining) && Math.abs(secondsRemaining) <= 900);
    assert.ok(Number.isSafeInteger(boundCredential) && boundCredential > 0);
    sql(`WITH sampled AS MATERIALIZED (SELECT floor(extract(epoch FROM clock_timestamp()))::bigint AS epoch)
      INSERT INTO auth_recovery_challenge(api_token_id,credential_id,issued_at_epoch,expires_at_epoch)
      SELECT t.id,${boundCredential},sampled.epoch+${secondsRemaining}-900,sampled.epoch+${secondsRemaining}
      FROM api_token t CROSS JOIN sampled WHERE token='${token}';`);
  };
  let winningPassword;
  const resetToken = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${resetToken}',${independent.partyId},'password-reset:claimant@example.test',true);`);
  bindRecovery(resetToken);
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

  const waitBlocked = async count => {
    for (let attempt = 0; attempt < 150; attempt++) {
      const waiting = Number(sql("SELECT count(*) FROM pg_stat_activity WHERE application_name='tdf_identity_http_fixture' AND wait_event_type='Lock'"));
      if (waiting >= count) return;
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    assert.fail(`Expected ${count} blocked lifecycle requests`);
  };
  const lifecycleRace = async (table, condition, first, second) => {
    const lockKey = `identity-http-${randomUUID()}`;
    const holder = spawn('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
    let output = '';
    try {
      await new Promise((resolve, reject) => {
        const timeout = setTimeout(() => reject(new Error('issuance barrier timed out')), 10000);
        holder.stdout.on('data', chunk => {
          output += chunk.toString();
          if (output.includes('issuance-barrier-ready')) { clearTimeout(timeout); resolve(); }
        });
        holder.once('error', error => { clearTimeout(timeout); reject(error); });
        holder.stdin.write(`SELECT pg_advisory_lock(hashtextextended('${lockKey}',0)); SELECT 'issuance-barrier-ready';\n`);
      });
      sql(`CREATE FUNCTION identity_http_hold_lifecycle() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
        IF ${condition} THEN PERFORM pg_advisory_xact_lock(hashtextextended('${lockKey}',0)); END IF; RETURN NEW; END $$;
        CREATE TRIGGER identity_http_hold_lifecycle BEFORE INSERT OR UPDATE ON ${table}
        FOR EACH ROW EXECUTE FUNCTION identity_http_hold_lifecycle();`);
      const firstPending = first(); await waitBlocked(1);
      const secondPending = second(); await waitBlocked(2);
      holder.stdin.end(`SELECT pg_advisory_unlock(hashtextextended('${lockKey}',0));\n`);
      return await Promise.all([firstPending, secondPending]);
    } finally {
      holder.kill('SIGTERM');
      sql(`DROP TRIGGER IF EXISTS identity_http_hold_lifecycle ON ${table}; DROP FUNCTION IF EXISTS identity_http_hold_lifecycle();`);
    }
  };
  const disableFirst = await lifecycleRace('user_credential', `NEW.id=${credentialId} AND NOT NEW.active`,
    () => adminUpdate({ uauActive: false }), () => login('synthetic-changed-password-42'));
  assert.deepEqual(disableFirst.map(response => response.status), [200, 401], 'login must revalidate after disable wins');
  assert.equal((await adminUpdate({ uauActive: true })).status, 200);
  const loginFirst = await lifecycleRace('api_token', `NEW.party_id=${independent.partyId} AND NEW.label LIKE 'password-login:%' AND TG_OP='INSERT'`,
    () => login('synthetic-changed-password-42'), () => adminUpdate({ uauActive: false }));
  assert.deepEqual(loginFirst.map(response => response.status), [200, 200]);
  assert.equal(await session((await loginFirst[0].json()).token), null, 'disable after committed issuance must revoke that session');
  assert.equal((await adminUpdate({ uauActive: true })).status, 200);

  // Database failure after credential mutation must roll back challenge and hash.
  const failureToken = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${failureToken}',${independent.partyId},'password-reset:claimant@example.test',true);
    CREATE FUNCTION identity_http_reject_session() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
      IF NEW.party_id=${independent.partyId} AND NEW.label LIKE 'password-login:%' THEN
        RAISE EXCEPTION 'synthetic session issuance failure'; END IF; RETURN NEW; END $$;
    CREATE TRIGGER identity_http_reject_session BEFORE INSERT ON api_token
      FOR EACH ROW EXECUTE FUNCTION identity_http_reject_session();`);
  bindRecovery(failureToken);
  const beforeFailedIssuance = identitySnapshot();
  try {
    assert.equal((await confirm(failureToken, 'synthetic-must-rollback-42')).status, 500);
    assert.equal(identitySnapshot(), beforeFailedIssuance, 'failed issuance must roll back hash, challenge and revocations');
  } finally {
    sql('DROP TRIGGER identity_http_reject_session ON api_token; DROP FUNCTION identity_http_reject_session();');
  }
  // ID-SESSION-003: legacy, expired, future and wrong-owner challenges fail
  // without consuming the token, changing credentials, or issuing a session.
  for (const scenario of ['legacy', 'expired', 'deadline', 'future', 'wrong-owner']) {
    const value = randomUUID();
    sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
      ('${value}',${independent.partyId},'password-reset:claimant@example.test',true)`);
    if (scenario !== 'legacy') {
      if (scenario === 'wrong-owner') {
        sql(`INSERT INTO user_credential(party_id,username,password_hash,active)
          SELECT ${actor},'identity-wrong-owner',password_hash,true FROM user_credential WHERE id=${credentialId}`);
      }
      bindRecovery(value, scenario === 'expired' ? -1 : scenario === 'deadline' ? 0 : 900,
        scenario === 'wrong-owner' ? Number(sql("SELECT id FROM user_credential WHERE username='identity-wrong-owner'")) : credentialId);
      if (scenario === 'future') sql(`UPDATE auth_recovery_challenge SET issued_at_epoch=issued_at_epoch+900,expires_at_epoch=expires_at_epoch+900 WHERE api_token_id=(SELECT id FROM api_token WHERE token='${value}')`);
    }
    const before = identitySnapshot();
    assert.equal((await confirm(value, 'synthetic-must-not-change-42')).status, 400, scenario);
    assert.equal(identitySnapshot(), before, `${scenario} must have no persisted effects`);
  }
  // Hold the TOKEN row, after credential locking. A transaction-start timestamp
  // or a clock sampled before this wait would incorrectly accept the challenge.
  const expiresWaiting = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${expiresWaiting}',${independent.partyId},'password-reset:claimant@example.test',true)`);
  bindRecovery(expiresWaiting, 5);
  const expiryHolder = spawn('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
  try {
    await new Promise((resolve, reject) => {
      let output = '';
      const timeout = setTimeout(() => reject(new Error('expiry barrier timed out')), 10000);
      expiryHolder.stdout.on('data', chunk => {
        output += chunk.toString();
        if (output.includes('expiry-barrier-ready')) { clearTimeout(timeout); resolve(); }
      });
      expiryHolder.once('error', error => { clearTimeout(timeout); reject(error); });
      expiryHolder.stdin.write(`BEGIN; SELECT id FROM api_token WHERE token='${expiresWaiting}' FOR UPDATE; SELECT 'expiry-barrier-ready';\n`);
    });
    const before = identitySnapshot();
    const pending = confirm(expiresWaiting, 'synthetic-expired-during-wait-42');
    await waitBlocked(1);
    assert.equal(sql(`SELECT expires_at_epoch>floor(extract(epoch FROM clock_timestamp()))::bigint FROM auth_recovery_challenge WHERE api_token_id=(SELECT id FROM api_token WHERE token='${expiresWaiting}')`), 't', 'request must reach barrier before expiry');
    for (let attempt = 0; attempt < 120; attempt++) {
      if (sql(`SELECT expires_at_epoch<=floor(extract(epoch FROM clock_timestamp()))::bigint FROM auth_recovery_challenge WHERE api_token_id=(SELECT id FROM api_token WHERE token='${expiresWaiting}')`) === 't') break;
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    expiryHolder.stdin.end('COMMIT;\n');
    assert.equal((await pending).status, 400, 'expiry must use the post-wait clock');
    assert.equal(identitySnapshot(), before);
  } finally { expiryHolder.kill('SIGTERM'); }
  const reboundToken = randomUUID();
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES
    ('${reboundToken}',${independent.partyId},'password-reset:claimant@example.test',true)`);
  bindRecovery(reboundToken);
  const bindingHolder = spawn('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
  try {
    await new Promise((resolve, reject) => {
      let output = '';
      const timeout = setTimeout(() => reject(new Error('binding barrier timed out')), 10000);
      bindingHolder.stdout.on('data', chunk => {
        output += chunk.toString();
        if (output.includes('binding-barrier-ready')) { clearTimeout(timeout); resolve(); }
      });
      bindingHolder.once('error', error => { clearTimeout(timeout); reject(error); });
      bindingHolder.stdin.write(`BEGIN; SELECT id FROM api_token WHERE token='${reboundToken}' FOR UPDATE; SELECT 'binding-barrier-ready';\n`);
    });
    const pending = confirm(reboundToken, 'synthetic-rebound-must-fail-42');
    await waitBlocked(1);
    sql(`UPDATE auth_recovery_challenge SET credential_id=(SELECT id FROM user_credential WHERE username='identity-http-alternate')
      WHERE api_token_id=(SELECT id FROM api_token WHERE token='${reboundToken}')`);
    const afterFixtureRebind = identitySnapshot();
    bindingHolder.stdin.end('COMMIT;\n');
    assert.equal((await pending).status, 400, 'same-Party rebind during lock wait must not switch to an unlocked credential');
    assert.equal(identitySnapshot(), afterFixtureRebind);
  } finally { bindingHolder.kill('SIGTERM'); }
  sql("UPDATE user_credential SET active=false WHERE username='identity-http-alternate'");
  const requestReset = () => fetch(`${base}/v1/password-reset`, { method: 'POST',
    headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ email: signupBody.email }),
    signal: AbortSignal.timeout(30000),
  });
  sql(`CREATE FUNCTION identity_http_reject_challenge() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
    RAISE EXCEPTION 'synthetic challenge metadata failure'; END $$;
    CREATE TRIGGER identity_http_reject_challenge BEFORE INSERT ON auth_recovery_challenge
    FOR EACH ROW EXECUTE FUNCTION identity_http_reject_challenge();`);
  try {
    const before = identitySnapshot();
    assert.equal((await requestReset()).status, 500);
    assert.equal(identitySnapshot(), before, 'metadata failure rolls back token insertion and prior challenge revocation');
  } finally {
    sql('DROP TRIGGER identity_http_reject_challenge ON auth_recovery_challenge; DROP FUNCTION identity_http_reject_challenge();');
  }
  sql(`UPDATE user_credential SET username='identity-bound-recovery-handle' WHERE id=${credentialId}`);
  assert.equal((await requestReset()).status, 200);
  const issued = JSON.parse(sql(`SELECT jsonb_build_object('token',t.token,'credential',c.credential_id,'duration',c.expires_at_epoch-c.issued_at_epoch)
    FROM api_token t JOIN auth_recovery_challenge c ON c.api_token_id=t.id
    WHERE t.party_id=${independent.partyId} AND t.active AND t.label LIKE 'password-reset:%'`));
  assert.equal(issued.credential, credentialId); assert.equal(issued.duration, 900);
  // A later change in public contact data cannot redirect a bound challenge.
  sql(`UPDATE party SET primary_email='changed-contact@example.test' WHERE id=${independent.partyId}`);
  assert.equal((await confirm(issued.token, 'synthetic-bound-recovery-42')).status, 200);
  const challengedCase = randomUUID();
  const bindingSnapshot = sql(`SELECT to_jsonb(c) FROM auth_recovery_challenge c JOIN api_token t ON t.id=c.api_token_id WHERE t.token='${issued.token}'`);
  sql(`INSERT INTO identity_reconciliation_case(id,member_ids,status,evidence,before_parties,reason,reviewed_by,reviewed_at)
    SELECT '${challengedCase}',ARRAY[${actor},${independent.partyId}],'confirmed',
      jsonb_build_object('basis','verified-source-subject','issuer','synthetic-recovery','scope','synthetic-only','subject','synthetic-challenged-account','evidence_reference','synthetic-recovery-fixture-only','external_reference_review','no-unresolved-references','member_ids',ARRAY[${actor},${independent.partyId}]),
      jsonb_agg(to_jsonb(p) ORDER BY p.id),'Synthetic challenged-account retirement guard',${actor},now()
    FROM party p WHERE id IN (${actor},${independent.partyId})`);
  const challengedPlan = JSON.parse(sql(`SELECT identity_merge_plan('${challengedCase}')`));
  assert.equal(challengedPlan.survivor, actor);
  assert.equal(challengedPlan.can_execute, false);
  assert.ok(challengedPlan.blockers.some(blocker => blocker.party_id === independent.partyId
    && JSON.stringify(blocker.dependencies).includes('user_credential')
    && JSON.stringify(blocker.dependencies).includes('api_token')), 'challenged account retirement must remain blocked by its credential/token references');
  assert.equal(sql(`SELECT to_jsonb(c) FROM auth_recovery_challenge c JOIN api_token t ON t.id=c.api_token_id WHERE t.token='${issued.token}'`), bindingSnapshot);
  console.log('Recovery expiry HTTP: missing metadata, elapsed deadline, future issuance, owner binding and expiry during a token lock wait passed.');
  // ID-CLAIM-REVIEW-001: database-serialized decisions and separated authority.
  sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('Identity HTTP module-only reviewer',false,now());
    INSERT INTO party_security_role(party_id,role_id,approval_mode,active)
      SELECT p.id,r.id,'bootstrap',true FROM party p CROSS JOIN security_role r
      WHERE p.display_name='Identity HTTP module-only reviewer' AND r.code='studio-manager' AND r.active;
    INSERT INTO api_token(token,party_id,label,active)
      SELECT 'synthetic-identity-module-only',id,'Synthetic module-only fixture',true FROM party
      WHERE display_name='Identity HTTP module-only reviewer';
    INSERT INTO party_security_role(party_id,role_id,approval_mode,active)
      SELECT p.id,r.id,'bootstrap',true FROM party p CROSS JOIN security_role r
      WHERE p.display_name='Identity HTTP operator B' AND r.code='artist' AND r.active;`);
  assert.ok((await session('synthetic-identity-module-only')).modules.includes('Admin'), 'negative control really has Admin module');
  assert.equal((await fetch(`${base}/directory/admin/claims`, { headers: headers('module-only') })).status, 403);
  assert.equal((await fetch(`${base}/directory/admin/claims`, { headers: headers('b') })).status, 200,
    'Admin plus Artist must retain the declared directory capability');
  const decide = (id, status, who = 'a') => fetch(`${base}/directory/admin/claims/${id}/status`, {
    method: 'PATCH', headers: { ...headers(who), 'Content-Type': 'application/json' },
    body: JSON.stringify({ status, reason: 'Synthetic independent review' }), signal: AbortSignal.timeout(30000),
  });
  const createReviewClaim = async (who, key) => {
    const response = await fetch(`${base}/directory/claims`, { method: 'POST',
      headers: { ...headers(who), 'Content-Type': 'application/json', 'Idempotency-Key': key },
      body: JSON.stringify({ profileId: target.id, claimType: 'profile', evidence: [{ note: 'Synthetic claim evidence' }] }),
    });
    assert.equal(response.status, 201, await response.clone().text()); return response.json();
  };
  const selfClaim = await createReviewClaim('a', 'identity-http-self-review');
  assert.equal((await decide(selfClaim.id, 'under_review', 'a')).status, 403, 'claimant cannot review their own request');
  assert.equal((await decide(selfClaim.id, 'under_review', 'b')).status, 200, 'independent multi-role Admin can review');
  assert.equal((await decide(submittedClaim.id, 'under_review')).status, 200);
  const decisions = await lifecycleRace('directory_claim', `NEW.id='${submittedClaim.id}' AND NEW.status='approved'`,
    () => decide(submittedClaim.id, 'approved', 'a'), () => decide(submittedClaim.id, 'rejected', 'b'));
  assert.deepEqual(decisions.map(response => response.status), [200, 409], 'conflicting terminal decisions need one winner');
  const approvedReceipt = await decisions[0].json();
  assert.equal(sql(`SELECT c.status||':'||m.active FROM directory_claim c JOIN directory_profile_manager m ON m.source_claim_id=c.id WHERE c.id='${submittedClaim.id}'`), 'approved:true');
  const reviewSnapshot = () => sql(`SELECT to_jsonb(c) FROM directory_claim c WHERE id='${submittedClaim.id}'`);
  const originalReview = reviewSnapshot();
  const reviewAuditCount = sql(`SELECT count(*) FROM directory_audit_event WHERE entity_kind='claim' AND entity_id='${submittedClaim.id}' AND action='claim.reviewed'`);
  assert.equal(reviewAuditCount, '2', 'each actual transition records its review in the transaction');
  sql(`UPDATE directory_profile_manager SET active=false,revoked_at=now(),version=version+1 WHERE source_claim_id='${submittedClaim.id}'`);
  const approvedReplay = await decide(submittedClaim.id, 'approved', 'b');
  assert.equal(approvedReplay.status, 200); assert.deepEqual(await approvedReplay.json(), approvedReceipt);
  assert.equal(reviewSnapshot(), originalReview, 'replay must not replace review evidence');
  assert.equal(sql(`SELECT count(*) FROM directory_audit_event WHERE entity_kind='claim' AND entity_id='${submittedClaim.id}' AND action='claim.reviewed'`), reviewAuditCount, 'replay must not append a fake review');
  assert.equal(sql(`SELECT active FROM directory_profile_manager WHERE source_claim_id='${submittedClaim.id}'`), 'f',
    'approval replay must never reactivate a separately revoked manager');
  const failingClaim = await createReviewClaim('b', 'identity-http-grant-failure');
  assert.equal((await decide(failingClaim.id, 'under_review', 'a')).status, 200);
  const claimBeforeFailure = sql(`SELECT to_jsonb(c) FROM directory_claim c WHERE id='${failingClaim.id}'`);
  sql(`CREATE FUNCTION identity_http_reject_manager() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
    IF NEW.source_claim_id='${failingClaim.id}' THEN RAISE EXCEPTION 'synthetic manager grant failure'; END IF; RETURN NEW; END $$;
    CREATE TRIGGER identity_http_reject_manager BEFORE INSERT OR UPDATE ON directory_profile_manager
      FOR EACH ROW EXECUTE FUNCTION identity_http_reject_manager();`);
  try {
    assert.equal((await decide(failingClaim.id, 'approved', 'a')).status, 500);
    assert.equal(sql(`SELECT to_jsonb(c) FROM directory_claim c WHERE id='${failingClaim.id}'`), claimBeforeFailure,
      'failed grant must roll back the decision');
    assert.equal(sql(`SELECT count(*) FROM directory_audit_event WHERE entity_kind='claim' AND entity_id='${failingClaim.id}' AND new_state='approved'`), '0');
    assert.equal(sql(`SELECT count(*) FROM directory_profile_manager WHERE source_claim_id='${failingClaim.id}'`), '0');
  } finally {
    sql('DROP TRIGGER identity_http_reject_manager ON directory_profile_manager; DROP FUNCTION identity_http_reject_manager();');
  }
  // Read the authoritative graph independently of the runtime helper. Each
  // pair starts from a fresh synthetic row, so negative cases cannot be hidden
  // by an earlier successful transition. SQL fixture setup is not an API grant.
  const claimGraph = yaml.parse(readFileSync(new URL('../../docs/music-directory/formal-model.yaml', import.meta.url), 'utf8')).state_machines.claim.transitions;
  const claimStates = Object.keys(claimGraph);
  assert.equal(claimStates.length, 7, 'review deliberate changes to the claim state domain');
  let checkedClaimPairs = 0;
  for (const from of claimStates) {
    for (const to of [...claimStates, 'unknown_state']) {
      const id = randomUUID();
      // States come from a checked-in contract, nevertheless bind fixture
      // interpolation to the protocol's identifier alphabet.
      assert.match(from, /^[a-z_]+$/); assert.match(to, /^[a-z_]+$/);
      sql(`INSERT INTO directory_claim(id,profile_id,claimant_party_id,claim_type,status,reviewer_party_id,reviewed_at)
        VALUES ('${id}','${target.id}',${independent.partyId},'profile','${from}',${actor},now());`);
      const before = sql(`SELECT to_jsonb(c) FROM directory_claim c WHERE id='${id}'`);
      const managerBefore = sql(`SELECT coalesce(jsonb_agg(to_jsonb(m) ORDER BY profile_id,account_party_id),'[]'::jsonb) FROM directory_profile_manager m WHERE profile_id='${target.id}'`);
      const response = await decide(id, to);
      const allowed = from === to || claimGraph[from].includes(to);
      assert.equal(response.status, allowed ? 200 : 409, `claim graph ${from} -> ${to}: ${await response.clone().text()}`);
      if (!allowed || from === to) {
        assert.equal(sql(`SELECT to_jsonb(c) FROM directory_claim c WHERE id='${id}'`), before,
          'denied or observational transition preserves every persisted field');
        assert.equal(sql(`SELECT coalesce(jsonb_agg(to_jsonb(m) ORDER BY profile_id,account_party_id),'[]'::jsonb) FROM directory_profile_manager m WHERE profile_id='${target.id}'`), managerBefore,
          'denied or observational transition cannot change any manager grant');
        assert.equal(sql(`SELECT count(*) FROM directory_audit_event WHERE entity_kind='claim' AND entity_id='${id}'`), '0');
      } else {
        assert.equal((await response.json()).status, to);
        assert.equal(sql(`SELECT count(*) FROM directory_audit_event WHERE entity_kind='claim' AND entity_id='${id}' AND previous_state='${from}' AND new_state='${to}'`), '1');
      }
      checkedClaimPairs++;
    }
  }
  console.log(`Directory claim graph: ${checkedClaimPairs} real HTTP pairs conform to the declared relation.`);
  console.log('Directory review HTTP: Admin-role enforcement, multi-role composition, separated reviewer, serialized decisions and read-only replay passed.');
  console.log('Credential lifecycle HTTP: disable, re-enable, password replacement, scoped revocation, deterministic reset race and issuance rollback passed.');
  console.log('Identity HTTP: authorization, concurrent replay, actor scope, shared details, archival, canonical access and rollback passed.');
} finally {
  server.kill('SIGTERM');
  await new Promise(resolve => { if (server.exitCode !== null) resolve(); else server.once('exit', resolve); });
}
