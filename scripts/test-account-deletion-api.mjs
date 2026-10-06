import assert from 'node:assert/strict';
import { execFileSync, spawn } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';

/** Called only by the existing disposable, loopback API harness. No real deletion. */
export async function verifyAccountDeletion({ request: rawRequest, requestStatus, admin, account, categoryId, severityId }) {
  const proof = { 'X-Requested-With': 'TDF-Account-Deletion' };
  const request = (path, options = {}) => rawRequest(path, { ...options, headers: { ...proof, ...options.headers } });
  const form = (index = 0) => {
    const body = new FormData();
    body.append('title', `Synthetic account deletion ${index}`);
    body.append('description', `account_deletion_request\nrequested_account_party_id: ${account.partyId}\nSynthetic test only; no fulfilment.`);
    body.append('categoryId', categoryId);
    body.append('severityId', severityId);
    body.append('consent', 'true');
    return body;
  };
  const endpoint = `/feedback/account-deletion?accountId=${account.partyId}`;
  const queue = '/feedback/internal/legacy?accountDeletionOnly=true&offset=0';
  const before = await request(queue, { token: admin.token });
  const cookie = { Cookie: `tdf_session=${account.token}` };
  await rawRequest(endpoint, { headers: { ...cookie, Origin: 'https://attacker.example' }, method: 'POST', body: form(), expected: 403 });
  await rawRequest(endpoint, { headers: cookie, method: 'POST', body: form(), expected: 403 });
  // Servant accepts a trailing slash; route aliases must retain the same CSRF boundary.
  await rawRequest(`/feedback/account-deletion/?accountId=${account.partyId}`, { headers: cookie, method: 'POST', body: form(), expected: 403 });
  await rawRequest(`/feedback/account-deletion/?accountId=${account.partyId}`, { headers: { ...cookie, Origin: 'https://attacker.example' }, method: 'POST', body: form(), expected: 403 });
  await rawRequest(endpoint, { token: account.token, method: 'POST', body: form(), expected: 403 });
  await request(endpoint, { token: account.token, headers: { Origin: 'https://attacker.example' }, method: 'POST', body: form(), expected: 403 });
  const spoof = form();
  spoof.set('description', `account_deletion_request\nrequested_account_party_id: ${admin.partyId}\nSynthetic spoof`);
  await request(endpoint, { token: account.token, method: 'POST', body: spoof, expected: 400 });
  await request('/feedback', { token: account.token, method: 'POST', body: form(), expected: 400 });
  await request('/feedback', { token: account.token, method: 'POST', body: spoof, expected: 400 });
  await request(endpoint, { method: 'POST', body: form(), expected: 401 });
  await request(endpoint, { token: 'expired-synthetic-token', method: 'POST', body: form(), expected: 401 });
  await request(endpoint, { token: admin.token, method: 'POST', body: form(), expected: 403 });
  assert.deepEqual(await request(queue, { token: admin.token }), before, 'Rejected identities must not create requests');
  await request(queue, { token: account.token, expected: 403 });
  // Concurrent first requests, including cookie and bearer transports, share
  // one owner receipt even when responses are ambiguous. They preserve its age.
  const concurrent = await Promise.all(Array.from({ length: 8 }, (_, index) => request(endpoint, {
    ...(index % 2 === 0 ? { headers: { ...cookie, Origin: 'http://localhost:5173' } } : { token: account.token }),
    method: 'POST', body: form(index),
  })));
  assert.ok(concurrent.every(receipt => receipt.adrCreatedBy === account.partyId && receipt.adrRequestId));
  assert.equal(new Set(concurrent.map(receipt => receipt.adrRequestId)).size, 1, 'Concurrent owner requests must return one pending receipt');
  const accepted = [concurrent[0].adrRequestId];
  const received = await request(queue, { token: admin.token });
  const original = received.find(record => record.lfdId === accepted[0]);
  assert.ok(original);
  assert.equal(received.filter(record => record.lfdCreatedBy === account.partyId && record.lfdDeletionHistory.length === 0).length, 1);
  const retry = await request(endpoint, { token: account.token, method: 'POST', body: form(99) });
  assert.equal(retry.adrRequestId, accepted[0]);
  assert.deepEqual(await request(queue, { token: admin.token }), received, 'Retry must preserve receipt, content, original timestamp and history');
  // Exercise pagination with a history of resolved cases, not 21 duplicate
  // pending requests. A new case is allowed only after terminal resolution.
  for (let index = 1; index < 21; index += 1) {
    await request(`/feedback/internal/account-deletion/${accepted.at(-1)}`, { token: admin.token, method: 'POST', json: { adrOutcome: 'rejected', adrNote: 'Synthetic historical case; no erasure.' } });
    const receipt = await request(endpoint, { token: account.token, method: 'POST', body: form(index) });
    assert.equal(receipt.adrCreatedBy, account.partyId);
    assert.ok(receipt.adrRequestId && !accepted.includes(receipt.adrRequestId));
    accepted.push(receipt.adrRequestId);
  }
  // New ordinary feedback must not occupy the privacy queue's first page.
  for (let index = 0; index < 11; index += 1) {
    const ordinary = form(index);
    ordinary.set('description', 'Synthetic ordinary feedback, not a privacy request.');
    await request('/feedback', { token: account.token, method: 'POST', body: ordinary });
  }
  const first = await request(queue, { token: admin.token });
  const second = await request('/feedback/internal/legacy?accountDeletionOnly=true&offset=20', { token: admin.token });
  assert.equal(first.length, 20);
  const visible = [...first, ...second];
  assert.ok(accepted.every(id => visible.some(record => record.lfdId === id && record.lfdCreatedBy === account.partyId)));
  assert.equal(new Set(visible.map(record => record.lfdId)).size, visible.length, 'Page boundaries must not repeat records');
  assert.ok(visible.every(record => record.lfdDescription.startsWith('account_deletion_request\n')));
  await request('/feedback/internal/legacy?accountDeletionOnly=true&offset=-1', { token: admin.token, expected: 400 });
  const resolutionPath = `/feedback/internal/account-deletion/${accepted.at(-1)}`;
  const resolution = { adrOutcome: 'completed', adrNote: 'Synthetic fulfilment verified; no real account erased.' };
  await request(resolutionPath, { method: 'POST', json: resolution, expected: 401 });
  await request(resolutionPath, { token: account.token, method: 'POST', json: resolution, expected: 403 });
  await request(resolutionPath, { token: admin.token, method: 'POST', json: { ...resolution, adrNote: ' ' }, expected: 400 });
  await request(resolutionPath, { token: admin.token, method: 'POST', json: { ...resolution, adrOutcome: 'unknown' }, expected: 400 });
  const receipt = await request(resolutionPath, { token: admin.token, method: 'POST', json: resolution });
  assert.equal(receipt.adaActor, admin.partyId);
  assert.equal(receipt.adaOutcome, 'completed');
  await request(resolutionPath, { token: admin.token, method: 'POST', json: { ...resolution, adrOutcome: 'rejected' }, expected: 409 });
  const refreshed = [...await request(queue, { token: admin.token }), ...await request('/feedback/internal/legacy?accountDeletionOnly=true&offset=20', { token: admin.token })];
  const history = refreshed.find(record => record.lfdId === accepted.at(-1)).lfdDeletionHistory;
  assert.equal(history.length, 1);
  assert.equal(history[0].adaNote, resolution.adrNote);
  assert.equal(history[0].adaActor, admin.partyId);
  const newRejected = await request(endpoint, { token: account.token, method: 'POST', body: form(21) });
  assert.ok(!accepted.includes(newRejected.adrRequestId));
  const rejected = await request(`/feedback/internal/account-deletion/${newRejected.adrRequestId}`, { token: admin.token, method: 'POST', json: { adrOutcome: 'rejected', adrNote: 'Synthetic invalid ownership evidence.' } });
  assert.equal(rejected.adaOutcome, 'rejected');
  const newRaced = await request(endpoint, { token: account.token, method: 'POST', body: form(22) });
  assert.notEqual(newRaced.adrRequestId, newRejected.adrRequestId);
  const racePath = `/feedback/internal/account-deletion/${newRaced.adrRequestId}`;
  const raced = await Promise.all([
    requestStatus(racePath, { token: admin.token, method: 'POST', json: resolution }),
    requestStatus(racePath, { token: admin.token, method: 'POST', json: { ...resolution, adrOutcome: 'rejected' } }),
  ]);
  assert.deepEqual(raced.sort(), [200, 409], 'Concurrent operators may append exactly one terminal outcome');

  // Old generic feedback could carry missing, duplicated or foreign owner
  // claims. More than one page of such rows must neither poison acceptance nor
  // hide an existing valid receipt on a later page.
  const database = process.env.TDF_AUDIT_E2E_DATABASE ?? '';
  assert.match(database, /^[a-z0-9_]+$/, 'Only the runner-created database is allowed');
  assert.match(newRaced.adrRequestId, /^[0-9a-f-]{36}$/i);
  assert.ok(Number.isSafeInteger(account.partyId) && account.partyId > 0);
  assert.ok(Number.isSafeInteger(admin.partyId) && admin.partyId > 0);
  const sql = statement => execFileSync('psql', ['-X', '-A', '-t', '-v', 'ON_ERROR_STOP=1', '-d', database, '-c', statement], { encoding: 'utf8', timeout: 10000 }).trim();
  sql(`INSERT INTO feedback (id,title,description,category_id,severity_id,consent,created_by,created_at)
    SELECT gen_random_uuid(), 'Synthetic invalid legacy',
      E'account_deletion_request\\n' || CASE n % 3
        WHEN 0 THEN 'requested_account_party_id: ${admin.partyId}'
        WHEN 1 THEN 'Missing owner claim'
        ELSE E'requested_account_party_id: ${account.partyId}\\nrequested_account_party_id: ${account.partyId}' END,
      category_id,severity_id,consent,created_by,created_at - INTERVAL '1 day'
    FROM feedback CROSS JOIN generate_series(1,105) n WHERE id='${newRaced.adrRequestId}'`);
  const invalidIds = sql("SELECT id FROM feedback WHERE title='Synthetic invalid legacy'").split('\n');
  assert.equal(invalidIds.length, 105);
  const pending = await request(endpoint, { token: account.token, method: 'POST', body: form(23) });
  assert.ok(!invalidIds.includes(pending.adrRequestId), 'Invalid legacy owner claims must not become accepted deletion receipts');
  assert.match(pending.adrRequestId, /^[0-9a-f-]{36}$/i);
  const legacyRetries = await Promise.all(Array.from({ length: 8 }, () => request(endpoint, { token: account.token, method: 'POST', body: form(23) })));
  assert.ok(legacyRetries.every(value => value.adrRequestId === pending.adrRequestId), 'Reuse the valid pending receipt beyond the first page of invalid legacy rows');
  assert.equal(sql("SELECT count(*) FROM feedback WHERE title='Synthetic invalid legacy'"), '105', 'Preserve legacy rows for explicit operator reconciliation');
  console.log('Account deletion ignores 105 invalid legacy owner claims and reuses the valid receipt across pages.');

  // Hold the row independently and observe real resolution blocked on it.
  // Intake must then wait for the shared owner mutex and receive a fresh case.
  const gate = spawn('psql', ['-X', '-A', '-t', '-v', 'ON_ERROR_STOP=1', '-d', database], { stdio: ['pipe', 'pipe', 'pipe'] });
  let output = '';
  let errorOutput = '';
  gate.stdout.on('data', data => { output += data; });
  gate.stderr.on('data', data => { errorOutput += data; });
  const closed = new Promise((resolve, reject) => {
    gate.once('error', reject);
    gate.once('close', code => code === 0 ? resolve() : reject(new Error(`Row gate exited ${code}: ${errorOutput}`)));
  });
  // Attach a rejection handler immediately; finally still awaits the original.
  void closed.catch(() => {});
  const until = async predicate => {
    const deadline = Date.now() + 20000;
    while (!predicate()) {
      assert.ok(Date.now() < deadline, 'Timed out observing PostgreSQL lock barrier');
      await delay(25);
    }
  };
  let resolving;
  let intake;
  let intakeSettled = false;
  try {
    gate.stdin.write(`BEGIN; SELECT id FROM feedback WHERE id = '${pending.adrRequestId}' FOR UPDATE; SELECT 'ROW_LOCK_READY';\n`);
    await until(() => output.includes('ROW_LOCK_READY'));
    resolving = request(`/feedback/internal/account-deletion/${pending.adrRequestId}`, { token: admin.token, method: 'POST', json: { ...resolution, adrOutcome: 'rejected' } });
    void resolving.catch(() => {});
    await until(() => sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE '%FROM feedback WHERE id =%FOR UPDATE%'") === '1');
    intake = request(endpoint, { token: account.token, method: 'POST', body: form(24) });
    void intake.then(() => { intakeSettled = true; }, () => { intakeSettled = true; });
    await until(() => {
      assert.equal(intakeSettled, false, 'Intake must not acknowledge the receipt while its resolution is blocked');
      return sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE '%pg_advisory_xact_lock%'") === '1';
    });
    gate.stdin.end('COMMIT;\n');
    await closed;
    assert.equal((await resolving).adaOutcome, 'rejected');
    const reopened = await intake;
    assert.notEqual(reopened.adrRequestId, pending.adrRequestId, 'Intake after resolution must create a new pending receipt');
    const latest = await request(queue, { token: admin.token });
    assert.equal(latest.find(row => row.lfdId === pending.adrRequestId).lfdDeletionHistory.length, 1);
    assert.deepEqual(latest.find(row => row.lfdId === reopened.adrRequestId).lfdDeletionHistory, []);
    assert.equal(latest.filter(row => row.lfdCreatedBy === account.partyId && row.lfdDeletionHistory.length === 0).length, 1);
  } finally {
    if (!gate.stdin.writableEnded) gate.stdin.end('ROLLBACK;\n');
    await closed;
    await Promise.allSettled([resolving, intake].filter(Boolean));
  }
  console.log('Account deletion resolution/intake PostgreSQL lock ordering passed.');

  // Pre-upgrade multipart bodies could persist CRLF (or bare CR). Exercise
  // all three real handlers against the original bytes, not just the validator.
  for (const [name, sqlEnding, marker] of [['CRLF', 'chr(13)||chr(10)', 'account_deletion_request\r\n'], ['CR', 'chr(13)', 'account_deletion_request\r']]) {
    const legacy = await request(endpoint, { token: account.token, method: 'POST', body: form(25) });
    assert.match(legacy.adrRequestId, /^[0-9a-f-]{36}$/i);
    sql(`UPDATE feedback SET description=replace(description,chr(10),${sqlEnding}) WHERE id='${legacy.adrRequestId}'`);
    const before = await request(queue, { token: admin.token });
    const row = before.find(value => value.lfdId === legacy.adrRequestId);
    assert.ok(row, `${name} legacy deletion must remain visible in the operator queue`);
    assert.ok(row.lfdDescription.startsWith(marker), 'Reading legacy feedback must preserve its original recorded bytes');
    const retries = await Promise.all(Array.from({ length: 8 }, () => request(endpoint, { token: account.token, method: 'POST', body: form(26) })));
    assert.ok(retries.every(value => value.adrRequestId === legacy.adrRequestId), `${name} legacy deletion must be reused without a duplicate receipt`);
    const path = `/feedback/internal/account-deletion/${legacy.adrRequestId}`;
    const outcome = await request(path, { token: admin.token, method: 'POST', json: resolution });
    assert.equal(outcome.adaOutcome, 'completed', `${name} owner-bound legacy deletion must resolve`);
    await request(path, { token: admin.token, method: 'POST', json: resolution, expected: 409 });
    const after = (await request(queue, { token: admin.token })).find(value => value.lfdId === legacy.adrRequestId);
    assert.equal(after.lfdDeletionHistory.length, 1);
    assert.equal(after.lfdDescription, row.lfdDescription);
    assert.equal(after.lfdCreatedAt, row.lfdCreatedAt);
  }
  console.log('Legacy CRLF and CR deletion requests remain visible, reusable and immutably resolvable.');

  // Authentication may have captured grants before revocation commits. Hold
  // only the token row, observe the handler's real lock wait, commit a role or
  // permission revocation independently, then let authorization continue.
  const operatorIds = sql(`SELECT string_agg(quote_literal(psr.id::text)||'::uuid',',')
    FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id
    WHERE psr.party_id=${admin.partyId} AND psr.active AND r.code IN ('admin','manager','studio-manager')`);
  const permissionIds = sql(`SELECT string_agg(DISTINCT quote_literal(rp.id::text)||'::uuid',',')
    FROM party_security_role psr JOIN role_permission rp ON rp.role_id=psr.role_id
    JOIN security_permission p ON p.id=rp.permission_id JOIN security_module m ON m.id=p.module_id
    JOIN security_action a ON a.id=p.action_id
    WHERE psr.party_id=${admin.partyId} AND psr.active AND rp.active
      AND m.code='internships' AND a.code='access' AND p.resource_scope='module'`);
  assert.ok(operatorIds && permissionIds, 'Fixture must have actual operator and module grants');
  for (const kind of ['operator-role', 'module-permission']) {
    let addedIntern = '';
    try {
      if (kind === 'operator-role') {
        addedIntern = sql(`INSERT INTO party_security_role(party_id,role_id,approval_mode,active)
          SELECT ${admin.partyId},r.id,'bootstrap',true FROM security_role r
          WHERE r.code='intern' AND NOT EXISTS (SELECT 1 FROM party_security_role psr WHERE psr.party_id=${admin.partyId} AND psr.role_id=r.id)
          RETURNING id`).split('\n').filter(value => /^[0-9a-f-]{36}$/i.test(value)).join('');
      }
      const table = kind === 'operator-role' ? 'party_security_role' : 'role_permission';
      const ids = kind === 'operator-role' ? operatorIds : permissionIds;
      const pending = await request(endpoint, { token: account.token, method: 'POST', body: form(28) });
      const paths = [
        [`/feedback/internal/account-deletion/${pending.adrRequestId}`, { method: 'POST', json: resolution }],
        [queue, {}], ['/feedback/internal/legacy', {}],
      ];
      for (const [path, options] of paths) {
        const gate = spawn('psql', ['-X', '-A', '-t', '-v', 'ON_ERROR_STOP=1', '-d', database], { stdio: ['pipe', 'pipe', 'pipe'] });
        let output = ''; let errors = ''; let response; let settled = false;
        gate.stdout.on('data', chunk => { output += chunk; });
        gate.stderr.on('data', chunk => { errors += chunk; });
        const closed = new Promise((resolve, reject) => {
          gate.once('error', reject);
          gate.once('close', code => code === 0 ? resolve() : reject(new Error(`Authority gate failed: ${errors}`)));
        });
        void closed.catch(() => {});
        try {
          gate.stdin.write(`BEGIN; SELECT id FROM api_token WHERE party_id=${admin.partyId} FOR UPDATE; SELECT 'AUTHORITY_LOCK_READY';\n`);
          await until(() => output.includes('AUTHORITY_LOCK_READY'));
          response = requestStatus(path, { token: admin.token, ...options });
          void response.then(() => { settled = true; }, () => { settled = true; });
          await until(() => {
            assert.equal(settled, false, 'Privacy effect must wait for current session/authority admission');
            return sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE '%FROM api_token WHERE id=%FOR SHARE%'") === '1';
          });
          sql(`UPDATE ${table} SET active=false WHERE id IN (${ids})`);
          if (kind === 'operator-role') {
            assert.equal(sql(`SELECT EXISTS(SELECT 1 FROM party_security_role psr
              JOIN security_role r ON r.id=psr.role_id JOIN role_permission rp ON rp.role_id=r.id
              JOIN security_permission p ON p.id=rp.permission_id JOIN security_module m ON m.id=p.module_id
              JOIN security_action a ON a.id=p.action_id WHERE psr.party_id=${admin.partyId}
              AND psr.active AND r.active AND rp.active AND p.active AND m.active AND a.active
              AND r.code='intern' AND m.code='internships' AND a.code='access' AND p.resource_scope='module')`),
            't', 'Intern and internships must remain active after operator revocation');
          }
          gate.stdin.end('COMMIT;\n'); await closed;
          assert.equal(await response, 403, `Committed ${kind} revocation must deny ${path}`);
          assert.equal(sql(`SELECT count(*) FROM audit_log WHERE entity='account_deletion_request' AND entity_id='${pending.adrRequestId}'`), '0', 'Denied resolution must append no terminal evidence');
        } finally {
          if (!gate.stdin.writableEnded) gate.stdin.end('ROLLBACK;\n');
          await closed; await Promise.allSettled([response].filter(Boolean));
          sql(`UPDATE ${table} SET active=true WHERE id IN (${ids})`);
        }
      }
    } finally {
      if (addedIntern) sql(`UPDATE party_security_role SET active=false WHERE id='${addedIntern}'`);
    }
  }
  console.log('Deletion authority: six witnessed session-lock races reject committed role/module revocation; retained Intern access cannot replace operator authority.');



}
