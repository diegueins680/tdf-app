import assert from 'node:assert/strict';

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
  const accepted = [];
  for (let index = 0; index < 21; index += 1) {
    const receipt = await request(endpoint, { ...(index === 0 ? { headers: { ...cookie, Origin: 'http://localhost:5173' } } : { token: account.token }), method: 'POST', body: form(index) });
    assert.equal(receipt.adrCreatedBy, account.partyId);
    assert.ok(receipt.adrRequestId);
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
  const resolutionPath = `/feedback/internal/account-deletion/${accepted[0]}`;
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
  const history = refreshed.find(record => record.lfdId === accepted[0]).lfdDeletionHistory;
  assert.equal(history.length, 1);
  assert.equal(history[0].adaNote, resolution.adrNote);
  assert.equal(history[0].adaActor, admin.partyId);
  const rejected = await request(`/feedback/internal/account-deletion/${accepted[1]}`, { token: admin.token, method: 'POST', json: { adrOutcome: 'rejected', adrNote: 'Synthetic invalid ownership evidence.' } });
  assert.equal(rejected.adaOutcome, 'rejected');
  const racePath = `/feedback/internal/account-deletion/${accepted[2]}`;
  const raced = await Promise.all([
    requestStatus(racePath, { token: admin.token, method: 'POST', json: resolution }),
    requestStatus(racePath, { token: admin.token, method: 'POST', json: { ...resolution, adrOutcome: 'rejected' } }),
  ]);
  assert.deepEqual(raced.sort(), [200, 409], 'Concurrent operators may append exactly one terminal outcome');


}
