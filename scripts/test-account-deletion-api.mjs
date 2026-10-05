import assert from 'node:assert/strict';

/** Called only by the existing disposable, loopback API harness. No real deletion. */
export async function verifyAccountDeletion({ request, admin, account, categoryId, severityId }) {
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
  await request(endpoint, { method: 'POST', body: form(), expected: 401 });
  await request(endpoint, { token: 'expired-synthetic-token', method: 'POST', body: form(), expected: 401 });
  await request(endpoint, { token: admin.token, method: 'POST', body: form(), expected: 403 });
  assert.deepEqual(await request(queue, { token: admin.token }), before, 'Rejected identities must not create requests');
  await request(queue, { token: account.token, expected: 403 });
  const accepted = [];
  for (let index = 0; index < 21; index += 1) {
    const receipt = await request(endpoint, { token: account.token, method: 'POST', body: form(index) });
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
}
