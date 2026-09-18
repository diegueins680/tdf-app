import assert from 'node:assert/strict';
const base = process.env.TDF_CLAIM_TARGET_BASE;
assert.match(base ?? '', /^http:\/\/127\.0\.0\.1:\d+$/);
const ids = JSON.parse(process.env.TDF_CLAIM_TARGET_IDS);
const headers = { Authorization: 'Bearer synthetic-claim-target-token' };
const path = id => `${base}/directory/artist-claim-targets/${id}`;
const fresh = ids['Claim target fresh fixture'];
assert.equal((await fetch(path(fresh), { method: 'PUT' })).status, 401);
const prepared = await Promise.all(Array.from({ length: 8 }, async () => {
  const response = await fetch(path(fresh), { method: 'PUT', headers });
  assert.equal(response.status, 200);
  return response.json();
}));
assert.equal(new Set(prepared.map(value => value.id)).size, 1, 'concurrent requests must reuse one target');
for (const value of prepared) {
  assert.deepEqual(Object.keys(value).sort(), ['id', 'name']);
  assert.equal(value.name, 'Claim target fresh fixture');
}
const draft = await fetch(path(ids['Claim target draft fixture']), { method: 'PUT', headers });
assert.equal(draft.status, 200);
assert.equal((await draft.json()).name, 'Claim target draft fixture', 'private directory name must not leak');
assert.equal((await fetch(path(ids['Claim target blocked fixture']), { method: 'PUT', headers })).status, 404);
assert.equal((await fetch(path(-1), { method: 'PUT', headers })).status, 404);
const claim = async () => {
  const response = await fetch(`${base}/directory/claims`, {
    method: 'POST', headers: { ...headers, 'Content-Type': 'application/json', 'Idempotency-Key': 'claim-target-runtime-retry' },
    body: JSON.stringify({ profileId: prepared[0].id, claimType: 'administration', evidence: [{ description: 'Synthetic verified-review fixture evidence' }] }),
  });
  assert.equal(response.status, 201);
  return response.json();
};
const first = await claim();
assert.equal(first.status, 'submitted');
assert.equal(first.profileId, prepared[0].id);
assert.deepEqual(await claim(), first, 'retry must preserve the original persisted receipt');
console.log('Artist claim targets: authenticated preparation, concurrent reuse, draft privacy, blocked denial, persisted idempotent claim passed.');
