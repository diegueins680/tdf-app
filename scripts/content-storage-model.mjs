// A finite policy model, not a proof of the SQL/HTTP implementation or Google.
export function verifyContentStorageModel(fault = '') {
  const violations = [];
  let cases = 0;
  const check = (condition, witness) => { cases += 1; if (!condition) violations.push(witness); };
  for (let used = 0; used <= 8; used += 1) for (let reserved = 0; reserved <= 5; reserved += 1) {
    for (let amount = 1; amount <= 5; amount += 1) for (const destination of ['tdf', 'google_drive']) {
      const permitted = destination === 'google_drive' || fault === 'ignore-quota' || used + reserved + amount <= 5;
      check(!permitted || destination !== 'tdf' || used + reserved + amount <= 5,
        { invariant: 'quota', used, reserved, amount, destination });
    }
  }
  for (const state of ['uploading', 'completed', 'cancelled']) for (const sameRequest of [false, true]) {
    const objects = state !== 'cancelled' && sameRequest && fault === 'duplicate-replay' ? 2 : 1;
    check(objects === 1, { invariant: 'stable-identity', state, sameRequest });
  }
  for (const state of ['uploading', 'completed', 'cancelled']) for (const owner of ['same', 'other']) {
    for (const lease of [0, 1]) for (const current of [0, 1]) for (const unexpired of [false, true]) {
      const commits = state === 'uploading' && owner === 'same' && unexpired && (fault === 'stale-lease' || lease === current);
      check(!commits || (state === 'uploading' && owner === 'same' && unexpired && lease === current),
        { invariant: 'lease', state, owner, lease, current, unexpired });
    }
  }
  for (const credential of [false, true]) {
    const destination = credential ? 'user' : fault === 'authority-fallback' ? 'tdf' : 'rejected';
    check(destination !== 'tdf', { invariant: 'chosen-authority', credential, destination });
  }
  return { cases, violations, scope: 'Capacity five units; two leases; three stored states; explicit personal/service authority. Provider IDs, I/O, processes, permissions and liveness need separate qualification.' };
}
