import test from 'node:test';
import assert from 'node:assert/strict';

// Bounded admission model, not a translation/proof of Haskell or PostgreSQL.
// Enumerate expiry/revocation, lock waits, channel and publication combinations.
const flags = [false, true];
function admits(s, mutant) {
  return (mutant === 'channel' || s.serviceChannel)
    && (mutant === 'revocation' || s.activeAtAdmission)
    && s.clientUnexpired && s.tokenUnexpiredAtAdmission
    && (mutant === 'expiry-after-wait' || s.tokenUnexpiredAtCommit)
    && (!s.publish || s.publicationScope);
}
function violations(mutant) {
  const bad = [];
  for (const serviceChannel of flags) for (const activeAtAdmission of flags)
    for (const clientUnexpired of flags) for (const tokenUnexpiredAtAdmission of flags)
      for (const tokenUnexpiredAtCommit of flags) for (const publish of flags)
        for (const publicationScope of flags) {
          const s = { serviceChannel, activeAtAdmission, clientUnexpired,
            tokenUnexpiredAtAdmission, tokenUnexpiredAtCommit, publish, publicationScope };
          const safe = serviceChannel && activeAtAdmission && clientUnexpired
            && tokenUnexpiredAtAdmission && tokenUnexpiredAtCommit && (!publish || publicationScope);
          if (admits(s, mutant) && !safe) bad.push(s);
        }
  return bad;
}
test('bounded service admission has no unauthorized commits in 128 combinations', () => {
  assert.deepEqual(violations(), []);
});
for (const mutant of ['channel', 'revocation', 'expiry-after-wait']) {
  test(`negative control detects missing ${mutant} guard`, () => {
    assert.ok(violations(mutant).length > 0);
  });
}
