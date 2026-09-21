import assert from 'node:assert/strict';
import test from 'node:test';
import { readFileSync } from 'node:fs';
import { checkDisabledEscrowWrites, disabledEscrowWrites } from '../lib/legacy-escrow-contract.mjs';

const source = readFileSync(new URL('../../tdf-hq/src/TDF/Server.hs', import.meta.url), 'utf8');
test('both real legacy escrow write handlers have the reviewed constant rejection bodies', () => {
  assert.equal(checkDisabledEscrowWrites(source).obligations.length, 2);
});
for (const { id, handler } of disabledEscrowWrites) {
  test(`${id} rejects representative persistence, environment, success and override regressions`, () => {
    const start = `${handler} _ _ =\n  throwError err503`;
    for (const replacement of [
      `${handler} _ _ = do\n  pool <- asks envPool\n  throwError err503`,
      `${handler} _ _ = do\n  liftIO (putStrLn "effect")\n  throwError err503`,
      `${handler} _ _ =\n  pure`,
      `${handler} user request =\n  if hasRole Admin user then oldWrite request else throwError err503`,
    ]) {
      assert.notEqual(source.replace(start, replacement), source, 'mutation must reach its target');
      assert.throws(() => checkDisabledEscrowWrites(source.replace(start, replacement)), /correspondence/);
    }
    assert.throws(() => checkDisabledEscrowWrites(source.replace(`${handler} ::`, `removed${handler} ::`)), /declaration/);
  });
}
