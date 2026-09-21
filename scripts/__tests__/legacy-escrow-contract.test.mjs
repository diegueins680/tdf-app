import assert from 'node:assert/strict';
import test from 'node:test';
import { readFileSync } from 'node:fs';
import { checkDisabledEscrowWrites, disabledEscrowWrites } from '../lib/legacy-escrow-contract.mjs';

const source = readFileSync(new URL('../../tdf-hq/src/TDF/Server.hs', import.meta.url), 'utf8');
const apiSource = readFileSync(new URL('../../tdf-hq/src/TDF/API.hs', import.meta.url), 'utf8');
test('both real legacy escrow write handlers have the reviewed constant rejection bodies', () => {
  assert.equal(checkDisabledEscrowWrites(source, apiSource).obligations.length, 2);
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
      assert.throws(() => checkDisabledEscrowWrites(source.replace(start, replacement), apiSource), /correspondence/);
    }
    assert.throws(() => checkDisabledEscrowWrites(source.replace(`${handler} ::`, `removed${handler} ::`), apiSource), /declaration/);
  });
}

for (const { handler } of disabledEscrowWrites) {
  test(`rejects rerouting ${handler} while retaining its unused safe definition`, () => {
    const rerouted = source.replace(`:<|> ${handler} user`, ':<|> legacyFinancialWriter user');
    assert.notEqual(rerouted, source);
    assert.throws(() => checkDisabledEscrowWrites(rerouted, apiSource), /route composition/);
  });
}
test('rejects replacing either parent server with a legacy composition', () => {
  assert.throws(() => checkDisabledEscrowWrites(source.replace(':<|> serviceMarketplaceServer user', ':<|> legacyMarketplaceServer user'), apiSource), /protected route binding/);
  assert.throws(() => checkDisabledEscrowWrites(source.replace(':<|> protectedServer\n', ':<|> legacyProtectedServer\n'), apiSource), /authenticated root binding/);
});
test('rejects API changes and reordered financial routes before recovery', () => {
  const moved = apiSource.replace(':> "escrow" :> "release"', ':> "escrow" :> "unverified-release"');
  assert.notEqual(moved, apiSource);
  assert.throws(() => checkDisabledEscrowWrites(source, moved), /API ordering/);
  const reordered = apiSource.replace(':> "complete" :> Post', ':> "TEMP" :> Post')
    .replace(':> "escrow" :> "release"', ':> "complete"')
    .replace(':> "TEMP" :> Post', ':> "escrow" :> "release" :> Post');
  assert.throws(() => checkDisabledEscrowWrites(source, reordered), /API ordering/);
  assert.throws(() => checkDisabledEscrowWrites(source, apiSource.replace('AuthProtect "bearer-token" :> ProtectedAPI', 'ProtectedAPI')), /authenticated root binding/);
});
