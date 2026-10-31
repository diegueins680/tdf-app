import { test } from 'node:test';
import assert from 'node:assert/strict';
import { withFixture } from '../../e2e/torrent-audio/fixture-server.mjs';
for (const mode of ['success', 'setup-failure', 'async-failure']) {
  test(`owned fixture HTTP listener closes after ${mode}`, { timeout: 5_000 }, async t => {
    let origin; let owned;
    t.after(async () => { if (owned) await owned.close(); });
    const error = new Error('controlled fixture failure');
    const operation = withFixture({}, async fixture => {
      owned = fixture; origin = fixture.origin;
      assert.equal(await (await fetch(origin + '/__liveness', { signal: AbortSignal.timeout(1_000) })).text(), 'unrelated-response-sentinel');
      if (mode === 'setup-failure') throw error;
      if (mode === 'async-failure') { await Promise.resolve(); throw error; }
      return 'completed';
    });
    if (mode === 'success') assert.equal(await operation, 'completed');
    else await assert.rejects(operation, e => e === error);
    await assert.rejects(fetch(origin + '/__liveness', { signal: AbortSignal.timeout(1_000) }));
  });
}
