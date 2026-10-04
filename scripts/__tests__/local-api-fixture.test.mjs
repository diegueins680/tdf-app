import assert from 'node:assert/strict';
import { test } from 'node:test';
import { localApiFixturePattern } from '../../e2e/web/helpers/local-api-fixture.mjs';

test('only exact loopback Vite module namespaces bypass the callback', () => {
  for (const origin of ['http://127.0.0.1:4191', 'http://localhost:4173', 'http://[::1]:4191']) {
    const pattern = localApiFixturePattern(origin);
    for (const path of ['/src/main.tsx', '/src/pages/EventTaskPage.tsx?t=123', '/node_modules/.vite/deps/react.js?v=123']) {
      assert.equal(pattern.test(origin + path), false, origin + path);
    }
    for (const path of ['/', '/src', '/src-api/task', '/node_modules-api/task', '/@vite/client',
      '/session', '/session/onboarding/reconcile', '/event-operations/events/80/tasks/8000',
      '/event-operations/events/80/tasks/8000?version=1', '/social-events/80', '/parties/11',
      '/navigation/preferences', '/unknown-api', '/social/eventos/80?tarea=8000']) {
      assert.equal(pattern.test(origin + path), true, origin + path);
    }
  }
});

test('foreign origins and lookalikes are intercepted even for module-shaped paths', () => {
  const pattern = localApiFixturePattern('http://127.0.0.1:4191');
  for (const origin of ['https://example.invalid', 'http://127.0.0.1:4192', 'https://127.0.0.1:4191',
    'http://localhost:4191', 'http://127x0x0x1:4191', 'http://127.0.0.1.evil.invalid:4191',
    'http://127.0.0.1:4191@evil.invalid']) {
    for (const path of ['/session', '/src/main.tsx', '/node_modules/.vite/deps/react.js']) {
      assert.equal(pattern.test(origin + path), true, origin + path);
    }
  }
});

test('missing, remote or ambiguous fixture bases fail closed', () => {
  for (const base of [undefined, '', '/relative', 'https://example.invalid', 'https://localhost:4191',
    'http://127.0.0.1:4191/path', 'http://127.0.0.1:4191?base=1', 'http://127.0.0.1:4191#hash',
    'http://user:password@127.0.0.1:4191']) {
    assert.throws(() => localApiFixturePattern(base));
  }
});
