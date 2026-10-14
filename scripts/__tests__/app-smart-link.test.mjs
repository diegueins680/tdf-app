import assert from 'node:assert/strict';
import test from 'node:test';
import { APP_LANDING_PATH, redirectToAppLanding, smartLinkTarget } from '../../functions/_shared/app-smart-link.mjs';
import { onRequest as descargar } from '../../functions/descargar.js';
import { onRequest as download } from '../../functions/download.js';

test('aliases redirect to /app on the same origin and keep only campaign labels', () => {
  const target = smartLinkTarget('https://www.tdfrecords.net/descargar?utm_source=instagram&utm_campaign=tu_escena&surface=campaign&token=secret&email=a%40b.c', 'https://www.tdfrecords.net');
  assert.equal(target, 'https://www.tdfrecords.net/app?utm_source=instagram&utm_campaign=tu_escena&surface=campaign');
});

test('drops labels that are not plain campaign tags', () => {
  const target = smartLinkTarget('https://www.tdfrecords.net/download?utm_source=person%40example.com&utm_medium=whats%20app&feedback=1', 'https://www.tdfrecords.net');
  assert.equal(target, 'https://www.tdfrecords.net/app?feedback=1');
});

test('both aliases answer with an uncached temporary redirect on the request origin', () => {
  for (const handler of [descargar, download]) {
    const response = handler({ request: new Request('https://preview.tdf-app.pages.dev/descargar?utm_source=qr') });
    assert.equal(response.status, 302);
    assert.equal(response.headers.get('Location'), `https://preview.tdf-app.pages.dev${APP_LANDING_PATH}?utm_source=qr`);
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
  }
});

test('a bare alias lands on the plain canonical page', () => {
  const response = redirectToAppLanding({ request: new Request('https://www.tdfrecords.net/descargar') });
  assert.equal(response.headers.get('Location'), 'https://www.tdfrecords.net/app');
});
