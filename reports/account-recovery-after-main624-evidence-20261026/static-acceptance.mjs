import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { writeFileSync } from 'node:fs';
import { chromium, devices, expect } from '/private/tmp/tdf-account-recovery-main620-20261026/node_modules/@playwright/test/index.mjs';

const [base, source, hash, output] = process.argv.slice(2);
assert.ok(['https://www.tdfrecords.net', 'http://127.0.0.1:4297'].includes(base));
assert.match(source, /^[a-f0-9]{40}$/); assert.match(hash, /^[a-f0-9]{64}$/);
assert.ok(output.startsWith('/private/tmp/tdf-account-recovery-'));
const copy = {
  es: { password: 'Nueva contraseña', confirm: 'Confirmar contraseña', save: 'Guardar contraseña',
    error: 'Este enlace no es válido o ya venció. Solicita uno nuevo para continuar.',
    link: 'Solicitar nuevo enlace', send: 'Enviar enlace',
    sent: 'Si el correo existe en TDF Records, te enviaremos un enlace para restablecer la contraseña.' },
  en: { password: 'New password', confirm: 'Confirm password', save: 'Save password',
    error: 'This link is invalid or has expired. Request a new link to continue.',
    link: 'Request a new link', send: 'Send link',
    sent: 'If this email is registered with TDF Records, we will send a link to reset your password.' },
};
const intent = '/mis-clasificados?st001=static-verification';
const result = { source, hash, base, startedAt: new Date().toISOString(), status: 'RUNNING', cases: [],
  productionBusinessWrites: 0, actualBusinessRequestsForwarded: 0, humans: 0,
  scope: 'Hash-bound actual optimized static client. Every API, session, reset and email-request response is synthetic and intercepted; all external traffic blocked. No production account, token, password, email, API persistence or cookie revocation claim.' };
const browser = await chromium.launch();
try {
  for (const language of ['es', 'en']) for (const mobile of [false, true]) for (const status of [400, 401]) {
    const c = await browser.newContext({ ...(mobile ? devices['Pixel 7'] : {}), locale: language,
      serviceWorkers: 'block' });
    const row = { language, formFactor: mobile ? 'Pixel7-emulated-web' : 'desktop-web', status,
      confirms: 0, requests: 0, blockedExternal: 0, unexpectedMutations: 0, pageErrors: [] };
    result.cases.push(row);
    await c.route('**/*', async (route) => {
      const r = route.request(), u = new URL(r.url()), method = r.method();
      if (u.origin === 'https://api.tdfrecords.net'
        || (u.origin === base && ['fetch', 'xhr'].includes(r.resourceType()) && !u.pathname.startsWith('/assets/'))) {
        const headers = { 'Access-Control-Allow-Origin': base, 'Access-Control-Allow-Credentials': 'true' };
        const json = (body, code = 200) => route.fulfill({ status: code, headers, contentType: 'application/json', body: JSON.stringify(body) });
        if (method === 'OPTIONS') return route.fulfill({ status: 204, headers });
        if (u.pathname === '/session' && method === 'GET') return json(null, 401);
        if (u.pathname === '/health' && method === 'GET') return json({ status: 'ok', db: 'ok' });
        if (u.pathname === '/v1/password-reset/confirm' && method === 'POST') {
          row.confirms++;
          assert.equal(r.postDataJSON().token, 'synthetic-expired-reset');
          return route.fulfill({ status, headers, contentType: 'text/plain', body: 'Invalid or expired token' });
        }
        if (u.pathname === '/v1/password-reset' && method === 'POST') {
          row.requests++; assert.equal(r.postDataJSON().email, 'static-reset@persona.test');
          assert.equal(u.searchParams.get('redirect'), intent);
          assert.equal(u.searchParams.get('locale'), language);
          row.requestPayload = r.postDataJSON(); return json({ ok: true });
        }
        if (u.pathname === '/radio/presence' && method === 'DELETE') return route.fulfill({ status: 204, headers });
        if (!['GET', 'HEAD'].includes(method)) { row.unexpectedMutations++; return route.abort('blockedbyclient'); }
        return json({ error: 'Synthetic endpoint outside this scoped acceptance' }, 404);
      }
      if (u.origin === base && method === 'GET'
        && (!['fetch', 'xhr'].includes(r.resourceType()) || u.pathname.startsWith('/assets/'))) return route.continue();
      row.blockedExternal++; return route.abort('blockedbyclient');
    });
    const p = await c.newPage(); p.on('pageerror', error => row.pageErrors.push(error.message));
    await p.goto(`${base}/reset?token=synthetic-expired-reset&lang=${language}&redirect=${encodeURIComponent(intent)}`, { waitUntil: 'domcontentloaded' });
    const script = await p.locator('script[type="module"][src]').first().getAttribute('src');
    const entry = new URL(script, base).href; assert.ok(entry.startsWith(`${base}/assets/index-`));
    const content = await p.evaluate(async url => { const r = await fetch(url, { cache: 'no-store' });
      if (!r.ok) throw Error('Static entry unavailable'); return r.text(); }, entry);
    assert.equal(createHash('sha256').update(content).digest('hex'), hash); assert.ok(content.includes(source));
    row.entry = entry;
    const text = copy[language];
    await expect(p.getByLabel(text.password, { exact: true })).toBeVisible();
    assert.equal(new URL(p.url()).searchParams.has('token'), false);
    await p.getByLabel(text.password, { exact: true }).fill('Synthetic-Reset!42');
    await p.getByLabel(text.confirm, { exact: true }).fill('Synthetic-Reset!42');
    await p.getByRole('button', { name: text.save, exact: true }).click();
    await expect(p.getByRole('alert').filter({ hasText: text.error })).toHaveText(text.error);
    assert.equal(row.confirms, 1);
    await p.getByRole('link', { name: text.link, exact: true }).click();
    await expect(p.getByRole('dialog')).toBeVisible();
    assert.equal(new URL(p.url()).searchParams.get('redirect'), intent);
    await p.getByRole('dialog').locator('input[type="email"]').fill('static-reset@persona.test');
    await p.getByRole('dialog').getByRole('button', { name: text.send, exact: true }).click();
    await expect(p.getByRole('dialog').getByText(text.sent, { exact: true })).toBeVisible();
    assert.equal(row.requests, 1); assert.equal(row.unexpectedMutations, 0); assert.deepEqual(row.pageErrors, []);
    row.outcome = 'PASS_LOCALIZED_EXPIRY_AND_NEW_LINK_INTENT';
    await c.close();
  }
  result.status = 'PASS_STATIC_CLIENT_SYNTHETIC_BUSINESS';
} catch (error) { result.status = 'FAILED'; result.error = String(error); throw error; }
finally {
  await browser.close(); result.browserClosed = true; result.finishedAt = new Date().toISOString();
  writeFileSync(output, JSON.stringify(result, null, 2) + '\n', { mode: 0o600 });
  console.log(JSON.stringify({ status: result.status, cases: result.cases.length, error: result.error }));
}
