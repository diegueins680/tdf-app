import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test('Live Sessions use the verified contact, preserve edits and isolate another account @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin;
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      writes.push({ method: request.method(), path });
      return route.fulfill({ status: 409, json: { error: 'No submission or contact writes allowed' } });
    }
    if (path === '/session') {
      const authorization = request.headers().authorization;
      if (authorization === 'Bearer synthetic-verified-code') return route.fulfill({ json: { partyId: 42, username: 'verified@example.test' } });
      if (authorization === 'Bearer synthetic-other-code') return route.fulfill({ json: { partyId: 99, username: 'a-handle' } });
      if (authorization === 'Bearer synthetic-invalid-code') return route.fulfill({ status: 401, json: {} });
      // The ambient staff account must never become this public form's contact.
      return route.fulfill({ json: { partyId: 101, username: 'staff@example.test', displayName: 'Cuenta sintética', roles: ['Admin'], modules: [] } });
    }
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/live-sessions/registro');
  const code = page.getByRole('textbox', { name: 'Código de acceso', exact: true });
  // Password inputs have no textbox role until the existing reveal control is used.
  await page.getByRole('button', { name: 'Mostrar código' }).click();
  const email = page.getByRole('textbox', { name: 'Email de contacto', exact: true });
  await expect(email).toBeDisabled(); await expect(email).toHaveValue('');
  await code.fill('synthetic-invalid-code');
  await expect(code).toHaveAttribute('aria-invalid', 'true');
  await expect(email).toBeDisabled(); await expect(email).toHaveValue('');
  await code.fill('synthetic-verified-code');
  await expect(email).toBeEnabled(); await expect(email).toHaveValue('verified@example.test');
  await expect(page.getByRole('textbox', { name: 'Nombre completo', exact: true })).toHaveValue('');
  await expect(page.getByRole('checkbox', { name: /Acepto los términos/ })).not.toBeChecked();
  await email.fill('edited@example.test');
  await expect.poll(() => page.evaluate(() => JSON.parse(localStorage.getItem('live-session-draft:public:42') ?? '{}').contactEmail)).toBe('edited@example.test');
  await page.reload();
  await page.getByRole('button', { name: 'Mostrar código' }).click();
  await expect(email).toHaveValue(''); await expect(email).toBeDisabled();
  await code.fill('synthetic-verified-code');
  await expect(email).toBeEnabled(); await expect(email).toHaveValue('edited@example.test');
  await email.fill('');
  await code.fill('synthetic-other-code');
  await expect(email).toBeEnabled(); await expect(email).toHaveValue('');
  await code.fill('synthetic-verified-code');
  await expect(email).toBeEnabled(); await expect(email).toHaveValue('');
  await expect(page.getByRole('checkbox', { name: /Acepto los términos/ })).not.toBeChecked();
  await email.fill('editable@example.test');
  // Focus and nested scrolling can settle on different animation frames.
  // Require the whole field and its real pointer target before capturing it.
  await expect(async () => {
    await email.evaluate(element => element.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(email).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await email.evaluate(element => {
      const rect = element.getBoundingClientRect();
      return document.elementFromPoint(rect.x + rect.width / 2, rect.y + rect.height / 2) === element;
    })).toBe(true);
  }).toPass({ timeout: 8000 });
  await page.addScriptTag({ content: axe.source });
  const violations = await email.evaluate(async element => (await globalThis.axe.run(element.closest('.MuiFormControl-root'), {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, impact: v.impact })));
  expect(violations).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  expect(writes).toEqual([]);
  await page.screenshot({ path: info.outputPath('live-session-editable-contact.png') });
});
