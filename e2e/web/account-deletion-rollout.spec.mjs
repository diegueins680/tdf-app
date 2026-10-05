import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test.use({ serviceWorkers: 'block' });
test.skip(process.env.PLAYWRIGHT_ACCOUNT_DELETION_FORM_ENABLED !== 'false', 'Exercise the disabled production rollout build separately.');

test('Disabled deletion rollout retains accessible contact without intake requests @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const deletionRequests = [];
  await page.route('**/*', async route => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    if (url.pathname.includes('account-deletion')) deletionRequests.push(request.method() + ' ' + url.pathname);
    if (url.pathname.endsWith('/session')) return route.fulfill({ json: { username: 'synthetic@example.test', partyId: 424242, roles: ['Admin'], modules: [], featureFlags: [] } });
    if (url.pathname.endsWith('/session/onboarding')) return route.fulfill({ json: { eligible: false } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/cuenta/eliminar');
  await expect(page.getByRole('link', { name: 'Solicitar eliminación por correo' })).toHaveAttribute('href', 'mailto:info@tdfrecords.net?subject=TDF%20account%20deletion');
  await expect(page.getByRole('checkbox')).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'Solicitar eliminación de esta cuenta', exact: true })).toHaveCount(0);
  await expect(page.getByText('No necesitas enviar un correo', { exact: false })).toHaveCount(0);
  await page.addScriptTag({ content: axe.source });
  expect(await page.evaluate(async () => (await axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => v.id))).toEqual([]);
  await page.getByRole('button', { name: 'English', exact: true }).click();
  await expect(page.getByRole('link', { name: 'Request deletion by email' })).toHaveAttribute('href', 'mailto:info@tdfrecords.net?subject=TDF%20account%20deletion');
  await expect(page.getByRole('checkbox')).toHaveCount(0);
  expect(deletionRequests).toEqual([]);
});

// This build pauses intake while independently keeping operator support enabled.
test('Pausing intake preserves the operator queue and recorded owner @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const deletionPosts = [];
  await page.route('**/*', async route => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    if (request.method() === 'POST' && url.pathname.includes('account-deletion')) deletionPosts.push(url.pathname);
    if (url.pathname.endsWith('/session')) return route.fulfill({ json: { username: 'synthetic@example.test', partyId: 424242, roles: ['Admin'], modules: ['internships'], featureFlags: [] } });
    if (url.pathname.endsWith('/session/onboarding')) return route.fulfill({ json: { eligible: false } });
    if (url.pathname.endsWith('/feedback/internal/legacy') && url.searchParams.get('accountDeletionOnly') === 'true') return route.fulfill({ json: [{
      lfdId: 'existing-private-request', lfdTitle: 'Existing deletion request', lfdDescription: 'account_deletion_request\nSynthetic record only.',
      lfdCreatedBy: 424242, lfdCreatedAt: '2026-10-05T12:00:00Z', lfdDeletionHistory: [],
    }] });
    return route.fulfill({ json: [] });
  });
  await page.goto('/feedback/interno');
  await expect(page.getByRole('heading', { name: 'Solicitudes de eliminación de cuenta' })).toBeVisible();
  await expect(page.getByText('Existing deletion request', { exact: true })).toBeVisible();
  await expect(page.getByText(/Solicitud existing-private-request · Cuenta autenticada registrada por el servidor: 424242/)).toBeVisible();
  await expect(page.getByLabel('Resultado y verificación del procesamiento')).toBeVisible();
  expect(deletionPosts).toEqual([]);
});
