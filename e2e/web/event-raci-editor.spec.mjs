import { expect, test } from '@playwright/test';
import axe from 'axe-core';
import { localApiFixturePattern } from './helpers/local-api-fixture.mjs';

// Synthetic browser/API boundary only; never forward unrecognized API or remote traffic.
for (const theme of ['dark', 'light']) {
test(`RACI ${theme} review, keyboard confirmation and exact uncertain retry @critical`, async ({ page, baseURL }, testInfo) => {
  const origin = new URL(baseURL).origin;
  const writes = []; const unexpected = [];
  const task = { eventId: 80, activityId: 8000, status: 'planned', version: 1,
    policy: { requiresAccountability: true, dependenciesGateCompletion: true, version: 1 },
    raci: [{ partyId: 11, role: 'accountable' }, { partyId: 12, role: 'responsible' }],
    accountabilityNeedsAttention: false };
  await page.addInitScript(selectedTheme => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    localStorage.setItem('tdf-hq-ui/theme-mode', selectedTheme);
  }, theme);
  await page.route(localApiFixturePattern(baseURL), async route => {
    const request = route.request(); const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    if (url.pathname === '/session') return route.fulfill({ json: {
      username: 'synthetic-raci-manager', displayName: 'Gestor de prueba', partyId: 42,
      roles: ['customer'], modules: [], featureFlags: [],
      preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
    } });
    if (url.pathname === '/event-operations/events/80/tasks/8000') return route.fulfill({ json: task });
    if (url.pathname === '/event-operations/events/80/tasks/8000/raci/context') {
      expect(request.method()).toBe('GET');
      return route.fulfill({ json: { eventId: 80, activityId: 8000, aggregateRevision: '4',
        canManage: true, operationReady: true, replaceableAssignments: task.raci, eligiblePartyIds: [11, 12, 13] } });
    }
    if (url.pathname === '/event-operations/events/80/tasks/8000/raci/reassign') {
      expect(request.method()).toBe('POST');
      const body = request.postDataJSON(); const key = request.headers()['idempotency-key'];
      writes.push({ body, key });
      if (writes.length === 1) return route.abort('failed');
      expect(writes[1]).toEqual(writes[0]);
      return route.fulfill({ json: { eventId: 80, activityId: 8000, commandId: key,
        role: body.role, fromPartyId: body.fromPartyId, toPartyId: body.toPartyId,
        aggregateRevision: '6', replayed: true } });
    }
    if (url.pathname.startsWith('/parties') || url.pathname.startsWith('/social-events')) unexpected.push(url.pathname);
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture' } });
  });
  await page.goto('/social/eventos/80?tarea=8000');
  await page.getByRole('button', { name: 'Preparar reasignación' }).click();
  await page.getByRole('combobox', { name: 'Asignación que se reemplaza' }).selectOption('12:responsible');
  await page.getByRole('combobox', { name: 'Nueva persona responsable del rol' }).selectOption('13');
  await page.getByRole('textbox', { name: 'Motivo de la reasignación' }).fill('Cambio de turno de prueba');
  await page.getByRole('button', { name: 'Revisar cambio' }).click();
  const dialog = page.getByRole('dialog', { name: 'Confirmar reasignación RACI' });
  await expect(dialog).toBeVisible(); expect(writes).toHaveLength(0);
  // onEntered owns the safe initial focus; visibility alone does not establish it.
  // Assert the actual accessibility contract, without fixed sleeps or retries.
  await expect(dialog.getByRole('button', { name: 'Volver sin enviar' })).toBeFocused();
  await expect(page.locator('html')).toHaveAttribute('data-theme', theme);
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter(({ impact }) => ['serious', 'critical'].includes(impact)));
  await testInfo.attach('raci-review-accessibility', {
    body: JSON.stringify(violations, null, 2), contentType: 'application/json',
  });
  expect(violations).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('raci-review.png'), fullPage: true });
  await dialog.getByRole('button', { name: 'Confirmar y enviar' }).focus();
  await page.keyboard.press('Enter');
  await expect(dialog.getByText(/No sabemos si se aplicó/)).toBeVisible();
  expect(writes).toHaveLength(1);
  await dialog.getByRole('button', { name: 'Reintentar la misma solicitud' }).click();
  await expect(dialog.getByText('Reasignación confirmada por el servidor.')).toBeVisible();
  expect(writes).toHaveLength(2); expect(unexpected).toEqual([]);
  await dialog.getByRole('button', { name: 'Volver a consultar la tarea' }).click();
  await expect(page.getByRole('table', { name: 'Asignaciones RACI' })).toBeVisible();
});
}

test('RACI accessibility guard detects a known low-contrast fixture @critical', async ({ page }) => {
  // Negative control for the same axe severity filter, on an owned blank page.
  // No application styles, rules or network fixtures are altered to pass the journey.
  await page.route('**/*', route => route.abort('blockedbyclient'));
  await page.setContent('<html lang="es"><head><title>Control de contraste</title></head>'
    + '<body style="background: #fff"><main><p id="contrast-control" style="color: #eee; font-size: 16px">'
    + 'Motivo de prueba con contraste insuficiente.</p></main></body></html>');
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter(({ impact }) => ['serious', 'critical'].includes(impact)));
  const contrast = violations.find(({ id }) => id === 'color-contrast');
  expect(contrast).toBeDefined();
  expect(contrast.nodes.some(({ target, failureSummary }) => target.includes('#contrast-control')
    && failureSummary.includes('contrast'))).toBe(true);
});
