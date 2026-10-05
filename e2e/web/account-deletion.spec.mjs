import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test.use({ serviceWorkers: 'block' });
test.skip(process.env.PLAYWRIGHT_ACCOUNT_DELETION_FORM_ENABLED === 'false', 'Enabled-workflow fixtures require explicit synthetic rollout.');

async function fixture(page, baseURL, authenticated = true) {
  const origin = new URL(baseURL).origin;
  const state = {
    session: authenticated ? { username: 'synthetic@example.test', displayName: 'Synthetic QA', partyId: 424242, roles: [], modules: [], featureFlags: [] } : null,
    submissions: [],
    rejectPost: false,
    sessionReads: 0,
  };
  await page.route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') { state.sessionReads += 1; return route.fulfill({ json: state.session }); }
    if (path === '/session/onboarding') return route.fulfill({ json: { eligible: false } });
    if (path === '/feedback/account-deletion' && request.method() === 'POST') {
      if (state.rejectPost) return route.fulfill({ status: 401, body: 'Session revoked' });
      state.submissions.push({ body: request.postData(), authorization: request.headers().authorization });
      return route.fulfill({ json: { adrRequestId: 'synthetic-request', adrCreatedBy: state.session?.partyId } });
    }
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [
      { catalog: { code: 'feedback-categories' }, items: [{ id: '10000000-0000-4000-8000-000000000001', code: 'idea', name: 'Idea', active: true, workflowState: 'published' }], defaults: [] },
      { catalog: { code: 'feedback-severities' }, items: [{ id: '10000000-0000-4000-8000-000000000002', code: 'p4', name: 'P4', active: true, workflowState: 'published' }], defaults: [] },
    ] } });
    return route.fulfill({ json: [] });
  });
  return state;
}

for (const width of [390, 412, 834, 1280]) {
  test(`Account deletion initiation ${width}px @critical`, async ({ page, baseURL }) => {
    await page.setViewportSize({ width, height: 900 });
    const state = await fixture(page, baseURL);
    await page.goto('/cuenta/eliminar');
    await expect(page.getByRole('heading', { name: 'Eliminar tu cuenta TDF', exact: true })).toBeVisible();
    const submit = page.getByRole('button', { name: 'Solicitar eliminación de esta cuenta', exact: true });
    await expect(submit).toBeDisabled();
    const checkbox = page.getByRole('checkbox');
    await expect(checkbox).toBeVisible();
    await expect(page.getByRole('complementary', { name: 'TDF Mobile' })).toHaveCount(0);
    await checkbox.focus(); await page.keyboard.press('Space'); await page.keyboard.press('Tab');
    await expect(submit).toBeFocused();
    expect(await submit.evaluate(el => getComputedStyle(el).outlineStyle)).not.toBe('none');
    expect((await submit.boundingBox()).height).toBeGreaterThanOrEqual(44);
    await page.addScriptTag({ content: axe.source });
    expect(await page.evaluate(async () => (await axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => ({ target: n.target, summary: n.failureSummary })) })))).toEqual([]);
    await page.evaluate(() => { document.documentElement.style.fontSize = '200%'; });
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    const readsBefore = state.sessionReads;
    await submit.click();
    await expect(page.getByRole('status')).toContainText('Solicitud de eliminación recibida');
    expect(state.sessionReads).toBeGreaterThan(readsBefore);
    expect(state.submissions).toHaveLength(1);
    expect(state.submissions[0].authorization).toBeUndefined();
    expect(state.submissions[0].body).toContain('account_deletion_request');
    expect(state.submissions[0].body).toContain('requested_account_party_id: 424242');
    await page.getByRole('button', { name: 'English', exact: true }).click();
    await expect(page.getByRole('heading', { name: 'Delete your TDF account', exact: true })).toBeVisible();
    await expect(page.getByRole('status')).toContainText('Deletion request received');
    expect(state.submissions).toHaveLength(1);
  });
}

test('Account deletion requires authentication @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, false);
  await page.goto('/cuenta/eliminar');
  await expect(page.getByRole('link', { name: 'Iniciar sesión para continuar' })).toHaveAttribute('href', '/login?redirect=%2Fcuenta%2Feliminar');
  await expect(page.getByRole('checkbox')).toHaveCount(0);
  expect(state.submissions).toHaveLength(0);
});

test('Account deletion refuses an expired session before submission @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/cuenta/eliminar');
  await page.getByRole('checkbox').check();
  const submit = page.getByRole('button', { name: 'Solicitar eliminación de esta cuenta', exact: true });
  await expect(submit).toBeEnabled(); state.session = null;
  await submit.click();
  await expect(page.getByRole('alert')).toContainText('No pudimos confirmar el envío');
  expect(state.submissions).toHaveLength(0);
});


test('Account deletion refuses authentication lost between session read and POST @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/cuenta/eliminar');
  await page.getByRole('checkbox').check();
  state.rejectPost = true;
  await page.getByRole('button', { name: 'Solicitar eliminación de esta cuenta', exact: true }).click();
  await expect(page.getByRole('alert')).toContainText('No pudimos confirmar el envío');
  await expect(page.getByRole('status')).toHaveCount(0);
  expect(state.submissions).toHaveLength(0);
});

test('Administrator records an auditable deletion outcome @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  state.session.roles = ['Admin']; state.session.modules = ['internships'];
  const record = { lfdId: 'synthetic-request', lfdTitle: 'Synthetic deletion request', lfdDescription: 'account_deletion_request\nSynthetic QA only.', lfdCreatedBy: 424242, lfdCreatedAt: '2026-10-05T12:00:00Z', lfdDeletionHistory: [] };
  await page.route('**/feedback/internal**', async route => {
    const url = new URL(route.request().url());
    if (url.pathname.endsWith('/account-deletion/synthetic-request') && route.request().method() === 'POST') {
      const payload = route.request().postDataJSON();
      expect(payload.adrOutcome).toBe('completed');
      record.lfdDeletionHistory = [{ adaOutcome: payload.adrOutcome, adaNote: payload.adrNote, adaActor: 77, adaCreatedAt: '2026-10-05T13:00:00Z' }];
      return route.fulfill({ json: record.lfdDeletionHistory[0] });
    }
    return route.fulfill({ json: url.searchParams.get('accountDeletionOnly') === 'true' ? [record] : [] });
  });
  await page.goto('/feedback/interno');
  await expect(page.getByRole('heading', { name: 'Solicitudes de eliminación de cuenta' })).toBeVisible();
  const complete = page.getByRole('button', { name: 'Registrar eliminación completada' });
  await expect(complete).toBeDisabled();
  const note = page.getByLabel('Resultado y verificación del procesamiento');
  await note.fill('Synthetic completion verified; no real account erased.');
  await expect(complete).toBeEnabled();
  expect((await complete.boundingBox()).height).toBeGreaterThanOrEqual(44);
  await complete.focus(); await page.keyboard.press('Enter');
  await expect(page.getByText('Completada', { exact: true })).toBeVisible();
  await expect(page.getByText(/Operador 77/)).toBeVisible();
  await expect(complete).toHaveCount(0);
});
