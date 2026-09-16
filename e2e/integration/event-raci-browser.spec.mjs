import { expect, test } from '@playwright/test';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import axe from 'axe-core';
import { localApiFixturePattern } from '../web/helpers/local-api-fixture.mjs';

function sql(statement) {
  const container = process.env.EVENT_RACI_TEST_CONTAINER ?? '';
  if (process.env.EVENT_RACI_DISPOSABLE_BROWSER_TEST !== '1' || !/^[a-f0-9]{64}$/.test(container)) {
    throw new Error('Owned disposable database required');
  }
  return execFileSync('docker', ['exec', container, 'psql', '-X', '-v', 'ON_ERROR_STOP=1',
    '-U', 'postgres', '-d', 'tdf_event_raci_browser_test', '-qAtc', statement],
  { encoding: 'utf8', timeout: 30_000 }).trim();
}
function ids(info, offset) {
  const event = (info.project.name === 'chromium-phone' ? 200 : 100) + offset;
  return { event, task: event * 100, path: `/event-operations/events/${event}/tasks/${event * 100}` };
}
function seed({ event, task }) {
  sql(`BEGIN;
    INSERT INTO social_event(id,organizer_party_id) VALUES (${event},'1');
    INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
      VALUES (${event},'planning',1,'real browser fixture');
    INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
      VALUES (${event},1,'primary_owner');
    INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (${task},${event},'planned',1);
    INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
      VALUES (${task},1,'accountable',1),(${task},3,'responsible',1);
    INSERT INTO event_operation_task_policy(activity_id) VALUES (${task});
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
      VALUES (${event},2,'task.read','task','${task}',1);
    COMMIT;`);
}
function persisted({ event, task }) {
  return JSON.parse(sql(`SELECT json_build_object(
    'revision',(SELECT revision FROM event_operation_task_revision WHERE activity_id=${task}),
    'responsible',(SELECT array_agg(party_id ORDER BY party_id) FROM event_operation_raci_assignment
      WHERE activity_id=${task} AND raci_role='responsible' AND revoked_at IS NULL),
    'audit',(SELECT count(*) FROM event_operation_audit_event WHERE event_id=${event}),
    'receipts',(SELECT count(*) FROM event_operation_command_receipt WHERE event_id=${event}));`));
}
async function open(page, baseURL, target, token = 'http-owner-test-token', party = 1) {
  await page.context().addCookies([{ name: 'tdf_session', value: token, url: baseURL, httpOnly: true, sameSite: 'Lax' }]);
  const sessionResponse = page.waitForResponse(response => new URL(response.url()).pathname === '/session');
  await page.goto(`/social/eventos/${target.event}?tarea=${target.task}`);
  const session = await sessionResponse;
  expect(session.status()).toBe(200);
  expect((await session.json()).partyId).toBe(party);
}
async function review(page) {
  await page.getByRole('button', { name: 'Preparar reasignación' }).click();
  await page.getByRole('combobox', { name: 'Asignación que se reemplaza' }).selectOption('3:responsible');
  await page.getByRole('combobox', { name: 'Nueva persona responsable del rol' }).selectOption('2');
  await page.getByRole('textbox', { name: 'Motivo de la reasignación' }).fill('Cambio de turno verificado');
  await page.getByRole('button', { name: 'Revisar cambio' }).click();
  return page.getByRole('dialog', { name: 'Confirmar reasignación RACI' });
}

test.beforeEach(async ({ page, context, baseURL }) => {
  const origin = new URL(baseURL).origin;
  await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'es'));
  await context.routeWebSocket('**/*', socket => {
    const url = new URL(socket.url());
    if (url.protocol === 'ws:' && url.host === new URL(baseURL).host) socket.connectToServer();
    else socket.close();
  });
  await context.route(localApiFixturePattern(baseURL), route => {
    const request = route.request(); const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    if (url.pathname === '/session' || url.pathname.startsWith('/event-operations/')) return route.continue();
    return route.abort('blockedbyclient');
  });
});

test('real commit, lost response, exact replay and database audit', async ({ page, baseURL }, info) => {
  const target = ids(info, 0); seed(target);
  const attempts = []; const receipts = [];
  await page.route(new URL(`${target.path}/raci/reassign`, baseURL).href, async route => {
    attempts.push({ key: route.request().headers()['idempotency-key'], body: route.request().postDataJSON() });
    // Forward to the real server first. The only injected failure is dropping its response.
    const response = await route.fetch({ maxRetries: 0, maxRedirects: 0 });
    expect(response.status()).toBe(200);
    receipts.push(await response.json());
    if (attempts.length === 1) return route.abort('failed');
    return route.fulfill({ response });
  });
  await open(page, baseURL, target);
  const dialog = await review(page);
  await page.addScriptTag({ content: axe.source });
  expect(await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter(({ impact }) => ['serious', 'critical'].includes(impact)).map(({ id }) => id))).toEqual([]);
  await page.screenshot({ path: info.outputPath('real-raci-review.png'), fullPage: true });
  expect(persisted(target)).toEqual({ revision: 4, responsible: [3], audit: 0, receipts: 0 });
  await dialog.getByRole('button', { name: 'Confirmar y enviar' }).click();
  await expect(dialog.getByText(/No sabemos si se aplicó/)).toBeVisible();
  expect(receipts[0]).toMatchObject({ aggregateRevision: '6', replayed: false });
  const committed = persisted(target);
  expect(committed).toEqual({ revision: 6, responsible: [2], audit: 1, receipts: 1 });
  await dialog.getByRole('button', { name: 'Reintentar la misma solicitud' }).click();
  await expect(dialog.getByText('Reasignación confirmada por el servidor.')).toBeVisible();
  expect(attempts).toHaveLength(2); expect(attempts[1]).toEqual(attempts[0]);
  expect(receipts[1]).toEqual({ ...receipts[0], replayed: true });
  expect(persisted(target)).toEqual(committed);
  expect(sql(`SELECT command_id::TEXT FROM event_operation_audit_event WHERE event_id=${target.event}`)).toBe(attempts[0].key);
  await dialog.getByRole('button', { name: 'Volver a consultar la tarea' }).click();
  const table = page.getByRole('table', { name: 'Asignaciones RACI' });
  await expect(table.getByText('Persona #2', { exact: true })).toBeVisible();
  await expect(table.getByText('Persona #3', { exact: true })).toHaveCount(0);
});

test('real concurrent write rejects the stale reviewed revision', async ({ page, baseURL }, info) => {
  const target = ids(info, 1); seed(target);
  await open(page, baseURL, target);
  const dialog = await review(page);
  const concurrent = await page.request.post(`${target.path}/raci/reassign`, {
    maxRedirects: 0,
    headers: { 'Idempotency-Key': randomUUID() },
    data: { expectedRevision: '4', role: 'responsible', fromPartyId: 3, toPartyId: 2,
      reason: 'Otro gestor confirma antes', correlationId: 'browser-concurrent' },
  });
  expect(concurrent.status()).toBe(200);
  const newer = persisted(target);
  const rejected = page.waitForResponse(response => response.url().endsWith('/raci/reassign'));
  await dialog.getByRole('button', { name: 'Confirmar y enviar' }).click();
  expect((await rejected).status()).toBe(409);
  await expect(dialog.getByText(/La tarea cambió/)).toBeVisible();
  expect(persisted(target)).toEqual(newer);
});

test('real token revocation after review prevents mutation', async ({ page, baseURL }, info) => {
  const target = ids(info, 2); seed(target);
  sql("UPDATE api_token SET active=TRUE WHERE token='http-revocable-test-token'");
  await open(page, baseURL, target, 'http-revocable-test-token');
  const dialog = await review(page);
  sql("UPDATE api_token SET active=FALSE WHERE token='http-revocable-test-token'");
  const rejected = page.waitForResponse(response => response.url().endsWith('/raci/reassign'));
  await dialog.getByRole('button', { name: 'Confirmar y enviar' }).click();
  expect((await rejected).status()).toBe(401);
  // The actual API client's recognized auth error expires the local session, so private
  // task/review data must disappear instead of keeping an uncertain dialog after logout.
  await expect(page).toHaveURL(url => url.origin === new URL(baseURL).origin && url.pathname === '/login'
    && url.searchParams.get('redirect') === `/social/eventos/${target.event}?tarea=${target.task}`);
  await expect(page.getByRole('heading', { name: 'Iniciar sesión', exact: true })).toBeVisible();
  await expect(dialog).toHaveCount(0);
  await expect(page.getByRole('table', { name: 'Asignaciones RACI' })).toHaveCount(0);
  expect(persisted(target)).toEqual({ revision: 4, responsible: [3], audit: 0, receipts: 0 });
});

test('real scoped reader and assigned outsider remain least privileged', async ({ page, baseURL }, info) => {
  const target = ids(info, 3); seed(target);
  await open(page, baseURL, target, 'http-collaborator-test-token', 2);
  await page.getByRole('button', { name: 'Preparar reasignación' }).click();
  await expect(page.getByText('No hay una reasignación disponible con tus permisos y el estado actual.')).toBeVisible();
  await expect(page.getByRole('combobox', { name: 'Nueva persona responsable del rol' })).toHaveCount(0);
  const forged = await page.request.post(`${target.path}/raci/reassign`, {
    maxRedirects: 0,
    headers: { 'Idempotency-Key': randomUUID() },
    data: { expectedRevision: '4', role: 'responsible', fromPartyId: 3, toPartyId: 2,
      reason: 'Intento sin autorización', correlationId: 'browser-forbidden' },
  });
  expect(forged.status()).toBe(403);
  const rejected = page.waitForResponse(response => new URL(response.url()).pathname === target.path);
  await open(page, baseURL, target, 'http-outsider-test-token', 3);
  expect((await rejected).status()).toBe(404);
  await expect(page.getByText(/No pudimos consultar esta tarea/)).toBeVisible();
  await expect(page.getByRole('table', { name: 'Asignaciones RACI' })).toHaveCount(0);
  expect(persisted(target)).toEqual({ revision: 4, responsible: [3], audit: 0, receipts: 0 });
});
