import { expect, test } from '@playwright/test';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { disposablePostgresUrl } from '../../scripts/lib/disposable-postgres-url.mjs';

const dsn = process.env.TDF_TICKET_JOURNEY_DSN ?? '';
const database = disposablePostgresUrl(dsn);
if (database.hostname !== '127.0.0.1' || !/^\/tdf_journey_ticket_\w*test$/.test(database.pathname)) {
  throw new Error('An owned loopback tdf_journey_ticket_*test database is required');
}
const sql = (statement) => execFileSync('psql', [dsn, '-XqAt', '-v', 'ON_ERROR_STOP=1', '-c', statement], {
  encoding: 'utf8', env: { PATH: process.env.PATH },
}).trim();

test('bank transfer buyer retrieves the same issued ticket after sales close', async ({ page, context, request, browser, baseURL }, testInfo) => {
  const suffix = randomUUID();
  const staffToken = `synthetic-ticket-staff-${suffix}`;
  const staffId = Number(sql(`INSERT INTO party(display_name,is_org,primary_email,created_at)
    VALUES ('Personal sintético',false,'staff-${suffix}@persona.test',now()) RETURNING id`));
  sql(`INSERT INTO party_role(party_id,role,active) VALUES (${staffId},'Admin',true);
    INSERT INTO api_token(token,party_id,label,active) VALUES ('${staffToken}',${staffId},'synthetic-ticket-test',true);
    INSERT INTO commerce_provider_account(provider,environment,merchant_account_ref,status,contract_status,
      credential_status,feature_flag_key,enabled,verified_at,verified_by)
    VALUES ('bank_transfer','sandbox','tdf-manual-settlement','ready','approved','validated','synthetic-ticket-test',true,now(),${staffId})
    ON CONFLICT(provider,environment) DO NOTHING;
    INSERT INTO commerce_provider_capability(provider_account_id,payment_method,capability,verification_status,source_reference,verified_at)
    SELECT id,'manual_bank_transfer','one_time','sandbox_verified','synthetic-local-staff',now()
    FROM commerce_provider_account WHERE provider='bank_transfer' AND environment='sandbox'
    ON CONFLICT(provider_account_id,payment_method,capability) DO NOTHING;`);
  const eventId = Number(sql(`INSERT INTO social_event(organizer_party_id,title,description,start_time,end_time,timezone,metadata,workflow_state_id)
    SELECT '${staffId}','Concierto sintético de recuperación','Evento local de prueba',now()+interval '7 days',now()+interval '7 days 2 hours',
      'America/Guayaquil','{"isPublic":true}',s.id
    FROM workflow_state s JOIN workflow_definition w ON w.id=s.workflow_id
    WHERE w.code='social-event-lifecycle' AND s.code='on_sale' RETURNING social_event.id`));
  const tierId = Number(sql(`INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active)
    VALUES (${eventId},'GENERAL','General sintética',2000,'USD',10,0,true) RETURNING id`));
  sql(`INSERT INTO event_ticket_checkout_policy(event_id,policy_version,currency,terms_version,terms_summary,refund_policy,
    approval_status,active,approved_at,approved_by,manual_transfer_hold_minutes,manual_transfer_cutoff_at)
    VALUES (${eventId},'synthetic-v1','USD','synthetic-terms-v1','Condiciones de prueba local.','Reembolso de prueba local.',
      'approved',true,now(),'synthetic-staff',1440,now()+interval '6 days');`);
  const external = [];
  await context.route('**/*', route => {
    const url = new URL(route.request().url());
    if (url.hostname === '127.0.0.1' || ['data:', 'blob:'].includes(url.protocol)) return route.continue();
    external.push(url.origin);
    return route.abort('blockedbyclient');
  });
  await page.goto(`/eventos/${eventId}`);
  await page.getByRole('link', { name: 'Ver entradas', exact: true }).click();
  await expect(page).toHaveURL(new RegExp(`/eventos/${eventId}/entradas`));
  await page.getByLabel('Nombre completo', { exact: true }).fill('Compradora Sintética');
  await page.getByLabel('Email', { exact: true }).fill(`buyer-${suffix}@persona.test`);
  await page.getByRole('checkbox').check();
  const created = page.waitForResponse(response => response.request().method() === 'POST'
    && new URL(response.url()).pathname === `/public/events/${eventId}/ticket-orders`);
  await page.getByRole('button', { name: 'Retener entradas y revisar total', exact: true }).click();
  const response = await created;
  expect(response.status(), await response.text()).toBe(200);
  const order = await response.json();
  const orderPath = `/eventos/${eventId}/orden/${order.orderId}`;
  await expect(page).toHaveURL(new RegExp(`${orderPath}$`));
  await page.getByRole('button', { name: 'Transferencia bancaria', exact: true }).click();
  await page.getByLabel('Número de comprobante de la transferencia').fill(`LOCAL-${suffix}`);
  await page.getByRole('button', { name: 'Ya transferí', exact: true }).click();
  await expect(page.getByText(/Recibimos tu referencia/)).toBeVisible();
  expect(sql(`SELECT payment_status||':'||fulfillment_status FROM event_ticket_checkout_runtime WHERE order_id=${order.orderId}`))
    .toBe('awaiting_payment:seat_held');
  const review = await request.post(`/social-events/events/${eventId}/ticket-orders/${order.orderId}/manual-payment/review`, {
    headers: { Authorization: `Bearer ${staffToken}` },
    data: { tmprAction: 'approve', tmprNotes: 'Depósito simulado verificado en prueba local' },
  });
  expect(review.status(), await review.text()).toBe(200);
  // Repeat staff confirmation: no second payment or ticket may result.
  const repeated = await request.post(`/social-events/events/${eventId}/ticket-orders/${order.orderId}/manual-payment/review`, {
    headers: { Authorization: `Bearer ${staffToken}` },
    data: { tmprAction: 'approve', tmprNotes: 'Depósito simulado verificado en prueba local' },
  });
  expect(repeated.status(), await repeated.text()).toBe(200);
  const state = () => sql(`SELECT r.payment_status||':'||r.fulfillment_status||':'||
    (SELECT count(*) FROM event_ticket t WHERE t.order_ref_id=r.order_id)||':'||
    (SELECT count(*) FROM commerce_payment_attempt a WHERE a.checkout_id=r.checkout_id AND a.status='succeeded')
    FROM event_ticket_checkout_runtime r WHERE r.order_id=${order.orderId}`);
  expect(state()).toBe('paid:issued:1:1');
  expect(sql(`SELECT count(*) FROM event_ticket_confirmation_delivery WHERE order_id=${order.orderId}`)).toBe('1');
  await page.reload();
  await expect(page.getByText('Entradas emitidas', { exact: true })).toBeVisible();
  await expect(page.getByRole('img', { name: 'Código QR privado de acceso' })).toBeVisible();
  const code = sql(`SELECT code FROM event_ticket WHERE order_ref_id=${order.orderId}`);
  await expect(page.getByText(code, { exact: true })).toBeVisible();
  // Real sales lifecycle change, not a mocked HTTP error. Issued tickets remain valid.
  sql(`UPDATE social_event SET workflow_state_id=(SELECT s.id FROM workflow_state s JOIN workflow_definition w
    ON w.id=s.workflow_id WHERE w.code='social-event-lifecycle' AND s.code='postponed') WHERE id=${eventId}`);
  expect((await request.get(`/public/events/${eventId}/tickets`)).status()).toBe(404);
  const authorized = await request.get(`/public/events/${eventId}/ticket-orders/${order.orderId}`, {
    headers: { 'X-Order-Lookup-Token': order.lookupToken },
  });
  expect(authorized.status()).toBe(200);
  expect((await authorized.json()).tickets[0].ticketCode).toBe(code);
  expect((await request.get(`/public/events/${eventId}/ticket-orders/${order.orderId}`)).status()).toBe(404);
  expect((await request.get(`/public/events/${eventId}/ticket-orders/${order.orderId}`, {
    headers: { Authorization: `Bearer ${staffToken}` },
  })).status()).toBe(404);
  // Reopen in the same browser: the existing capability must survive the closed storefront.
  await page.close();
  const reopened = await context.newPage();
  await reopened.goto(orderPath);
  await expect(reopened.getByText('Entradas emitidas', { exact: true })).toBeVisible();
  await expect(reopened.getByText(code, { exact: true })).toBeVisible();
  await expect(reopened.getByRole('img', { name: 'Código QR privado de acceso' })).toBeVisible();
  expect(await reopened.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await reopened.screenshot({ path: testInfo.outputPath('reopened-ticket.png'), fullPage: true });
  expect(state()).toBe('paid:issued:1:1');
  expect(sql(`SELECT quantity_sold FROM event_ticket_tier WHERE id=${tierId}`)).toBe('1');
  // A different browser/account has no order capability and must not see this receipt.
  const otherContext = await browser.newContext({ baseURL, viewport: { width: 360, height: 800 } });
  try {
    await otherContext.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1'
      ? route.continue() : route.abort('blockedbyclient'));
    const otherPage = await otherContext.newPage();
    await otherPage.goto(orderPath);
    await expect(otherPage.getByText(/navegador donde compraste/)).toBeVisible();
    await expect(otherPage.getByText(code, { exact: true })).toHaveCount(0);
    await expect(otherPage.getByRole('button', { name: 'Retener entradas y revisar total', exact: true })).toHaveCount(0);
  } finally { await otherContext.close(); }
  expect(external).toEqual([]);
});
