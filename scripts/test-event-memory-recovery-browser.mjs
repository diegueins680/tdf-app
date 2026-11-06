import { expect } from '@playwright/test';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';

/** Actual reset request/confirmation; only delivery is relayed by the owned fixture. */
export async function verifyRecoveredPublication({ context, owner, config, origin, output, result }) {
  if (config.api !== 'http://127.0.0.1:18886' || config.eventId !== '919143001'
    || config.accounts[1].email !== 'memory.participant@persona.test') throw new Error('Synthetic recovery fixture only');
  const page = await context.newPage();
  const guest = `/social/eventos/${config.eventId}?utm_source=synthetic-recovery#momentos`;
  const destination = `/social/eventos/${config.eventId}?utm_source=synthetic-recovery&compose=1#momentos`;
  await page.goto(origin + guest, { waitUntil: 'domcontentloaded' });
  await page.getByRole('link', { name: 'Compartir mis momentos', exact: true }).click();
  await expect(page.locator('input[autocomplete="username"]')).toBeVisible({ timeout: 30000 });
  expect(new URL(page.url()).searchParams.get('redirect')).toBe(destination);
  await page.getByRole('button', { name: 'Recuperar acceso', exact: true }).click();
  const dialog = page.getByRole('dialog');
  await expect(dialog).toBeVisible();
  await dialog.locator('input[type="email"]').fill(config.accounts[1].email);
  const requested = page.waitForResponse(response => new URL(response.url()).pathname === '/v1/password-reset' && response.request().method() === 'POST');
  await dialog.locator('button[type="submit"]').click();
  const issuance = await requested;
  // PasswordResetAPI is Post '[JSON] NoContent (HTTP200), not PostNoContent204.
  expect(issuance.status()).toBe(200);
  expect(new URL(issuance.url()).searchParams.get('redirect')).toBe(destination);
  expect(new URL(issuance.url()).searchParams.get('locale')).toBe('es');
  expect(issuance.request().postDataJSON().email).toBe(config.accounts[1].email);

  // SMTP is disabled. The owning HTTP fixture reads the actual freshly issued
  // challenge from its disposable DB; it never fabricates a successful API response.
  writeFileSync(path.join(output, 'recovery-requested'), 'owned-memory-recovery-fixture-v1', { flag: 'wx', mode: 0o600 });
  const linkFile = path.join(output, 'recovery-link-private.json');
  await expect.poll(() => existsSync(linkFile), { timeout: 15000 }).toBe(true);
  const challenge = JSON.parse(readFileSync(linkFile, 'utf8'));
  if (challenge.source !== 'actual-owned-reset-issuance' || !Number.isSafeInteger(challenge.partyId)
    || !/^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/.test(challenge.token)) throw new Error('Invalid owned recovery relay');
  // Match Email.passwordResetLinkWithLocale and the existing public /reset route.
  const link = new URL('/reset', origin);
  link.searchParams.set('token', challenge.token); link.searchParams.set('redirect', destination); link.searchParams.set('lang', 'es');
  await page.goto(link.href, { waitUntil: 'domcontentloaded' }).catch(() => { throw new Error('Owned recovery navigation failed'); });
  await expect(page.locator('input[autocomplete="new-password"]')).toHaveCount(2);
  await expect.poll(() => new URL(page.url()).searchParams.has('token')).toBe(false);
  expect(new URL(page.url()).searchParams.get('redirect')).toBe(destination);
  const password = 'synthetic-recovered-password-141';
  await page.locator('input[autocomplete="new-password"]').nth(0).fill(password);
  await page.locator('input[autocomplete="new-password"]').nth(1).fill(password);
  const confirmed = page.waitForResponse(response => new URL(response.url()).pathname === '/v1/password-reset/confirm' && response.request().method() === 'POST');
  await page.locator('button[type="submit"]').click();
  const confirmation = await confirmed;
  expect(confirmation.status()).toBe(200);
  // Boolean comparisons prevent assertion messages from printing a live token.
  expect(confirmation.request().postDataJSON().token === challenge.token).toBe(true);
  const account = await confirmation.json();
  expect(account.partyId).toBe(challenge.partyId);
  await expect(page).toHaveURL(origin + destination, { timeout: 30000 });
  await expect(page.getByRole('combobox', { name: 'Tipo de recuerdo' })).toBeVisible({ timeout: 30000 });
  const auth = { Authorization: `Bearer ${account.token}` };
  const preference = await context.request.get(config.api + '/social-events/me/memory-invitations', { headers: auth });
  expect(preference.status()).toBe(200); expect((await preference.json()).inApp).toBe(true);
  const privateRead = await context.request.get(config.api + '/social-events/events/919143002/moments?extended', { headers: auth });
  expect(privateRead.status()).toBe(404);
  const campaign = await context.request.get(config.api + `/social-events/events/${config.eventId}/memory-invitations`, { headers: auth });
  expect(campaign.status()).toBe(404);
  result.assertions.push('Actual password-reset request and confirmation preserve the full invitation/composer target, scrub the URL token and restore the same participant without new permissions');

  const caption = 'Recuerdo sintético después de recuperar mi cuenta';
  await page.getByRole('combobox', { name: 'Tipo de recuerdo' }).click();
  await page.getByRole('option', { name: 'Solo texto' }).click();
  await page.getByRole('textbox', { name: 'Describe tu publicación' }).fill(caption);
  const posted = page.waitForResponse(response => response.url() === config.api + `/social-events/events/${config.eventId}/moments` && response.request().method() === 'POST');
  await page.getByRole('button', { name: 'Publicar', exact: true }).click();
  const publication = await posted; expect(publication.status()).toBe(200);
  const moment = await publication.json();
  expect(String(moment.emAuthorPartyId)).toBe(String(challenge.partyId));
  expect(String(moment.emEventId)).toBe(config.eventId);
  const card = current => current.getByRole('region', { name: /^Publicación de / }).filter({ hasText: caption });
  await expect(card(page)).toBeVisible({ timeout: 30000 });
  await page.reload(); await expect(card(page)).toBeVisible({ timeout: 30000 });
  await owner.reload(); await expect(card(owner)).toBeVisible({ timeout: 30000 });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  await card(page).screenshot({ path: path.join(output, 'recovery-memory-phone.png') });
  result.recoveredAccount = { partyId: challenge.partyId, momentId: String(moment.emId), requestStatus: issuance.status(), confirmationStatus: confirmation.status(), publicationStatus: publication.status(), tokenRemovedFromUrl: true, delivery: 'private relay from owned disposable DB; SMTP disabled, no email delivery claimed', invitationPreferenceBefore: true, invitationPreferenceAfter: true, privateReadStatus: privateRead.status(), organizerCampaignStatus: campaign.status(), fixtureRoleGrantsDuringRecovery: 0 };
  result.assertions.push('Recovered participant publishes actual text visible after author/organizer reloads while existing consent and private-event/organizer restrictions remain unchanged');
}
