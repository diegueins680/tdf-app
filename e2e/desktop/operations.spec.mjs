import { expect, test } from '@playwright/test';
import { launchShell } from './helpers/shell.mjs';
import { STAFF_ACCOUNT, createApiFixture } from './helpers/api-fixture.mjs';

// Flows 3 and 4: authorized staff consult studio bookings and clients, and
// manage events, campaigns and sales from the shell through the same API
// routes the web uses. Every request carries the TDF session credential.

const navigate = (window, path) => window.evaluate((target) => { window.history.pushState({}, '', target); window.dispatchEvent(new PopStateEvent('popstate')); }, path);

test('staff reach studio bookings, clients, events, campaigns and sales with their TDF session', async ({ baseURL }) => {
  test.setTimeout(150_000);
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const today = new Date();
  const start = new Date(Date.UTC(today.getUTCFullYear(), today.getUTCMonth(), today.getUTCDate(), 19, 0, 0));
  api.state.bookings[0].startsAt = start.toISOString();
  api.state.bookings[0].endsAt = new Date(start.getTime() + 2 * 3_600_000).toISOString();
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    const { window } = shell;
    await shell.openLogin();
    await window.getByLabel('Usuario o correo').fill('staff@example.invalid');
    await window.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
    await window.getByRole('button', { name: 'Ingresar', exact: true }).click();
    await expect(window).not.toHaveURL(/\/login/);

    await navigate(window, '/estudio/calendario');
    await expect.poll(() => api.requestsTo('/bookings').length, { timeout: 30_000 }).toBeGreaterThan(0);
    await expect(window.getByText('Grabación — Banda sintética').first()).toBeVisible({ timeout: 30_000 });
    expect(api.requestsTo('/rooms').length).toBeGreaterThan(0);

    await navigate(window, '/crm/contactos');
    await expect.poll(() => api.requestsTo('/parties').length, { timeout: 30_000 }).toBeGreaterThan(0);
    await expect(window.getByText(/banda@example\.invalid|@bandasintetica|Banda sintética/).first()).toBeVisible({ timeout: 30_000 });

    await navigate(window, '/social/eventos');
    await expect(window.getByRole('heading', { name: 'Eventos', exact: true })).toBeVisible({ timeout: 30_000 });
    await expect.poll(() => api.requestsTo('/social-events/events').length, { timeout: 30_000 }).toBeGreaterThan(0);
    await expect(window.getByText('Noche sintética en el Domo').first()).toBeVisible({ timeout: 30_000 });

    await navigate(window, '/social/campanas');
    await expect(window.getByRole('heading', { name: 'Campañas musicales' })).toBeVisible({ timeout: 30_000 });

    await navigate(window, '/finanzas/pagos');
    await expect.poll(() => api.requestsTo('/payments').length, { timeout: 30_000 }).toBeGreaterThan(0);
    await expect(window.getByText('REC-0001').first()).toBeVisible({ timeout: 30_000 });

    // Every data read of the four operational areas carried the TDF session credential.
    const dataPaths = ['/bookings', '/rooms', '/parties', '/social-events/events', '/payments'];
    const dataRequests = api.state.requests.filter((entry) => dataPaths.includes(entry.path));
    expect(dataRequests.length).toBeGreaterThanOrEqual(dataPaths.length);
    expect(dataRequests.every((entry) => entry.headers.authorization === 'Bearer synthetic-desktop-token')).toBe(true);
  } finally { await shell.close(); }
});
