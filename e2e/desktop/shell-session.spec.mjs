import { expect, test } from '@playwright/test';
import { launchShell } from './helpers/shell.mjs';
import { CUSTOMER_ACCOUNT, STAFF_ACCOUNT, createApiFixture } from './helpers/api-fixture.mjs';

// Desktop shell: sign-in with TDF credentials, session persistence across a
// relaunch, server-side expiry, insufficient permissions, deep-link
// validation, offline fallback and the navigation boundary. API is synthetic.

async function signIn(window, { username = 'staff@example.invalid', password = 'secreta-larga-1' } = {}) {
  await window.getByLabel('Usuario o correo').fill(username);
  await window.getByLabel('Contraseña', { exact: false }).first().fill(password);
  await window.getByRole('button', { name: 'Ingresar', exact: true }).click();
}

test('exposes the bridge only on the hosted origin and hides Google sign-in', async ({ baseURL }) => {
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    const api = createApiFixture();
    await api.install(shell.app.context(), baseURL);
    const { window } = shell;
    await shell.openLogin();
    const bridge = await window.evaluate(() => {
      const api = window.tdfDesktop;
      return api ? { app: api.version.app, channel: api.version.channel, unsigned: api.version.unsigned, frozen: Object.isFrozen(api) } : null;
    });
    expect(bridge).toEqual({ app: '0.1.0', channel: 'local', unsigned: true, frozen: true });
    await expect(window.getByTestId('google-desktop-unavailable')).toBeVisible();
    await expect(window.getByText('O continúa con Google')).toHaveCount(0);
    expect(await window.evaluate(() => Array.from(document.scripts).some((script) => script.src.includes('accounts.google.com')))).toBe(false);
    // The shell runs with Chromium isolation and no Node in the page.
    expect(await window.evaluate(() => typeof require === 'undefined' && typeof process === 'undefined')).toBe(true);
    const title = await shell.app.evaluate(({ BrowserWindow }) => BrowserWindow.getAllWindows()[0]?.getTitle());
    expect(title).toContain('prueba sin firma');
  } finally { await shell.close(); }
});

test('signs in with TDF credentials, keeps the session across a relaunch and signs out on the server', async ({ baseURL }) => {
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  let shell = await launchShell({ appOrigin: baseURL });
  const userData = shell.userData;
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    await signIn(shell.window, { password: 'incorrecta' });
    await expect(shell.window.getByText(/Credenciales inválidas/)).toBeVisible();
    await signIn(shell.window);
    await expect(shell.window).not.toHaveURL(/\/login/);
    await expect(shell.window.getByRole('button', { name: /Abrir menú de sesión|Menú de sesión|sesión/i }).first()).toBeVisible();
    expect(api.writesTo('/login')).toHaveLength(2);
    await expect.poll(() => api.state.requests.filter((entry) => entry.headers.authorization === 'Bearer synthetic-desktop-token').length, { timeout: 20_000 }).toBeGreaterThan(0);

    // Relaunch with the same profile: the server session is still valid.
    await shell.app.close();
    shell = await launchShell({ appOrigin: baseURL, env: { TDF_DESKTOP_USER_DATA: userData } });
    await api.install(shell.app.context(), baseURL);
    await expect(shell.window).not.toHaveURL(/\/login/, { timeout: 30_000 });
    await expect.poll(() => api.requestsTo('/session').length).toBeGreaterThan(0);

    // Sign out through the application: the server receives the revocation.
    await shell.window.getByRole('button', { name: /sesión/i }).first().click();
    await shell.window.getByRole('menuitem', { name: /Cerrar sesión/ }).click();
    await expect.poll(() => api.writesTo('/session/logout').length).toBe(1);
    await expect(shell.window).toHaveURL(/\/(login|fans|inicio)?/);
    expect(api.state.authenticated).toBe(false);
  } finally { await shell.close(); }
});

test('an expired server session returns to the login screen', async ({ baseURL }) => {
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    await signIn(shell.window);
    await expect(shell.window).not.toHaveURL(/\/login/);
    api.state.expired = true;
    await shell.window.evaluate(() => { window.history.pushState({}, '', '/crm/contactos'); window.dispatchEvent(new PopStateEvent('popstate')); });
    await expect(shell.window).toHaveURL(/\/login/, { timeout: 30_000 });
    expect(api.requestsTo('/parties').length + api.requestsTo('/session').length).toBeGreaterThan(0);
  } finally { await shell.close(); }
});

test('insufficient permissions show the 403 page instead of staff tools', async ({ baseURL }) => {
  const api = createApiFixture({ account: CUSTOMER_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    await signIn(shell.window, { username: 'fan@example.invalid' });
    await expect(shell.window).not.toHaveURL(/\/login/);
    await shell.window.evaluate(() => { window.history.pushState({}, '', '/operacion/reservas-equipo'); window.dispatchEvent(new PopStateEvent('popstate')); });
    await expect(shell.window.getByText('ERROR 403')).toBeVisible({ timeout: 20_000 });
    expect(api.requestsTo('/bookings')).toHaveLength(0);
  } finally { await shell.close(); }
});

test('validates deep links before navigating and keeps unsafe ones out', async ({ baseURL }) => {
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    const openUrl = (url) => shell.app.evaluate(({ app }, link) => { app.emit('open-url', { preventDefault() {} }, link); }, url);
    await openUrl('tdf-desktop://open?path=/social/eventos');
    await expect(shell.window).toHaveURL(/\/login\?redirect=%2Fsocial%2Feventos|\/social\/eventos/);
    // A token-bearing route reaches the application, but the token never reaches the log.
    await openUrl('tdf-desktop://open?path=%2Fsocial%2Feventos%3Finvite%3Dsecret-invite-token');
    await expect(shell.window).toHaveURL(/invite(=|%3D)secret-invite-token/);
    const before = shell.window.url();
    for (const bad of ['tdf-desktop://open?path=//evil.example/phish?token=secret-rejected-token', 'tdf-desktop://open?path=/../../etc/passwd', 'https://evil.example/eventos/1', 'file:///etc/passwd']) {
      await openUrl(bad);
    }
    await shell.window.waitForTimeout(500);
    expect(shell.window.url()).toBe(before);
    await expect.poll(() => (shell.readLog().match(/rejected deep link/g) ?? []).length).toBe(4);
    const log = shell.readLog();
    expect(log).toMatch(/deep link -> \/social\/eventos\?\[redacted\]/);
    expect(log).toMatch(/rejected deep link tdf-desktop:\/\/open\?\[redacted\]/);
    expect(log).not.toMatch(/secret-invite-token|secret-rejected-token/);
  } finally { await shell.close(); }
});

test('shows the offline fallback when the hosted application is unreachable and recovers on retry', async ({ baseURL }) => {
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    let offline = true;
    await shell.app.context().route(`${baseURL}/**`, (route) => {
      if (offline && route.request().isNavigationRequest()) return route.abort('internetdisconnected');
      return route.fallback();
    });
    await shell.window.evaluate(() => { window.location.assign('/inicio'); }).catch(() => undefined);
    await expect.poll(() => shell.window.url(), { timeout: 30_000 }).toMatch(/^tdf-desktop:\/\/internal\/offline\.html\?code=-106$/);
    await expect(shell.window.getByRole('heading', { name: 'No se pudo conectar con TDF Records' })).toBeVisible();
    offline = false;
    await shell.window.getByRole('link', { name: 'Reintentar ahora' }).click();
    await expect.poll(() => shell.window.url(), { timeout: 30_000 }).toMatch(/\/(login|inicio)/);
    expect(shell.window.url().startsWith(baseURL)).toBe(true);
    expect(shell.readLog()).toMatch(/hosted application failed to load \(-106/);
  } finally { await shell.close(); }
});

test('opens external links in the system browser and blocks foreign schemes', async ({ baseURL }) => {
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    await shell.openLogin();
    await shell.app.evaluate(({ shell: electronShell }) => {
      globalThis.__opened = [];
      electronShell.openExternal = async (url) => { globalThis.__opened.push(url); };
    });
    await shell.window.evaluate(() => { window.open('https://checkout.stripe.com/pay/synthetic', '_blank'); });
    await shell.window.evaluate(() => { const link = document.createElement('a'); link.href = 'https://www.paypal.com/checkoutnow'; link.textContent = 'ext'; document.body.appendChild(link); link.click(); });
    await expect.poll(() => shell.app.evaluate(() => globalThis.__opened)).toEqual(['https://checkout.stripe.com/pay/synthetic', 'https://www.paypal.com/checkoutnow']);
    expect(shell.window.url().startsWith(baseURL)).toBe(true);
    // Chromium refuses file: from an http page before the shell is asked; a
    // registered foreign-scheme target does reach the shell policy and is blocked.
    await shell.window.evaluate(() => { window.open('file:///etc/passwd'); });
    await shell.window.evaluate(() => { const link = document.createElement('a'); link.href = 'file:///etc/passwd'; document.body.appendChild(link); link.click(); });
    await shell.window.evaluate(() => { const link = document.createElement('a'); link.href = 'tdf-desktop://open?path=/x'; document.body.appendChild(link); link.click(); }).catch(() => undefined);
    await shell.window.waitForTimeout(500);
    expect(shell.window.url().startsWith(baseURL)).toBe(true);
    expect(await shell.app.evaluate(({ BrowserWindow }) => BrowserWindow.getAllWindows().length)).toBe(1);
    await expect.poll(() => (shell.readLog().match(/blocked navigation tdf-desktop:\/\/open/g) ?? []).length).toBeGreaterThanOrEqual(1);
    // The renderer bridge refuses unsafe targets as well.
    expect(await shell.window.evaluate(() => window.tdfDesktop.links.openExternal('file:///etc/passwd'))).toBe(false);
    expect(await shell.window.evaluate(() => window.tdfDesktop.links.openExternal('https://example.com/ok'))).toBe(true);
  } finally { await shell.close(); }
});
