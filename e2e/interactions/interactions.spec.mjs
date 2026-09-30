import { expect, test } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import axe from 'axe-core';
const fixture = JSON.parse(readFileSync(process.env.TDF_INTERACTION_TEST_FIXTURE, 'utf8'));
if (!['127.0.0.1', 'localhost'].includes(new URL(fixture.base).hostname) || !fixture.database.startsWith('tdf_interaction_')) throw new Error('Isolated local fixture required');
async function api(request, actor, path, data) {
  const response = await request.fetch(fixture.base + path, { method: data ? 'POST' : 'GET', headers: { Authorization: `Bearer ${fixture.tokens[actor]}` }, data });
  expect(response.status(), await response.text()).toBe(200); return response.json();
}
async function client(browser, baseURL, actor, options = {}) {
  const context = await browser.newContext({ baseURL, ...options }); const page = await context.newPage();
  await context.route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === new URL(baseURL).origin ? route.continue() : route.abort();
    if (/^\/(interactions|public\/interactions|session|fans\/me\/notifications|parties\/search)(\/|\?|$)/.test(url.pathname)) {
      const response = await context.request.fetch(fixture.base + url.pathname + url.search, {
        method: request.method(), headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${fixture.tokens[actor]}` }, data: request.postData() ?? undefined,
      });
      return route.fulfill({ response });
    }
    if (url.pathname.startsWith('/catalog')) return route.fulfill({ json: { catalogs: [], items: [], defaults: [] } });
    if (url.pathname.endsWith('/rsvp-feed')) return route.fulfill({ json: { feedItems: [], feedNextCursor: null } });
    return route.fulfill({ json: [] });
  });
  return { context, page };
}

test('real discussion: disclosure, pagination, reaction reconciliation and accessible deep-link focus @critical', async ({ page, context, baseURL, request }) => {
  // Use the project-configured viewport/touch/engine; only network routing is shared.
  const actor = fixture.actors[0];
  const isolated = await client({ newContext: async () => context }, baseURL, actor);
  page = isolated.page;
  await page.goto(`/conversacion/target/${fixture.targetId}`);
  const section = page.getByRole('region', { name: 'Reacciones y conversación' });
  await expect(section.getByRole('button', { name: 'Ocultar comentarios', exact: true })).toBeVisible();
  const comments = section.getByRole('article'); await expect(comments).toHaveCount(20);
  await section.getByRole('button', { name: 'Ver más comentarios' }).click(); await expect(comments).toHaveCount(40);
  await section.getByRole('button', { name: 'Ocultar comentarios', exact: true }).click(); await expect(comments).toHaveCount(0);
  const expand = section.getByRole('button', { name: /^Ver los \d+ comentarios$/ });
  await expand.focus(); await page.keyboard.press('Enter'); await expect(comments).toHaveCount(40);
  const like = section.getByRole('button', { name: /^Me gusta:/ });
  const previous = await like.getAttribute('aria-pressed'); await like.click();
  await expect(like).toHaveAttribute('aria-pressed', previous === 'true' ? 'false' : 'true');
  const authoritative = await api(request, actor, `/interactions/targets/club_post/${fixture.postId}`);
  expect(Boolean(authoritative.myReactionTypeId)).toBe(previous !== 'true');
  await page.goto(`/conversacion/comment/${fixture.replyId}`);
  const target = page.locator(`#comment-${fixture.replyId}`); await expect(target).toBeVisible(); await expect(target).toBeFocused();
  await expect(page.locator(`#comment-${fixture.rootId}`).getByText('Comentario eliminado', { exact: true })).toBeVisible();
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await window.axe.run(document.querySelector('main') ?? document.body)).violations.filter(v => ['serious', 'critical'].includes(v.impact)));
  expect(violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.target) }))).toEqual([]);
});

test('real authoring, notification destination, editing and parent deletion retain replies @critical', async ({ browser, baseURL, request }, info) => {
  const owner = fixture.actors[0], fan = fixture.actors[2];
  const options = Object.fromEntries(['viewport', 'hasTouch', 'isMobile', 'deviceScaleFactor', 'userAgent', 'colorScheme'].filter(key => key in info.project.use).map(key => [key, info.project.use[key]]));
  const a = await client(browser, baseURL, owner, options), b = await client(browser, baseURL, fan, options);
  try {
    await a.page.goto(`/conversacion/target/${fixture.targetId}`);
    const body = `Browser discussion ${randomUUID()}`;
    await a.page.getByRole('textbox', { name: 'Escribe un comentario', exact: true }).fill(body);
    await a.page.getByRole('button', { name: 'Publicar', exact: true }).click();
    await expect(a.page.getByText(body, { exact: true })).toBeVisible();
    const roots = await api(request, owner, `/interactions/targets/club_post/${fixture.postId}/comments?sort=newest&limit=20`);
    const root = roots.items.find(row => row.body === body); expect(root).toBeTruthy();
    await b.page.goto(`/conversacion/comment/${root.id}`);
    const rootCard = b.page.locator(`#comment-${root.id}`); await rootCard.getByRole('button', { name: 'Responder', exact: true }).click();
    const replyBody = `Browser reply ${randomUUID()}`;
    await b.page.getByRole('textbox', { name: /Responde/ }).fill(replyBody);
    await b.page.locator('form').filter({ has: b.page.getByRole('textbox', { name: /Responder a/ }) }).getByRole('button', { name: 'Publicar', exact: true }).click();
    await expect(b.page.getByText(replyBody, { exact: true })).toBeVisible();
    execFileSync('psql', ['-X', '-v', 'ON_ERROR_STOP=1', '-d', fixture.database, '-c', 'SELECT interaction_dispatch_events(20);'], { stdio: 'pipe' });
    let notification;
    const replies = await api(request, owner, `/interactions/targets/club_post/${fixture.postId}/comments?root=${root.id}&sort=oldest&limit=20`);
    const reply = replies.items.find(row => row.body === replyBody); expect(reply).toBeTruthy();
    await expect.poll(async () => { notification = (await api(request, owner, '/fans/me/notifications')).find(row => row.nTargetKey === reply.id); return Boolean(notification); }, { timeout: 20000 }).toBe(true);
    await a.page.goto(`/notificaciones/${notification.nId}`);
    await expect(a.page).toHaveURL(new RegExp(`/conversacion/comment/${reply.id}$`));
    await expect(a.page.locator(`#comment-${reply.id}`)).toBeFocused();
    const parent = a.page.locator(`#comment-${root.id}`);
    await parent.getByRole('button', { name: 'Opciones del comentario' }).click();
    await a.page.getByRole('menuitem', { name: 'Editar', exact: true }).click();
    await parent.getByRole('textbox').fill(body + ' edited'); await parent.getByRole('button', { name: 'Publicar', exact: true }).click();
    await expect(parent.getByText(body + ' edited', { exact: true })).toBeVisible();
    await parent.getByRole('button', { name: 'Opciones del comentario' }).click(); await a.page.getByRole('menuitem', { name: 'Eliminar mi comentario' }).click();
    await a.page.getByRole('dialog').getByRole('button', { name: /Confirmar|Eliminar/ }).click();
    await expect(parent.getByText('Comentario eliminado', { exact: true })).toBeVisible();
    await expect(a.page.getByText(replyBody, { exact: true })).toBeVisible();
  } finally { await a.context.close(); await b.context.close(); }
});

test('publication links select a later catalog page and focus its media @critical', async ({ page }) => {
  const ids = Array.from({ length: 24 }, (_, index) => `92500000-0000-4000-8000-${String(index + 1).padStart(12, '0')}`);
  const source = ids.map((id, index) => ({ id, title: `Linked catalog item ${index}`, sortOrder: index,
    contributors: [], resources: [{ id, kind: 'video', primary: true, providerCode: 'youtube', externalCode: `video${index}`,
      url: `https://www.youtube.com/watch?v=video${index}`, availability: 'available', providerMetadata: {}, thumbnailUrl: 'https://example.test/image.jpg' }] }));
  await page.route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url());
    if (url.pathname === '/records/feed') return route.fulfill({ json: { recordings: source, sessions: source, releases: source } });
    if (['fetch', 'xhr'].includes(request.resourceType())) return route.fulfill({ status: 404, json: {} });
    if (url.hostname !== '127.0.0.1' && url.hostname !== 'localhost') return route.abort();
    return route.continue();
  });
  for (const parameter of ['recording', 'session', 'release']) {
    await page.goto(`/records?${parameter}=${ids[17]}`);
    const card = page.locator('.MuiCard-root[tabindex="-1"]').filter({ has: page.getByText('Linked catalog item 17', { exact: true }) });
    await expect(card).toHaveCount(1);
    await expect(card).toBeFocused();
    await expect(card).toBeInViewport();
    await expect(card.getByText('Linked catalog item 17', { exact: true })).toBeVisible();
    await page.goto(`/records?${parameter}=missing`);
    await expect(page.getByText('Esta publicación ya no está disponible o no tienes acceso.')).toBeVisible();
  }
});

test('record destinations remain reachable outside the feed window or without a preview @critical', async ({ page, context, request }) => {
  const fixtures = [];
  for (const [kind, parameter, table] of [['recording', 'recording', 'recording'], ['recording_session', 'session', 'recording_session'], ['record_release', 'release', 'record_release']]) {
    const id = execFileSync('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', fixture.database, '-c',
      `SELECT id FROM ${table} WHERE interaction_resolve('${kind}',id::text,NULL) IS NOT NULL ORDER BY id LIMIT 1;`], { encoding: 'utf8' }).trim();
    expect(id).toBeTruthy();
    const response = await request.get(`${fixture.base}/public/interactions/targets/${kind}/${id}`);
    expect(response.status()).toBe(200);
    fixtures.push({ kind, parameter, id, summary: await response.json() });
  }
  const others = Array.from({ length: 200 }, (_, i) => ({ id: `92510000-0000-4000-8000-${String(i).padStart(12, '0')}`,
    title: `Feed preview ${i}`, sortOrder: i, contributors: [], resources: [{ kind: 'video', primary: true,
      providerCode: 'youtube', externalCode: `fixture${i}`, url: 'https://example.test/video', providerMetadata: {} }] }));
  let feed; const summaryLookups = [];
  await page.route('**/*', async route => {
    const incoming = route.request(); const url = new URL(incoming.url());
    if (url.pathname === '/records/feed') return route.fulfill({ json: feed });
    if (url.pathname.startsWith('/public/interactions/')) {
      summaryLookups.push(url.pathname);
      const response = await context.request.get(fixture.base + url.pathname + url.search);
      return route.fulfill({ response });
    }
    if (['fetch', 'xhr'].includes(incoming.resourceType())) return route.fulfill({ status: 404, json: {} });
    if (!['127.0.0.1', 'localhost'].includes(url.hostname)) return route.abort();
    return route.continue();
  });
  for (const source of fixtures) {
    for (const scenario of source.kind === 'record_release' ? ['window'] : ['window', 'no-preview']) {
      const items = scenario === 'window' ? others : [{ id: source.id, title: source.summary.title, sortOrder: 0, contributors: [], resources: [] }];
      feed = { recordings: source.kind === 'recording' ? items : [], sessions: source.kind === 'recording_session' ? items : [], releases: source.kind === 'record_release' ? items : [] };
      summaryLookups.length = 0;
      await page.goto(`/records?${source.parameter}=${source.id}`);
      const card = page.locator('.MuiCard-root[tabindex="-1"]').filter({ has: page.getByRole('heading', { name: source.summary.title, exact: true }) });
      await expect(card).toBeVisible(); await expect(card).toBeFocused(); await expect(card).toBeInViewport();
      await expect(card.getByRole('region', { name: 'Reacciones y conversación' })).toBeVisible();
      expect(summaryLookups.filter(path => path === `/public/interactions/targets/${source.kind}/${source.id}`).length).toBeLessThanOrEqual(3);
      await expect(page.getByText('Esta publicación ya no está disponible o no tienes acceso.')).toHaveCount(0);
    }
  }
  const selected = page.locator('.MuiCard-root[tabindex="-1"]');
  const disclosure = selected.getByRole('region', { name: 'Reacciones y conversación' }).locator('button[aria-controls]').first();
  await disclosure.focus(); await page.keyboard.press('Enter'); await expect(disclosure).toHaveAttribute('aria-expanded', 'false');
  await page.keyboard.press('Enter'); await expect(disclosure).toHaveAttribute('aria-expanded', 'true');
  await page.addScriptTag({ content: axe.source });
  const violations = await selected.evaluate(async node => (await window.axe.run(node)).violations.filter(v => ['serious', 'critical'].includes(v.impact)));
  expect(violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.target) }))).toEqual([]);
});

test('a refreshed record feed keeps the linked publication on its current page @critical', async ({ page, context, request }) => {
  const id = execFileSync('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', fixture.database, '-c',
    "SELECT id FROM record_release WHERE interaction_resolve('record_release',id::text,NULL) IS NOT NULL ORDER BY id LIMIT 1;"], { encoding: 'utf8' }).trim();
  const response = await request.get(`${fixture.base}/public/interactions/targets/record_release/${id}`);
  expect(response.status()).toBe(200); const summary = await response.json();
  const releases = Array.from({ length: 12 }, (_, i) => ({ id: i === 5 ? id : `92520000-0000-4000-8000-${String(i).padStart(12, '0')}`,
    title: i === 5 ? summary.title : `Refresh fixture ${i}`, sortOrder: i + 1, contributors: [], resources: [] }));
  let feed = { recordings: [], sessions: [], releases };
  await page.route('**/*', async route => {
    const incoming = route.request(); const url = new URL(incoming.url());
    if (url.pathname === '/records/feed') return route.fulfill({ json: feed });
    if (url.pathname.startsWith('/public/interactions/')) return route.fulfill({ response: await context.request.get(fixture.base + url.pathname + url.search) });
    if (['fetch', 'xhr'].includes(incoming.resourceType())) return route.fulfill({ status: 404, json: {} });
    if (!['127.0.0.1', 'localhost'].includes(url.hostname)) return route.abort();
    return route.continue();
  });
  await page.clock.install();
  await page.goto(`/records?release=${id}`);
  const card = page.locator('.MuiCard-root[tabindex="-1"]').filter({ has: page.getByRole('heading', { name: summary.title, exact: true }) });
  await expect(card).toBeFocused();
  for (const insert of [true, false]) {
    feed = { recordings: [], sessions: [], releases: insert ? [{ ...releases[0], id: '92520000-0000-4000-8000-999999999999', title: 'New first release', sortOrder: 0 }, ...releases] : releases };
    await page.clock.fastForward(6 * 60 * 1000);
    const refreshed = page.waitForResponse(url => new URL(url.url()).pathname === '/records/feed');
    await page.evaluate(() => { window.dispatchEvent(new Event('offline')); window.dispatchEvent(new Event('online')); });
    await refreshed;
    // Wait for the new page contents, not just the response or the previously focused card.
    await expect(page.getByRole('heading', { name: insert ? 'Refresh fixture 6' : 'Refresh fixture 0', exact: true })).toBeVisible();
    await expect(card).toBeFocused(); await expect(card).toBeInViewport();
    await expect(card.getByRole('region', { name: 'Reacciones y conversación' })).toBeVisible();
  }
  await page.unrouteAll({ behavior: 'wait' });
});
