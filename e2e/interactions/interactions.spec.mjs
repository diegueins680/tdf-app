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
