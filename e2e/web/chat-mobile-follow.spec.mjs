import { expect, test } from '@playwright/test';

const messageBodyLocator = (page, text) => page.getByText(text, { exact: true }).and(page.locator('p'));

for (const kind of ['incoming', 'outgoing']) {
  test(`mobile chat follows ${kind} messages inside its own viewport @critical @mobile-flow`, async ({ page, baseURL }, testInfo) => {
    if (page.viewportSize().width >= 900) await page.setViewportSize({ width: 390, height: 844 });
    const base = new URL(baseURL).origin;
    const thread = { ctThreadId: 10, ctOtherPartyId: 2, ctOtherDisplayName: 'Synthetic peer', ctLastMessage: 'Initial message', ctLastMessageAt: '2026-11-07T10:00:00Z', ctUpdatedAt: '2026-11-07T10:00:00Z' };
    const initial = { cmId: 10, cmThreadId: 10, cmSenderPartyId: 2, cmBody: 'Initial message', cmCreatedAt: '2026-11-07T10:00:00Z' };
    let incoming = false;
    let stored = [initial];
    let messageReads = 0;
    let sends = 0;
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.context().route('**/*', async route => {
      const req = route.request(); const url = new URL(req.url()); const path = url.pathname;
      if (!['fetch', 'xhr'].includes(req.resourceType())) return url.origin === base ? route.continue() : route.abort();
      if (path === '/session') return route.fulfill({ json: { username: 'synthetic', displayName: 'Synthetic owner', partyId: 5, roles: ['Fan'], modules: ['Catalog'], featureFlags: [] } });
      if (path === '/session/preferences') return route.fulfill({ json: { localeId: '', locale: 'es', currencyId: '', currency: 'USD', timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC' } });
      if (path === '/session/onboarding') return route.fulfill({ json: { eligible: false, completedAt: '2026-11-07T10:00:00Z' } });
      if (path === '/health') return route.fulfill({ json: { status: 'ok' } });
      if (path === '/chat/threads') return route.fulfill({ json: [thread] });
      if (path === '/chat/threads/10/messages') {
        if (req.method() === 'POST') {
          sends += 1;
          const body = req.postDataJSON();
          const message = { ...initial, cmId: 14, cmSenderPartyId: 5, cmBody: body.body ?? body.cmBody ?? 'Mobile outgoing tail' };
          stored = [...stored, message]; return route.fulfill({ json: message });
        }
        messageReads += 1;
        return route.fulfill({ json: incoming ? [...stored, { ...initial, cmId: 11, cmBody: 'Overflow content. '.repeat(300) }, { ...initial, cmId: 12, cmBody: 'Mobile incoming tail' }] : stored });
      }
      if (path.startsWith('/social/profiles/')) return route.fulfill({ json: { sppPartyId: 2, sppDisplayName: 'Synthetic peer', sppBio: '', sppAvatarUrl: null } });
      if (path === '/fans/me/notifications/count') return route.fulfill({ json: { ncUnread: 0 } });
      if (path.startsWith('/radio/presence')) return route.fulfill({ json: null });
      if (path.endsWith('/rsvp-feed')) return route.fulfill({ json: { feedItems: [], feedNextCursor: null } });
      if (path.startsWith('/catalog')) return route.fulfill({ json: { catalogs: [], items: [], defaults: [] } });
      return route.fulfill({ json: [] });
    });
    const geometry = async text => messageBodyLocator(page, text).evaluate(node => {
      let viewport = node.parentElement;
      while (viewport && viewport.id !== 'main-content' && getComputedStyle(viewport).overflowY !== 'auto') viewport = viewport.parentElement;
      if (!viewport || viewport.id === 'main-content') throw new Error('Message body must belong to its own viewport');
      const rect = node.getBoundingClientRect(); const main = document.getElementById('main-content'); const m = main.getBoundingClientRect();
      return { visible: rect.top >= Math.max(0, m.top) && rect.bottom <= Math.min(innerHeight, m.bottom), main: main.scrollTop, top: viewport.scrollTop, height: viewport.clientHeight, total: viewport.scrollHeight, width: innerWidth };
    });
    await page.goto(`${base}/chat`);
    await expect(page.getByText('Initial message', { exact: true }).last()).toBeVisible();
    const body = page.getByText('Initial message', { exact: true }).last();
    await body.scrollIntoViewIfNeeded();
    // The response arrives after the one-time explicit reveal, without another navigation.
    const mainBefore = await page.locator('#main-content').evaluate(node => node.scrollTop);
    if (kind === 'incoming') {
      const before = messageReads; incoming = true;
      await expect.poll(() => messageReads).toBeGreaterThan(before);
      await expect.poll(async () => { const state = await geometry('Mobile incoming tail'); return state.visible && state.total > state.height && state.top + state.height >= state.total - 2; }).toBe(true);
      const after = await geometry('Mobile incoming tail');
      expect(after.total).toBeGreaterThan(after.height); expect(after.top + after.height).toBeGreaterThanOrEqual(after.total - 2);
      expect(after.main).toBe(mainBefore);
      await testInfo.attach('mobile-message-follow.json', { body: JSON.stringify({ mainBefore, after }), contentType: 'application/json' });
    } else {
      // Start with overflow while the selected thread is already revealed.
      stored = [initial, { ...initial, cmId: 13, cmBody: 'Existing conversation. '.repeat(300) }];
      const before = messageReads;
      await expect.poll(() => messageReads).toBeGreaterThan(before);
      await page.getByRole('textbox', { name: 'Escribe un mensaje' }).fill('Mobile outgoing tail');
      await page.getByRole('button', { name: 'Enviar mensaje', exact: true }).click();
      await expect.poll(() => sends).toBe(1);
      await expect.poll(async () => { const state = await geometry('Mobile outgoing tail'); return state.visible && state.total > state.height && state.top + state.height >= state.total - 2; }).toBe(true);
      const after = await geometry('Mobile outgoing tail');
      expect(after.total).toBeGreaterThan(after.height); expect(after.top + after.height).toBeGreaterThanOrEqual(after.total - 2);
      await testInfo.attach('mobile-message-follow.json', { body: JSON.stringify({ after }), contentType: 'application/json' });
    }
    expect(errors).toEqual([]);
  });
}


test('a pending composer draft is not a delivered message body @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  if (page.viewportSize().width >= 900) await page.setViewportSize({ width: 390, height: 844 });
  const base = new URL(baseURL).origin;
  const marker = 'Pending composer is not a delivered message';
  const initial = { cmId: 10, cmThreadId: 10, cmSenderPartyId: 2, cmBody: 'Initial message', cmCreatedAt: '2026-11-07T10:00:00Z' };
  const thread = { ctThreadId: 10, ctOtherPartyId: 2, ctOtherDisplayName: 'Synthetic peer', ctLastMessage: initial.cmBody, ctLastMessageAt: initial.cmCreatedAt, ctUpdatedAt: initial.cmCreatedAt };
  const errors = [];
  let stored = [initial]; let postStarted = false; let releasePost;
  const pendingPost = new Promise(resolve => { releasePost = resolve; });
  page.on('pageerror', error => errors.push(error.message));
  await page.context().route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url()); const path = url.pathname;
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === base ? route.continue() : route.abort();
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic', displayName: 'Synthetic owner', partyId: 5, roles: ['Fan'], modules: ['Catalog'], featureFlags: [] } });
    if (path === '/session/preferences') return route.fulfill({ json: { localeId: '', locale: 'es', currencyId: '', currency: 'USD', timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC' } });
    if (path === '/session/onboarding') return route.fulfill({ json: { eligible: false, completedAt: initial.cmCreatedAt } });
    if (path === '/health') return route.fulfill({ json: { status: 'ok' } });
    if (path === '/chat/threads') return route.fulfill({ json: [thread] });
    if (path === '/chat/threads/10/messages') {
      if (request.method() === 'POST') {
        postStarted = true;
        const body = request.postDataJSON();
        expect(body.csmBody).toBe(marker);
        await pendingPost;
        const message = { ...initial, cmId: 14, cmSenderPartyId: 5, cmBody: body.csmBody };
        stored = [...stored, message];
        return route.fulfill({ json: message });
      }
      return route.fulfill({ json: stored });
    }
    if (path.startsWith('/social/profiles/')) return route.fulfill({ json: { sppPartyId: 2, sppDisplayName: 'Synthetic peer', sppBio: '', sppAvatarUrl: null } });
    if (path === '/fans/me/notifications/count') return route.fulfill({ json: { ncUnread: 0 } });
    if (path.startsWith('/radio/presence')) return route.fulfill({ json: null });
    if (path.endsWith('/rsvp-feed')) return route.fulfill({ json: { feedItems: [], feedNextCursor: null } });
    if (path.startsWith('/catalog')) return route.fulfill({ json: { catalogs: [], items: [], defaults: [] } });
    return route.fulfill({ json: [] });
  });
  try {
    await page.goto(`${base}/chat`);
    await expect(page.getByText('Initial message', { exact: true }).last()).toBeVisible();
    await page.getByRole('textbox', { name: 'Escribe un mensaje' }).fill(marker);
    await page.getByRole('button', { name: 'Enviar mensaje', exact: true }).click();
    await expect.poll(() => postStarted).toBe(true);
    // React retains the pending draft as textarea text. It cannot qualify as a
    // rendered message while the real send response is deliberately withheld.
    await expect(page.getByText(marker, { exact: true })).toHaveCount(1);
    expect(await page.getByText(marker, { exact: true }).evaluate(node => node.tagName)).toBe('TEXTAREA');
    await expect(messageBodyLocator(page, marker)).toHaveCount(0);
  } finally {
    releasePost();
  }
  await expect(messageBodyLocator(page, marker)).toBeVisible();
  expect(await messageBodyLocator(page, marker).evaluate(node => node.tagName)).toBe('P');
  await testInfo.attach('pending-message-locator.json', { body: JSON.stringify({ withheldResponseRejectedComposer: true, canonicalRequestField: 'csmBody' }), contentType: 'application/json' });
  expect(errors).toEqual([]);
});
