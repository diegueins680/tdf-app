import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const catalogId = '10000000-0000-4000-8000-000000000001';
const config = { ios: { status: 'testflight_external', url: 'https://testflight.apple.com/join/7k3VE2JJ', capacity: 'available', verifiedAt: new Date(Date.now()-60000).toISOString(), validUntil: new Date(Date.now()+86400000).toISOString() }, android: { status: 'unavailable', verifiedAt: '2026-01-01', validUntil: '2027-01-01' } };
async function fixture(page, baseURL, distribution = config) {
  const origin = new URL(baseURL).origin;
  const submissions = [];
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch','xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/mobile-distribution.json') return route.fulfill({ json: distribution });
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/feedback' && request.method() === 'POST') { submissions.push(request.postData()); return route.fulfill({ status: 204 }); }
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: ['feedback-categories','feedback-severities'].map(code => ({ catalog: {code}, items: (code === 'feedback-categories' ? ['bug','idea','ux'] : ['p2','p4']).map((itemCode, index) => ({ id: index === 0 ? catalogId : catalogId.slice(0,-1) + String(index + 1), code: itemCode, active: true, workflowState: 'published', name: itemCode })), defaults: [{scopeKind: code === 'feedback-categories' ? 'feedback-category' : 'feedback-severity', scopeId:'global', entityId:catalogId}] })) } });
    return route.fulfill({ json: [] });
  });
  return submissions;
}
for (const width of [390, 834, 1280]) {
  test(`Mobile tester funnel ${width}px @critical`, async ({ page, baseURL }) => {
    await page.setViewportSize({width,height:900});
    const submissions = await fixture(page,baseURL);
    await page.goto('/app?utm_source=instagram&utm_campaign=tu_escena_conectada');
    await expect(page.getByRole('heading', {name:'TDF Mobile',exact:true})).toBeVisible();
    await page.getByRole('button',{name:'Android',exact:true}).click();
    await page.getByRole('button',{name:'Solicitar acceso',exact:true}).click();
    await page.getByRole('textbox',{name:'Correo para coordinar el acceso'}).fill('synthetic@example.test');
    await page.getByRole('checkbox').check();
    await page.getByRole('button',{name:'Enviar',exact:true}).click();
    await expect(page.getByRole('status')).toContainText('Solicitud recibida');
    expect(submissions.length).toBe(1);
    await page.getByRole('button',{name:'iPhone / iOS',exact:true}).click();
    await expect(page.getByRole('link',{name:'Probar beta en TestFlight'})).toHaveAttribute('href',config.ios.url);
    await page.getByRole('button',{name:'Ya estoy probando: enviar feedback'}).click();
    const attachment = page.getByRole('button', {name: /Adjuntar captura/});
    await attachment.focus();
    const chooserPromise = page.waitForEvent('filechooser');
    await page.keyboard.press('Enter');
    const chooser = await chooserPromise;
    expect(chooser.isMultiple()).toBe(false);
    await page.getByRole('textbox',{name:'Cuéntanos qué ocurrió'}).fill('Synthetic feedback: hard to find the player.');
    await page.getByRole('checkbox').check();
    await page.getByRole('button',{name:'Enviar',exact:true}).click();
    await expect(page.getByRole('status')).toContainText('Tu comentario fue recibido');
    expect(submissions.length).toBe(2);
    await page.getByRole('button',{name:'English',exact:true}).click();
    await expect(page.getByRole('link',{name:'Try the beta on TestFlight'})).toBeVisible();
    await page.addScriptTag({content:axe.source});
    const violations = await page.evaluate(async()=> (await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}})).violations.map(({id,nodes})=>({id,targets:nodes.map(n=>n.target)})));
    expect(violations).toEqual([]);
    expect(await page.evaluate(()=>document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await page.evaluate(()=>{document.documentElement.style.fontSize='200%';});
    expect(await page.evaluate(()=>document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await page.getByRole('button',{name:'English',exact:true}).focus(); await page.keyboard.press('Tab');
    expect(await page.evaluate(()=>document.activeElement?.tagName)).toBe('BUTTON');
  });
}
test('Dismissed mobile promotion stays dismissed @critical',async({page,baseURL})=>{
  await fixture(page,baseURL); await page.addInitScript(()=>Object.defineProperty(navigator,'userAgent',{value:'Android',configurable:true}));
  await page.goto('/records');
  const banner=page.getByRole('complementary',{name:'TDF Mobile'}).filter({has:page.getByRole('button',{name:'Ahora no'})});
  await banner.getByRole('button',{name:'Ahora no'}).click(); await page.reload();
  await expect(page.getByRole('button',{name:'Ahora no'})).toHaveCount(0);
});


test('Admitted Android testers can reach Play without submitting another form @critical', async ({ page, baseURL }) => {
  const closed = { ...config, android: { ...config.ios, status: 'closed_testing', admission: 'approval_required', url: 'https://play.google.com/apps/testing/com.tdf.records' } };
  const submissions = await fixture(page, baseURL, closed);
  await page.goto('/app');
  await page.getByRole('button', { name: 'Android', exact: true }).click();
  const admitted = page.getByRole('link', { name: 'Ya tengo acceso: abrir Google Play' });
  await expect(admitted).toBeVisible();
  await expect(admitted).toHaveAttribute('href', closed.android.url);
  await expect(page.getByRole('button', { name: 'Solicitar acceso', exact: true })).toBeVisible();
  await expect(page.getByText(/Solicita acceso con la cuenta de Google/)).toBeVisible();
  await expect(page.getByRole('textbox', { name: /Correo/ })).toHaveCount(0);
  expect(submissions).toEqual([]);
  await page.getByRole('button', { name: 'Solicitar acceso', exact: true }).focus();
  await page.keyboard.press('Tab');
  await expect(admitted).toBeFocused();
  expect(await admitted.evaluate(el => getComputedStyle(el).outlineStyle)).not.toBe('none');
  expect((await admitted.boundingBox()).height).toBeGreaterThanOrEqual(44);
  await page.addScriptTag({ content: axe.source });
  expect(await page.evaluate(async () => (await axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => v.id))).toEqual([]);
  await page.evaluate(() => { document.documentElement.style.fontSize = '200%'; });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await page.getByRole('button', { name: 'English', exact: true }).click();
  await expect(page.getByRole('link', { name: 'I already have access: open Google Play' })).toBeVisible();
});
