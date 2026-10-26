import assert from 'node:assert/strict';
import {chromium, devices, expect} from '/private/tmp/tdf-music-current-main-integration-20261023/node_modules/@playwright/test/index.mjs';
import axe from '/private/tmp/tdf-music-current-main-integration-20261023/node_modules/axe-core/axe.js';
import {readFileSync,writeFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
const base='https://fc0026d2.tdf-app.pages.dev';
const source='c219279b33ce13337f71ddee8d14171f5adfc069';
const dir='/private/tmp/tdf-studio-gallery-20261026-private/all-photos';
const manifest=JSON.parse(readFileSync('/private/tmp/tdf-studio-photo-gallery-20261026/reports/studio-gallery-2026-10-26.json'));
const sha=body=>createHash('sha256').update(body).digest('hex');
const browser=await chromium.launch({headless:true});const cases=[];
try {
 for(const [mode,options] of [['desktop-light',{viewport:{width:1280,height:720},colorScheme:'light'}],['Pixel7-dark',{...devices['Pixel 7'],colorScheme:'dark'}]]) {
  const context=await browser.newContext({...options,locale:'es-EC',serviceWorkers:'block'});
  const errors=[],writes=[],blockedExternal=[],assets=new Map();let injectFailure=false,failedRequests=0;
  await context.addInitScript(()=>localStorage.setItem('tdf-hq-ui/locale','es'));
  await context.route('**/*',async route=>{
   const req=route.request(),u=new URL(req.url()),path=u.pathname.replace(/^\/api(?=\/)/,'');
   if(!['fetch','xhr'].includes(req.resourceType())) {
    if(u.origin!==base) {blockedExternal.push({host:u.hostname,type:req.resourceType()});return route.abort();}
    if(req.method()!=='GET'){writes.push({path:u.pathname,method:req.method()});return route.abort();}
    if(injectFailure&&path==='/assets/tdf-studio/recording-room-1280.webp') {injectFailure=false;failedRequests++;return route.fulfill({status:503,body:'Synthetic image outage'});}
    return route.continue();
   }
   // Classify external analytics before business methods. No external request
   // reaches its destination; only preview documents/static assets use network.
   if(u.origin!==base&&!/(^|\.)tdfrecords\.net$/.test(u.hostname)){blockedExternal.push({host:u.hostname,type:req.resourceType()});return route.abort();}
   if(req.method()!=='GET'){writes.push({path,method:req.method()});return route.abort();}
   if(path==='/session')return route.fulfill({json:null});
   if(path==='/health')return route.fulfill({json:{status:'ok',db:'ok'}});
   if(path==='/directory/taxonomies')return route.fulfill({json:{locale:'es',cities:[],professions:[],classifiedCategories:[],compensationTypes:[],serviceOfferings:[],currencies:[],instruments:[],genres:[]}});
   if(path==='/directory/search')return route.fulfill({json:{items:[],sponsoredItems:[],facets:{total:0,entityTypes:{},cities:[]},nextCursor:null}});
   if(path==='/fans/artists'||path==='/directory/suggestions')return route.fulfill({json:[]});
   if(path==='/records/feed')return route.fulfill({json:{collections:[],releases:[],recordings:[],sessions:[]}});
   if(path.startsWith('/catalogs/'))return route.fulfill({json:{items:[],catalogs:[],defaults:[]}});
   return route.fulfill({status:404,json:{error:'Unknown synthetic API'}});
  });
  const page=await context.newPage();page.on('pageerror',error=>errors.push(error.message));
  page.on('response',response=>{if(response.url().startsWith(base+'/assets/'))assets.set(response.url(),response);});
  await page.goto(base+'/inicio',{waitUntil:'domcontentloaded'});
  const welcome=page.getByRole('region',{name:'Bienvenido a TDF Records'});await expect(welcome).toBeVisible();
  const entry=await page.locator('script[type="module"][src]').first().getAttribute('src');
  const entryResponse=assets.get(new URL(entry,base).href);assert.ok(entryResponse);const entryBytes=await entryResponse.body();assert.ok(entryBytes.toString().includes(source));
  await welcome.getByRole('link',{name:'Conoce TDF y sus servicios',exact:true}).click();
  await page.getByRole('link',{name:'Conoce el estudio',exact:true}).click();
  const gallery=page.getByRole('group',{name:'Fotos del estudio TDF'});await gallery.scrollIntoViewIfNeeded();
  const images=gallery.getByRole('img');await expect(images).toHaveCount(6);
  for(const image of await images.all()){await image.scrollIntoViewIfNeeded();await expect.poll(()=>image.evaluate(i=>i.complete&&i.naturalWidth===640)).toBe(true);}
  const verified=[];
  for(const photo of manifest.selection){const asset=photo.assets.find(a=>a.path.includes('-640.'));const url=base+asset.path.replace('tdf-hq-ui/public','');const response=assets.get(url);assert.ok(response);assert.equal(sha(await response.body()),asset.sha256);verified.push(photo.name);}
  assert.equal([...assets.keys()].filter(url=>url.includes('/assets/tdf-studio/')&&url.includes('-1280.')).length,0);
  await gallery.screenshot({path:dir+'/preview-gallery-'+mode+'.png'});
  const trigger=gallery.getByRole('button',{name:'Ampliar foto: Sala de control',exact:true});await trigger.focus();await trigger.press('Enter');
  let dialog=page.getByRole('dialog',{name:'Sala de control',exact:true});await expect(dialog).toBeVisible();
  await expect.poll(()=>dialog.getByRole('img').evaluate(i=>i.complete&&i.naturalWidth===1280)).toBe(true);
  await page.keyboard.press('Escape');await expect(dialog).toHaveCount(0);await expect(trigger).toBeFocused();
  const completePhotos=[];
  for(const photo of manifest.selection){
   const asset=photo.assets.find(a=>a.path.includes('-1280.'));
   const item=gallery.locator('button').filter({has:page.locator(`img[src$="${photo.name}-640.webp"]`)});
   await item.focus();await item.press('Enter');
   const full=page.getByRole('dialog').getByRole('img');
   await expect.poll(()=>full.evaluate(i=>i.complete&&i.naturalWidth===1280)).toBe(true);
   const response=assets.get(base+asset.path.replace('tdf-hq-ui/public',''));assert.ok(response);assert.equal(sha(await response.body()),asset.sha256);
   await page.keyboard.press('Escape');await expect(page.getByRole('dialog')).toHaveCount(0);await expect(item).toBeFocused();completePhotos.push(photo.name);
  }
  writeFileSync(dir+'/complete-photos-'+mode+'.json',JSON.stringify({mode,completePhotos,allOriginalDerivativesMatch:true},null,2));
  injectFailure=true;
  await gallery.getByRole('button',{name:'Ampliar foto: Sala de grabación',exact:true}).click();
  dialog=page.getByRole('dialog',{name:'Sala de grabación',exact:true});await expect(dialog.getByRole('alert')).toContainText('No se pudo cargar esta foto');
  await dialog.getByRole('button',{name:'Reintentar foto',exact:true}).click();
  await expect.poll(()=>dialog.getByRole('img').evaluate(i=>i.complete&&i.naturalWidth===1280)).toBe(true);assert.equal(failedRequests,1);
  await dialog.getByRole('button',{name:'Cerrar foto',exact:true}).click();
  await page.setViewportSize({width:320,height:740});await trigger.scrollIntoViewIfNeeded();assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await trigger.click();await expect(page.getByRole('dialog',{name:'Sala de control',exact:true})).toBeVisible();
  await page.addScriptTag({content:axe.source});
  const violations=await page.evaluate(async()=>{const r=await globalThis.axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}});return r.violations.filter(v=>['serious','critical'].includes(v.impact)).map(v=>({id:v.id,impact:v.impact}));});assert.deepEqual(violations,[]);
  await page.getByRole('dialog').screenshot({path:dir+'/preview-photo-320-'+mode+'.png'});
  await page.getByRole('button',{name:'Cerrar foto',exact:true}).click();await expect(trigger).toBeFocused();assert.deepEqual(errors,[]);assert.deepEqual(writes,[]);
  cases.push({mode,source,entry,entrySha256:sha(entryBytes),thumbnailHashesVerified:verified,initialEnlargedRequests:0,syntheticImageFailures:failedRequests,retryPassed:true,keyboardFocusPassed:true,overflow320:false,seriousCriticalAxeViolations:violations,businessWrites:0,pageErrors:errors,blockedExternal,passed:true});await context.close();
 }
} finally {await browser.close();writeFileSync(dir+'/preview-results.json',JSON.stringify({observedAt:new Date().toISOString(),base,scope:'Actual Cloudflare preview static artifact; all business/session APIs synthetic, external requests blocked. Not production, human usability or provider verification.',cases},null,2));}
console.log(JSON.stringify(cases));
