import assert from 'node:assert/strict';
import {chromium,devices,expect} from '/private/tmp/tdf-music-current-main-integration-20261023/node_modules/@playwright/test/index.mjs';
import {writeFileSync} from 'node:fs';
const base='https://b669cfb9.tdf-app.pages.dev';
const browser=await chromium.launch({headless:true}); const observations=[];
try {for(const [mode,options] of [['desktop',{viewport:{width:1280,height:800}}],['Pixel7',devices['Pixel 7']]]) {
 const ctx=await browser.newContext({...options,locale:'es-EC',serviceWorkers:'block'});let catalogCalls=0;const writes=[],errors=[];
 await ctx.route('**/*',async route=>{const q=route.request(),u=new URL(q.url()),path=u.pathname.replace(/^\/api(?=\/)/,'');
 if(!['xhr','fetch'].includes(q.resourceType()))return u.origin===base&&q.method()==='GET'?route.continue():route.abort();
 if(u.origin!==base&&!/(^|\.)tdfrecords\.net$/.test(u.hostname))return route.abort();
 if(q.method()!=='GET'){writes.push({path,method:q.method()});return route.abort();}
 if(path==='/services/catalog/public'){catalogCalls++;return route.fulfill({status:503,json:{error:'Synthetic service catalog outage'}});}
 if(path==='/session')return route.fulfill({json:null});
 if(path==='/health')return route.fulfill({json:{status:'ok',db:'ok'}});
 if(path==='/fans/artists'||path.includes('engineers'))return route.fulfill({json:[]});
 if(path==='/records/feed')return route.fulfill({json:{collections:[],releases:[],recordings:[],sessions:[]}});
 if(path.includes('/reviews'))return route.fulfill({json:{items:[],reviews:[],summary:null,nextCursor:null}});
 return route.fulfill({status:404,json:{error:'Synthetic API not used'}});
 });
 const page=await ctx.newPage();page.on('pageerror',e=>errors.push(e.message));
 await page.goto(base+'/tdf#estudio',{waitUntil:'domcontentloaded'});
 const studio=page.locator('#estudio');await expect(studio.getByRole('group',{name:'Fotos del estudio TDF'})).toBeVisible();
 const booking=studio.locator('a[href="/reservar"]');await expect(booking).toBeVisible();await booking.click();await expect(page).toHaveURL(/\/reservar$/);
 const warning=page.getByRole('alert').filter({hasText:'No pudimos cargar el catálogo de servicios'});await expect(warning).toBeVisible({timeout:30000});
 await expect(page.getByRole('textbox',{name:'Nombre completo',exact:true})).toBeDisabled();
 const actions=await warning.getByRole('button').count();assert.equal(actions,0);assert.deepEqual(writes,[]);assert.deepEqual(errors,[]);
 await warning.screenshot({path:'/private/tmp/tdf-studio-booking-catalog-20261026/catalog-unavailable-'+mode+'.png'});
 observations.push({mode,path:'/tdf#estudio -> /reservar',catalogCalls,contactFormDisabled:true,warningActions:actions,warning:await warning.innerText(),pageErrors:errors,businessWrites:writes,observedAt:new Date().toISOString()});await ctx.close();
}} finally {await browser.close();writeFileSync('/private/tmp/tdf-studio-booking-catalog-20261026/before.json',JSON.stringify({scope:'Actual preview static artifact, simulated API503 only. No real reservation or provider calls.',observations},null,2));}
console.log(JSON.stringify(observations));
