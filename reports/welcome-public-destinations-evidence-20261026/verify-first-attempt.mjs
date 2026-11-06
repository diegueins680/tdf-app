import assert from 'node:assert/strict';
import {chromium,devices,expect} from '/private/tmp/tdf-music-current-main-integration-20261023/node_modules/@playwright/test/index.mjs';
import {writeFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
const base='https://www.tdfrecords.net',api='https://api.tdfrecords.net',out='/private/tmp/tdf-welcome-records-live-20261026';
const report={startedAt:new Date().toISOString(),scope:'Anonymous read-only production destination verification. No signup, authentication, follow, booking, provider playback or other business mutation. External images/embeds blocked; media playback and thumbnail availability are not verified.',cases:[],status:'running'};
const save=()=>writeFileSync(out+'/results.json',JSON.stringify(report,null,2)+'\n');save();const browser=await chromium.launch({headless:true});
try{
 for(const[mode,options]of[['desktop',{viewport:{width:1440,height:1000}}],['Pixel7',devices['Pixel 7']]]){
  const context=await browser.newContext({...options,locale:'es-EC',reducedMotion:'reduce',serviceWorkers:'block'});const writes=[],errors=[],blocked=[],entries=[];
  try{
   await context.route('**/*',route=>{const req=route.request(),url=new URL(req.url());if(!['GET','HEAD','OPTIONS'].includes(req.method())){writes.push({method:req.method(),path:url.pathname});return route.abort('blockedbyclient');}if(![base,api].includes(url.origin)){blocked.push({type:req.resourceType(),origin:url.origin});return route.abort('blockedbyclient');}return route.continue();});
   const anonymous=await context.request.get(api+'/session');assert.equal(anonymous.status(),200);assert.equal(await anonymous.json(),null);
   const page=await context.newPage();page.on('pageerror',e=>errors.push(e.message));page.on('response',response=>{if(response.url().startsWith(base+'/assets/index-')&&response.url().endsWith('.js'))entries.push(response);});
   const feedPromise=page.waitForResponse(r=>r.url().startsWith(api+'/records/feed')&&r.request().method()==='GET');
   await page.goto(base+'/records',{waitUntil:'domcontentloaded'});const feedResponse=await feedPromise;assert.equal(feedResponse.status(),200);const feed=await feedResponse.json();assert.ok(feed.releases.length>0&&feed.recordings.length>0&&feed.sessions.length>0);
   const session=feed.sessions[0];assert.ok(session.title);const link=page.getByRole('link',{name:session.title,exact:true}).first();await expect(link).toBeVisible();const href=await link.getAttribute('href');assert.ok(/^https:\/\/(www\.)?(youtube\.com|youtu\.be)\//.test(href));
   await page.getByRole('link',{name:'Ver lanzamientos',exact:true}).click();await expect(page).toHaveURL(base+'/records#releases');await expect(page.locator('#releases')).toBeVisible();await expect(page.locator('input[type=password]')).toHaveCount(0);assert.deepEqual(writes,[]);assert.deepEqual(errors,[]);
   await page.screenshot({path:out+'/records-'+mode+'.png',fullPage:false});
   const entry=entries[0];assert.ok(entry);const bytes=await entry.body();report.cases.push({mode,url:page.url(),feedStatus:feedResponse.status(),counts:{collections:feed.collections.length,releases:feed.releases.length,recordings:feed.recordings.length,sessions:feed.sessions.length},publishedSessionVisible:true,publicSessionLinkPresent:true,releasesAnchorWorks:true,passwordGateAbsent:true,anonymousSession:true,pageErrors:errors,businessWrites:writes.length,blockedExternalRequests:blocked.length,entryPath:new URL(entry.url()).pathname,entrySha256:createHash('sha256').update(bytes).digest('hex'),embeddedCommits:[...new Set(bytes.toString().match(/(?<![a-f0-9])[a-f0-9]{40}(?![a-f0-9])/g)||[])],passed:true});save();
  }finally{await context.close();}
 }
 report.status='VERIFIED-public-records-destination-only';
}catch(error){report.status='FAILED';report.failure=error.message;throw error;}finally{await browser.close();report.browserClosed=true;report.finishedAt=new Date().toISOString();save();}
