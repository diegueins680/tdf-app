import assert from 'node:assert/strict';
import { chromium, firefox, webkit } from '@playwright/test';
import axe from 'axe-core';
import { randomBytes } from 'node:crypto';
import { mkdir, writeFile } from 'node:fs/promises';
const base=process.env.TDF_ONBOARDING_BROWSER_BASE;
assert.match(base??'',/^http:\/\/127\.0\.0\.1:\d+$/, 'requires an isolated synthetic-account backend and local bundle');
const output=process.env.TDF_ONBOARDING_BROWSER_EVIDENCE??'/tmp/tdf-ux-onboarding-browser';
await mkdir(output,{recursive:true});
const results=[];
for(const [engine,type] of Object.entries({chromium,firefox,webkit})){
 if(process.env.TDF_ONBOARDING_ENGINE && process.env.TDF_ONBOARDING_ENGINE!==engine)continue;
 const browser=await type.launch();
 try {
 const context=await browser.newContext({viewport:{width:390,height:844},locale:'es-EC',reducedMotion:'reduce'});
 const page=await context.newPage();
 const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(`${base}/login?redirect=%2Ffans`);
 await page.getByRole('button',{name:'Crear cuenta general',exact:true}).click();
 const dialog=page.getByRole('dialog');
 await dialog.getByRole('textbox',{name:'Nombre'}).fill('Cuenta sintética QA');
 await dialog.getByRole('textbox',{name:'Correo'}).fill(`ux-browser-${randomBytes(8).toString('hex')}@example.test`);
 await dialog.locator('input[name=newPassword]').fill(`Synthetic-${randomBytes(16).toString('hex')}`);
 await dialog.getByRole('checkbox').check();
 const signupResponse=page.waitForResponse(r=>new URL(r.url()).pathname.endsWith('/signup') && r.request().method()==='POST');
 await dialog.getByRole('button',{name:'Crear e ingresar',exact:true}).click();
 const signup=await signupResponse;
 assert.equal(new URL(signup.url()).pathname,'/api/signup','bundle must target the isolated API proxy');
 assert.ok(signup.ok(),`synthetic signup failed with ${signup.status()}`);
 assert.match(signup.headers()['content-type']??'',/application\/json/,'signup must return the real API response');
 await page.waitForURL(`${base}/fans`);
 await page.getByText('Primeros pasos',{exact:true}).waitFor();
 const completionPromise=page.waitForResponse(r=>r.url().includes('/session/onboarding/complete'));
 await page.getByRole('button',{name:'Seguir',exact:true}).first().click();
 const completion=await completionPromise;assert.equal(completion.status(),200);
 const persisted=await completion.json();assert.equal(persisted.progress.firstValue,'artist_followed');assert.ok(persisted.progress.completedAt);
 await page.reload();
 const following=page.getByRole('button',{name:'Siguiendo',exact:true});await following.waitFor();
 assert.equal(await page.getByText('Primeros pasos',{exact:true}).count(),0);
 const widths=[];
 for(const width of [320,390,834,1280]){
  await page.setViewportSize({width,height:900});
  const measured=await page.evaluate(()=>({viewport:innerWidth,scroll:document.documentElement.scrollWidth}));
  assert.ok(measured.scroll<=width+1,`${engine} horizontal overflow: ${JSON.stringify(measured)}`);widths.push(measured);
 }
 await following.focus();await page.keyboard.press('Space');
 await page.waitForFunction(()=>!Array.from(document.querySelectorAll('button')).some(b=>b.textContent==='Siguiendo'));
 assert.equal(new URL(page.url()).pathname,'/fans','keyboard follow action must not navigate to artist');
 await page.addScriptTag({content:axe.source});
 const accessibility=await page.evaluate(async()=>window.axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}}));
 const violations=accessibility.violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));
 await writeFile(`${output}/${engine}-axe.json`,JSON.stringify(violations,null,2));
 await page.emulateMedia({colorScheme:'dark'});
 await page.waitForFunction(()=>document.documentElement.dataset.theme==='dark');
 await page.waitForTimeout(350); // let CSS color transitions settle before contrast sampling
 const darkWidths=[];
 for(const width of [320,390,834,1280]){
  await page.setViewportSize({width,height:900});
  const measured=await page.evaluate(()=>({viewport:innerWidth,scroll:document.documentElement.scrollWidth}));
  assert.ok(measured.scroll<=width+1,`${engine} dark overflow: ${JSON.stringify(measured)}`);darkWidths.push(measured);
 }
 const darkAxe=await page.evaluate(async()=>window.axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}}));
 const darkViolations=darkAxe.violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));
 await writeFile(`${output}/${engine}-dark-axe.json`,JSON.stringify(darkViolations,null,2));
 await page.evaluate(()=>document.documentElement.style.setProperty('font-size','32px','important'));
 await page.waitForFunction(()=>getComputedStyle(document.documentElement).fontSize==='32px');
 const enlarged=await page.evaluate(()=>({viewport:innerWidth,scroll:document.documentElement.scrollWidth,fontSize:getComputedStyle(document.documentElement).fontSize}));
 assert.equal(enlarged.fontSize,'32px','text enlargement must take effect');
 assert.ok(enlarged.scroll<=1281,`${engine} enlarged text overflow`);
 await page.evaluate(()=>document.documentElement.style.fontSize='');
 await page.setViewportSize({width:390,height:844});await page.screenshot({path:`${output}/${engine}-phone.png`,fullPage:true});
 results.push({engine,signup:true,persistedFirstValue: persisted.progress.firstValue,returnVisit:true,keyboardAction:true,widths,darkWidths,enlargedText:enlarged,pageErrors:errors,axeViolations:violations,darkAxeViolations:darkViolations});
 assert.deepEqual(errors,[]);assert.deepEqual(violations,[]);assert.deepEqual(darkViolations,[]);
 if(engine==='chromium'){
  const guestContext=await browser.newContext({viewport:{width:390,height:844},locale:'es-EC'});
  const guest=await guestContext.newPage();let followWrites=0;
  guest.on('request',r=>{if(r.method()==='POST'&&new URL(r.url()).pathname.includes('/fans/me/follows/'))followWrites+=1});
  await guest.goto(`${base}/fans`);
  await guest.getByRole('button',{name:'Seguir',exact:true}).first().click();
  await guest.getByRole('dialog').getByRole('button',{name:'Ir a login'}).click();
  await guest.waitForURL(url=>url.pathname==='/login');
  const target=new URL(guest.url()).searchParams.get('redirect');
  assert.match(target??'',/^\/a\/[^?]+\?resume=follow&artistId=\d+$/);
  const signup=guest.getByRole('dialog');
  await signup.getByRole('textbox',{name:'Nombre'}).fill('Continuidad sintética');
  await signup.getByRole('textbox',{name:'Correo'}).fill(`ux-resume-${randomBytes(8).toString('hex')}@example.test`);
  await signup.locator('input[name=newPassword]').fill(`Synthetic-${randomBytes(16).toString('hex')}`);
  await signup.getByRole('checkbox').check();await signup.getByRole('button',{name:'Crear e ingresar'}).click();
  await guest.getByRole('button',{name:'Seguir ahora',exact:true}).waitFor();
  assert.equal(followWrites,0,'signup must not silently execute a follow');
  await guest.getByRole('button',{name:'Seguir ahora',exact:true}).click();
  await guest.waitForURL(url=>url.pathname.startsWith('/a/')&&!url.searchParams.has('resume'));
  await guest.getByRole('button',{name:/^Dejar de seguir a /}).waitFor({timeout:10000}).catch(async e=>{await writeFile(`${output}/guest-failure.json`,JSON.stringify({url:guest.url(),text:await guest.locator('body').innerText(),followWrites}));throw e});
  await guest.reload();await guest.getByRole('button',{name:/^Dejar de seguir a /}).waitFor();
  assert.equal(followWrites,1,'only the confirmed follow may be sent');
  results.push({engine,guestFollowIntent:true,explicitConfirmation:true,persistedAfterReload:true});
  await guestContext.close();
 }

 } finally {await browser.close();await writeFile(`${output}/results.json`,JSON.stringify(results,null,2));}
}
console.log(JSON.stringify({passed:true,results}));
