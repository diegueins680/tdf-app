import assert from 'node:assert/strict';
import {writeFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {chromium,devices,expect} from '/private/tmp/tdf-follow-response-recovery-20261026/node_modules/@playwright/test/index.mjs';
const [base,expected,expectedHash,mode,output]=process.argv.slice(2);
assert.equal(base,'https://317e5922.tdf-app.pages.dev');
assert.equal(expected,'6a097370d59d3c2b72ee2d387dc633b3eca98d96');
assert.match(expected,/^[a-f0-9]{40}$/);assert.match(expectedHash,/^[a-f0-9]{64}$/);
assert.ok(['original','repaired'].includes(mode));assert.ok(output.startsWith('/private/tmp/tdf-'));
const api=base.startsWith('https:')?'https://api.tdfrecords.net':'http://127.0.0.1:18714';
const artistId=91001,accountId=4101;
const artist={apArtistId:artistId,apDisplayName:'Artista ficticio',apSlug:'artista-ficticio',apBio:'Datos ficticios para verificar recuperación.',apCity:'Quito',apGenreIds:[],apFollowerCount:0,apHasUserAccount:true};
const account={username:'published-follow@example.test',displayName:'Fan ficticio',partyId:accountId,roles:['fan'],modules:[],featureFlags:[],preferences:{locale:'es',currency:'USD',timeZone:'America/Guayaquil'}};
const follow={ffArtistId:artistId,ffArtistName:artist.apDisplayName,ffStartedAt:'2026-10-26T12:00:00Z'};
const changeError='No pudimos confirmar el cambio de seguimiento. Comprueba tu conexión y vuelve a intentarlo.';
const result={startedAt:new Date().toISOString(),expectedCommit:expected,expectedHash,mode,base,status:'PARTIAL',productionWrites:false,humanParticipants:0,cases:[],scope:'Actual provider-listed preview, exactsource/hash required. ALL API/session/follow/first-value responses synthetic and intercepted; writes never leave the browser. Synthetic state changes simulate a committed write followed by a lost response. This is client verification, not actual server persistence, cookies, people, VAR or retention.'};
const browser=await chromium.launch();
try{
 for(const surface of ['public','community'])for(const mobile of [false,true])for(const failedRead of (mode==='original'?[false]:[false,true])){
  const c=await browser.newContext({...mobile?devices['Pixel 7']:{},locale:'es-EC',serviceWorkers:'block'});
  const row={surface,formFactor:mobile?'Pixel7-emulated-web':'desktop-web',failedRead,postCount:0,deleteCount:0,followReads:0,unexpectedMutations:0,blockedExternal:0,ambientCleanup:0,pageErrors:[],entry:null};result.cases.push(row);
  let followed=false,lostNext=false,readUnavailable=false;
  await c.route('**/*',async route=>{
   const r=route.request(),u=new URL(r.url()),method=r.method();
   if(u.origin===api){
    const headers={'Access-Control-Allow-Origin':base,'Access-Control-Allow-Credentials':'true'};
    const json=(v,status=200)=>route.fulfill({status,headers,contentType:'application/json',body:JSON.stringify(v)});
    if(method==='OPTIONS')return route.fulfill({status:204,headers});
    if(u.pathname==='/session'&&method==='GET')return json(account);
    if(u.pathname==='/session/preferences'&&method==='GET')return json(account.preferences);
    if(u.pathname.startsWith('/session/onboarding/')&&method==='POST')return json({newlyCompleted:false,progress:{eligible:false,firstValue:null,completedAt:null}});
    if(u.pathname==='/radio/presence'&&method==='DELETE'){row.ambientCleanup++;return route.fulfill({status:204,headers});}
    if(u.pathname==='/fans/me/follows/'+artistId&&['POST','DELETE'].includes(method)){
     if(method==='POST'){row.postCount++;followed=true;}else{row.deleteCount++;followed=false;}
     if(lostNext){lostNext=false;readUnavailable=failedRead;return route.abort('failed');}
     return json(method==='POST'?follow:null);
    }
    if(!['GET','HEAD'].includes(method)){row.unexpectedMutations++;return route.abort('blockedbyclient');}
    if(u.pathname==='/fans/me/follows'){row.followReads++;return readUnavailable?json({error:'Synthetic unavailable read'},503):json(followed?[follow]:[]);}
    if(u.pathname==='/artists/'+artistId+'/public'||u.pathname==='/artists/artista-ficticio/public'||u.pathname==='/fans/artists/'+artistId)return json(artist);
    if(u.pathname==='/fans/artists'||u.pathname==='/artists/search')return json([artist]);
    if(u.pathname==='/fans/me/profile')return json({fpArtistId:accountId,fpDisplayName:account.displayName,fpFavoriteGenreIds:[]});
    if(u.pathname==='/fans/me/clubs'||u.pathname==='/fans/artists/'+artistId+'/releases'||u.pathname==='/social-events/upcoming')return json([]);
    if(u.pathname==='/fans/artists/'+artistId+'/fans')return json({items:[],total:0,page:1,pageSize:5});
    return json({error:'Synthetic endpoint outside this scoped probe'},404);
   }
   if(u.origin===base&&method==='GET'&&(!['fetch','xhr'].includes(r.resourceType())||u.pathname.startsWith('/assets/')))return route.continue();
   row.blockedExternal++;return route.abort('blockedbyclient');
  });
  const p=await c.newPage();p.on('pageerror',e=>row.pageErrors.push(e.message));
  await p.goto(base+(surface==='public'?'/artista/'+artistId+'?source=fixture#music':'/fans'),{waitUntil:'domcontentloaded'});
  const src=await p.locator('script[type="module"][src]').first().getAttribute('src'),url=new URL(src,base).href;assert.ok(url.startsWith(base+'/assets/index-'));
  const body=await p.evaluate(async url=>{const r=await fetch(url,{cache:'no-store'});if(!r.ok)throw Error('Static entry unavailable');return r.text();},url);
  row.entry={url,sha256:createHash('sha256').update(body).digest('hex'),expectedVersionObserved:body.includes(expected)};assert.equal(row.entry.sha256,expectedHash);assert.ok(row.entry.expectedVersionObserved);
  const button=following=>p.getByRole('button',{name:surface==='public'?(following?'Dejar de seguir a '+artist.apDisplayName:'Seguir a '+artist.apDisplayName):(following?'Siguiendo':'Seguir'),exact:true}).last();
  await expect(button(false)).toBeEnabled();const initialReads=row.followReads;lostNext=true;await button(false).click();
  if(mode==='original'){
   await expect(p.getByRole('alert').filter({hasText:/No pudimos/}).last()).toBeVisible();await expect(button(false)).toBeEnabled();assert.ok(followed);assert.equal(row.postCount,1);assert.equal(row.followReads,initialReads);row.originalMismatchReproduced=true;
  }else{
   if(failedRead){await expect(p.getByText(changeError,{exact:true})).toBeVisible();await expect(button(false)).toBeDisabled();assert.equal(row.postCount,1);readUnavailable=false;await (surface==='community'?p.getByRole('alert').filter({hasText:'Tuvimos un problema cargando tu información. Revisa tu conexión o intenta de nuevo.'}):p).getByRole('button',{name:'Reintentar',exact:true}).click();}
   await expect(button(true)).toBeEnabled();assert.ok(followed);assert.equal(row.postCount,1);assert.ok(row.followReads>initialReads);await expect(p.getByText(changeError,{exact:true})).toHaveCount(0);row.recoveredWithoutReplay=true;
   const before=row.followReads;lostNext=true;await button(true).click();
   if(failedRead){await expect(p.getByText(changeError,{exact:true})).toBeVisible();readUnavailable=false;await (surface==='community'?p.getByRole('alert').filter({hasText:'Tuvimos un problema cargando tu información. Revisa tu conexión o intenta de nuevo.'}):p).getByRole('button',{name:'Reintentar',exact:true}).click();}
   await expect(button(false)).toBeEnabled();assert.ok(!followed);assert.equal(row.deleteCount,1);assert.equal(row.postCount,1);assert.ok(row.followReads>before);row.unfollowRecoveredWithoutReplay=true;
  }
  assert.equal(row.unexpectedMutations,0);assert.deepEqual(row.pageErrors,[]);await c.close();row.status='VERIFIED-static-client-with-synthetic-business';
 }
 for(const mobile of [false,true]){
  const c=await browser.newContext({...mobile?devices['Pixel 7']:{},locale:'es-EC',serviceWorkers:'block'});
  const row={surface:'signup-default-name-privacy',formFactor:mobile?'Pixel7-emulated-web':'desktop-web',signupPosts:0,privateMarkerSentAsPublicName:false,pageErrors:[]};result.cases.push(row);
  let received;
  await c.route('**/*',async route=>{
   const r=route.request(),u=new URL(r.url()),method=r.method();
   if(u.origin===api){
    const headers={'Access-Control-Allow-Origin':base,'Access-Control-Allow-Credentials':'true'};
    const json=(value,status=200)=>route.fulfill({status,headers,contentType:'application/json',body:JSON.stringify(value)});
    if(method==='OPTIONS')return route.fulfill({status:204,headers});
    if(u.pathname==='/session'&&method==='GET')return json(null);
    if(u.pathname==='/signup'&&method==='POST'){row.signupPosts++;received=r.postDataJSON();return json({error:'Fictional signup adapter: no account created'},503);}
    return json(null,404);
   }
   if(u.origin===base&&method==='GET'&&(!['fetch','xhr'].includes(r.resourceType())||u.pathname.startsWith('/assets/')))return route.continue();
   return route.abort('blockedbyclient');
  });
  const p=await c.newPage();p.on('pageerror',e=>row.pageErrors.push(e.message));await p.goto(base+'/login?signup=1&intent=artist',{waitUntil:'domcontentloaded'});
  const dialog=p.getByRole('dialog');await expect(dialog).toBeVisible();await dialog.locator('input[autocomplete="email"]').fill('private-identity-938421@pilot.example.test');await dialog.locator('input[autocomplete="new-password"]').fill('Synthetic-activation-42!');await dialog.locator('button[type="submit"]').click();
  await expect.poll(()=>row.signupPosts).toBe(1);assert.equal(received.firstName,'Cuenta TDF');assert.equal(received.lastName,'');assert.equal(received.email,'private-identity-938421@pilot.example.test');assert.equal(received.termsAccepted,true);assert.equal(received.marketingOptIn,false);row.privateMarkerSentAsPublicName=received.firstName.includes('938421');assert.equal(row.privateMarkerSentAsPublicName,false);await expect(dialog).toBeVisible();assert.deepEqual(row.pageErrors,[]);row.status='VERIFIED-static-client-neutral-signup-name-with-synthetic-503';await c.close();
 }
 result.status=mode==='original'?'VERIFIED-original-client-mismatch-with-synthetic-state':'VERIFIED-static-client-recovery-with-synthetic-state';
}catch(e){result.error=String(e);throw e;}
finally{await browser.close();result.browserClosed=true;result.finishedAt=new Date().toISOString();writeFileSync(output,JSON.stringify(result,null,2)+'\n',{mode:0o600});console.log(JSON.stringify({status:result.status,cases:result.cases.length,error:result.error}));}
