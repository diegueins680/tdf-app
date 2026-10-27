import assert from 'node:assert/strict';
import {readFileSync, readdirSync, writeFileSync} from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {execFileSync} from 'node:child_process';
const root=process.env.TDF_RECOVERY_SOURCE, out=process.env.TDF_RECOVERY_OUT;
const {chromium,expect}=await import(pathToFileURL(path.join(root,'node_modules/@playwright/test/index.mjs')));
const base='http://127.0.0.1:4295',api='http://127.0.0.1:18859';
const email='st001-recovery@persona.test',oldPassword='Synthetic-ST001-old42!',newPassword='Synthetic-ST001-new42!';
const result={scope:'Real isolated native API/PostgreSQL and rendered optimized web with local synthetic SMTP. No real email/provider/device/production claim.',checks:{},blockedExternal:0,blockedMutations:0,blockedMutationPaths:[],pageErrors:0};
function sql(input){return execFileSync('psql',[process.env.TDF_RECOVERY_DATABASE_URL,'-XqAt','-v','ON_ERROR_STOP=1','-c',input],{encoding:'utf8'}).trim();}
async function request(p,body,token){const r=await fetch(api+p,{method:body?'POST':'GET',headers:{...(body?{'Content-Type':'application/json'}:{}),...(token?{Authorization:`Bearer ${token}`}:{})},...(body?{body:JSON.stringify(body)}:{})});const text=await r.text();return {status:r.status,value:text?(()=>{try{return JSON.parse(text)}catch{return text}})():null};}
function mails(){return readdirSync(out).filter(x=>/^mail-\d+\.json$/.test(x)).map(x=>JSON.parse(readFileSync(path.join(out,x),'utf8')));}
async function resetLink(after){await expect.poll(()=>mails().filter(x=>x.links.length).length,{timeout:15000}).toBeGreaterThan(after);const message=mails().filter(x=>x.links.length).at(-1);assert.deepEqual(message.recipient,[email]);assert.match(message.subject,/Restablecer/);return message.links[0].replaceAll('&amp;','&');}
function save(){writeFileSync(path.join(out,'journey.json'),JSON.stringify(result,null,2)+'\n');}
const browser=await chromium.launch();
const context=await browser.newContext({locale:'es-EC',serviceWorkers:'block'});
await context.route('**/*',route=>{
 const r=route.request(),u=new URL(r.url());
 if(![base,api].includes(u.origin)){result.blockedExternal++;return route.abort('blockedbyclient');}
 if(!['GET','HEAD','OPTIONS'].includes(r.method())){
  const allowed=u.origin===api&&((r.method()==='POST'&&['/signup','/login','/session/logout','/v1/password-reset','/v1/password-reset/confirm','/session/onboarding/reconcile','/session/onboarding/complete','/navigation/preferences/directory.classifieds.manage/visit'].includes(u.pathname))||(r.method()==='DELETE'&&u.pathname==='/radio/presence')||(['POST','PUT','PATCH'].includes(r.method())&&u.pathname.startsWith('/onboarding/')));
  if(!allowed){result.blockedMutations++;result.blockedMutationPaths.push(r.method()+' '+u.pathname);return route.abort('blockedbyclient');}
 }
 return route.continue();
});
const page=await context.newPage();page.on('pageerror',()=>result.pageErrors++);
try{
 await page.goto(base+'/mis-clasificados?st001=return');
 await expect(page).toHaveURL(/\/login\?/,{timeout:30000});
 assert.equal(new URL(page.url()).searchParams.get('redirect'),'/mis-clasificados?st001=return');result.checks.protectedIntentPreserved=true;
 await page.getByRole('button',{name:'Crear cuenta general',exact:true}).click();
 const dialog=page.getByRole('dialog');await expect(dialog).toBeVisible();
 await dialog.locator('input[autocomplete="email"]').fill(email);
 await dialog.locator('input[autocomplete="new-password"]').fill(oldPassword);
 await dialog.locator('button[type="submit"]').click();
 await expect(page).toHaveURL(base+'/mis-clasificados?st001=return',{timeout:30000});result.checks.uiSignupAndOriginalIntent=true;
 const login=await request('/login',{username:email,password:oldPassword});assert.equal(login.status,200);
 result.partyId=login.value.partyId;
 const session=await context.request.get(api+'/session');assert.equal(session.status(),200);assert.equal((await session.json()).partyId,result.partyId);
 assert.equal(sql(`SELECT count(*) FROM user_credential WHERE party_id=${Number(result.partyId)} AND active`),'1');
 // Explicit server-side invalidation models the expired/revoked-session boundary.
 // Ordinary API token TTL is not asserted: that is a separate contract.
 sql(`UPDATE api_token SET active=false WHERE party_id=${Number(result.partyId)};`);
 await page.reload();await expect(page).toHaveURL(/\/login\?/,{timeout:30000});
 assert.equal(new URL(page.url()).searchParams.get('redirect'),'/mis-clasificados?st001=return');result.checks.revokedSessionReturnsToLoginWithIntent=true;
 await page.getByRole('button',{name:/Recuperar|Restablecer/}).first().click();
 const reset=page.getByRole('dialog');await expect(reset).toBeVisible();
 await reset.locator('input[type="email"]').fill(email);
 await reset.getByRole('button',{name:'Enviar enlace',exact:true}).click();
 const expiredLink=await resetLink(0);let expiredToken=new URL(expiredLink).searchParams.get('token');assert.ok(expiredToken);
 assert.equal(new URL(expiredLink).searchParams.get('redirect'),'/mis-clasificados?st001=return');assert.equal(new URL(expiredLink).searchParams.get('lang'),'es');result.checks.nativeLocalizedMailPreservesIntent=true;
 sql(`UPDATE auth_recovery_challenge SET issued_at_epoch=floor(extract(epoch from clock_timestamp()))::bigint-901,expires_at_epoch=floor(extract(epoch from clock_timestamp()))::bigint-1 WHERE api_token_id=(SELECT id FROM api_token WHERE token='${expiredToken}');`);
 await page.goto(expiredLink);await expect(page.getByLabel('Nueva contraseña',{exact:true})).toBeVisible();
 assert.equal(new URL(page.url()).searchParams.has('token'),false);result.checks.tokenRemovedFromVisibleUrl=true;
 await page.getByLabel('Nueva contraseña',{exact:true}).fill(newPassword);
 await page.getByLabel('Confirmar contraseña',{exact:true}).fill(newPassword);
 await page.getByRole('button',{name:'Guardar contraseña',exact:true}).click();
 await expect(page.getByRole('alert')).toHaveText('Este enlace no es válido o ya venció. Solicita uno nuevo para continuar.');result.checks.expiredResetRejectedInUi=true;
 const beforeResetSession=await request('/login',{username:email,password:oldPassword});assert.equal(beforeResetSession.status,200);
 await page.getByRole('link',{name:'Solicitar nuevo enlace',exact:true}).click();
 await expect(page.getByRole('dialog')).toBeVisible();
 await page.getByRole('dialog').locator('input[type="email"]').fill(email);
 await page.getByRole('button',{name:'Enviar enlace',exact:true}).click();
 const validLink=await resetLink(1),validToken=new URL(validLink).searchParams.get('token');assert.notEqual(validToken,expiredToken);
 await page.goto(validLink);await expect(page.getByLabel('Nueva contraseña',{exact:true})).toBeVisible();
 await page.getByLabel('Nueva contraseña',{exact:true}).fill(newPassword);
 await page.getByLabel('Confirmar contraseña',{exact:true}).fill(newPassword);
 await page.getByRole('button',{name:'Guardar contraseña',exact:true}).click();
 await expect(page).toHaveURL(base+'/mis-clasificados?st001=return',{timeout:30000});result.checks.resetUiReturnsToOriginalIntent=true;
 assert.equal((await request('/login',{username:email,password:oldPassword})).status,401);result.checks.oldPasswordRejected=true;
 const fresh=await request('/login',{username:email,password:newPassword});assert.equal(fresh.status,200);assert.equal(fresh.value.partyId,result.partyId);result.checks.newPasswordAuthenticatesSameParty=true;
 assert.equal((await request('/v1/password-reset/confirm',{token:validToken,newPassword:oldPassword})).status,400);result.checks.resetTokenSingleUse=true;
 assert.equal((await request('/v1/password-reset/confirm',{token:expiredToken,newPassword:oldPassword})).status,400);result.checks.earlierResetStillRejected=true;
 assert.equal((await request('/session',null,login.value.token)).value,null);result.checks.oldSessionRejected=true;
 assert.equal((await request('/session',null,beforeResetSession.value.token)).value,null);result.checks.activePriorSessionRevokedByReset=true;
 assert.equal(sql(`SELECT count(*) FROM user_credential WHERE party_id=${Number(result.partyId)} AND active`),'1');result.checks.singlePersistedCredential=true;
 assert.equal(result.blockedMutations,0);assert.equal(result.pageErrors,0);result.status='PASS';save();
}catch(error){result.status='FAILED';result.failure=String(error).replaceAll(/[a-f0-9]{8}-[a-f0-9-]{27,}/gi,'[redacted-token]');save();throw error;}
finally{await context.close();await browser.close();}
