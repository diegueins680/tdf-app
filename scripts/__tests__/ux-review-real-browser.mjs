import {chromium,firefox,webkit} from '@playwright/test';
import {randomBytes} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {writeFile} from 'node:fs/promises';
import assert from 'node:assert/strict';
const base=process.env.TDF_AUDIT_BASE??'http://127.0.0.1:18655',results=[];
assert.ok(['localhost','127.0.0.1'].includes(new URL(base).hostname), 'isolated origin required');
for(const [engine,type] of Object.entries({chromium,firefox,webkit})){
 const browser=await type.launch();
 try {
  for(const stored of [false,true]){
   const c=await browser.newContext({locale:'es-EC'});
   if(stored)await c.addInitScript(()=>{localStorage.setItem('tdf-hq-ui/locale-preferences',JSON.stringify({locale:'es',localeId:'synthetic-es'}));localStorage.setItem('tdf-locale','es')});
   const p=await c.newPage();await p.goto(base+'/reset?token=synthetic&lang=en&redirect=%2Ffans');
   await p.getByRole('heading',{name:/Choose your new password/i}).waitFor({timeout:10000});
   assert.equal(await p.locator('html').getAttribute('lang'),'en');
   results.push({engine,journey:'recovery-language',storedSpanish:stored,language:'en'});await c.close();
  }
  if(engine==='chromium'){
   const c=await browser.newContext({locale:'es-EC'});const p=await c.newPage();
   await p.goto(base+'/login?redirect=%2Ffans');await p.getByRole('button',{name:'Crear cuenta general',exact:true}).click();
   const d=p.getByRole('dialog');await d.getByRole('textbox',{name:'Nombre'}).fill('Sesión de navegador sintética');await d.getByRole('textbox',{name:'Correo'}).fill('ux-cookie-'+randomBytes(8).toString('hex')+'@example.test');await d.locator('input[name=newPassword]').fill('Synthetic-'+randomBytes(16).toString('hex'));await d.getByRole('checkbox').check();await d.getByRole('button',{name:'Crear e ingresar',exact:true}).click();await p.waitForURL(base+'/fans');
   const before=await(await c.request.get(base+'/api/session')).json();assert.ok(before.partyId!==50);
   const band='UX authority '+randomBytes(8).toString('hex');
   await p.goto(base+'/live-sessions/registro');await p.getByLabel('Código de acceso',{exact:true}).fill('synthetic-ux-admin-inspection');await p.getByRole('button',{name:'Validar código',exact:true}).click();
   const input=p.getByLabel('Nombre de la banda / artista',{exact:false});await input.fill(band);
   await p.getByLabel('Nombre completo',{exact:false}).fill('Músico aislado '+randomBytes(5).toString('hex'));
   await p.getByRole('checkbox',{name:/Acepto los términos/}).check();
   const response=p.waitForResponse(r=>r.request().method()==='POST'&&r.url().endsWith('/live-sessions/intake'));
   await p.getByRole('button',{name:'Enviar Live Session',exact:true}).click();const r=await response;if(!r.ok())throw new Error(await r.text());
   const createdBy=execFileSync('docker',['exec','tdf-ux-claim-target-db-20260917','psql','-U','postgres','-d','tdf_ux_claim_target_test','-Atc',`SELECT created_by FROM live_session_intake WHERE band_name='${band}'`],{encoding:'utf8'}).trim();assert.equal(createdBy,'50');
   const after=await(await c.request.get(base+'/api/session')).json();assert.equal(after.partyId,before.partyId);
   results.push({engine,journey:'live-intake-authority',status:r.status(),persistedUnderCodeAccount:true,ambientSessionUnchanged:true});await c.close();
  }
 } finally{await browser.close();await writeFile(process.env.TDF_AUDIT_OUTPUT??'/tmp/tdf-ux-review-browser-results.json',JSON.stringify(results,null,2));}
}
console.log(JSON.stringify({passed:true,results}));
