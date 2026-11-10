import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { createRequire } from 'node:module';
import { mkdirSync,writeFileSync,openSync,closeSync,readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import path from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from '@playwright/test';

export async function runCrmBrowser({base,token,cmd,sql,request,invite,checks}) {
  const root=process.cwd();const require=createRequire(path.join(root,'tdf-hq-ui/package.json'));
  const artifacts=path.join(root,'output/playwright/event-crm');mkdirSync(artifacts,{recursive:true});
  const scratch=process.env.TDF_EVENT_CRM_ARTIFACTS;const config=path.join(root,'.private',`event-crm-vite-${process.pid}.mjs`);
  mkdirSync(path.dirname(config),{recursive:true});
  writeFileSync(config,`import original from ${JSON.stringify(pathToFileURL(path.join(root,'tdf-hq-ui/vite.config.ts')).href)};\nexport default {...original,root:${JSON.stringify(path.join(root,'tdf-hq-ui'))},cacheDir:${JSON.stringify(path.join(scratch,'vite-cache'))},server:{host:'127.0.0.1',port:4491,strictPort:true,proxy:{'/api':{target:${JSON.stringify(base)},changeOrigin:true,rewrite:p=>p.replace(/^\\/api/,'')}}}};\n`,{flag:'wx'});
  const viteBin=path.join(path.dirname(require.resolve('vite/package.json')),'bin/vite.js');
  const viteEnv=Object.fromEntries(Object.entries(process.env).filter(([k])=>['PATH','USER','LOGNAME','SHELL','TMPDIR','LANG'].includes(k)));
  Object.assign(viteEnv,{VITE_API_BASE:'/api',VITE_POSTHOG_KEY:'',VITE_SENTRY_DSN:''});
  const log=openSync(path.join(scratch,'vite.log'),'w');
  const vite=spawn(process.execPath,[viteBin,'--config',config,'--configLoader','runner'],{cwd:path.join(root,'tdf-hq-ui'),env:viteEnv,stdio:['ignore',log,log]});
  let browser;
  const origin='http://127.0.0.1:4491';
  const eventTitle=sql('SELECT title FROM social_event WHERE id=7002');
  const mark=(name,value)=>{assert(value,name);checks.push(name);console.log(`PASS ${name}`);};
  const external=new Set();
  async function contextFor(actor,viewport){
    const context=await browser.newContext({viewport,acceptDownloads:true});
    await context.addCookies([{name:'tdf_session',value:token(actor),url:origin,httpOnly:true,sameSite:'Lax'}]);
    await context.route('**/*',route=>{
      const url=new URL(route.request().url());
      if(['127.0.0.1','localhost'].includes(url.hostname)||['data:','blob:'].includes(url.protocol))return route.continue();
      external.add(url.origin);return route.abort();
    });
    return context;
  }
  try {
    let ready=false;for(let i=0;i<150;i++){
      if(vite.exitCode!==null)throw new Error('Owned Vite process exited; inspect private vite.log');
      try{const response=await fetch(origin);if(response.ok){ready=true;break;}}catch{}
      await delay(100);
    }assert(ready,'Owned Vite readiness');
    browser=await chromium.launch({headless:true});
    const desktop=await contextFor(7101,{width:1440,height:1000});
    const page=await desktop.newPage();page.setDefaultTimeout(30000);
    await page.goto(origin+'/social/eventos/7002/crm');
    await page.getByRole('heading',{name:eventTitle+' · CRM',exact:true}).waitFor();
    mark('desktop administration uses actual event-scoped HTTP session',await page.getByText(/Ventas con chiva desactivadas/).isVisible());
    await page.getByLabel('Nombre del contacto',{exact:true}).fill('Interesado móvil de ensayo');
    await page.getByLabel('Correo del contacto',{exact:true}).fill('mobile-crm@example.invalid');
    await page.getByLabel('Teléfono del contacto',{exact:true}).fill('0992223344');
    await page.getByRole('button',{name:'Buscar contacto canónico y añadir'}).click();
    await page.getByRole('heading',{name:'Interesado móvil de ensayo'}).waitFor();
    await page.getByLabel('Destinataria por',{exact:true}).click();
    await page.getByRole('option',{name:'ID de cuenta TDF confirmada'}).click();
    await page.getByLabel('Cuenta o correo de la destinataria',{exact:true}).fill('7108');
    await page.getByRole('button',{name:'Resolver cuenta TDF antes de emitir'}).click();
    await page.getByRole('button',{name:/Usar cuenta 7108/}).click();
    await page.getByLabel('Rol de la invitación',{exact:true}).click();
    await page.getByRole('option',{name:'Coordinación: equipo, logística y CRM del evento'}).click();
    await page.getByRole('checkbox',{name:/Confirmé con la persona organizadora/}).check();
    await page.getByRole('button',{name:'Generar enlace privado de siete días'}).click();
    const linkInput=page.getByLabel('Enlace privado',{exact:true});await linkInput.waitFor();
    const link=await linkInput.inputValue();assert(new URL(link).hash.length===44);
    await page.screenshot({path:path.join(artifacts,'admin-desktop.png'),fullPage:true,mask:[linkInput]});
    mark('desktop account resolution and private one-use link generation work',true);
    const mobile=await contextFor(7108,{width:390,height:844});
    const mobilePage=await mobile.newPage();mobilePage.setDefaultTimeout(30000);
    await mobilePage.goto(link);
    await mobilePage.getByRole('button',{name:'Revisar evento, rol y permisos'}).click();
    await mobilePage.getByText(/Consultar y gestionar el embudo completo/).waitFor();
    mark('mobile invitation displays event role and permissions before acceptance',await mobilePage.getByText(eventTitle,{exact:true}).isVisible());
    await mobilePage.screenshot({path:path.join(artifacts,'invitation-mobile.png'),fullPage:true});
    await mobilePage.getByRole('button',{name:'Aceptar este rol y sus permisos'}).click();
    await mobilePage.getByText('Invitación aceptada. El enlace ya no puede volver a usarse.').waitFor();
    mark('mobile accepts with the correct account and removes the secret fragment',new URL(mobilePage.url()).hash==='');
    await mobilePage.goto(origin+'/social/eventos/7002/crm');
    await mobilePage.getByRole('heading',{name:'Interesado móvil de ensayo'}).waitFor();
    await mobilePage.getByLabel('Notas',{exact:true}).fill('Seguimiento guardado desde móvil');
    await mobilePage.getByLabel('Próxima acción',{exact:true}).fill('Confirmar interés y combinación');
    await mobilePage.getByLabel('Fecha de seguimiento (ISO con zona horaria)',{exact:true}).fill('2026-11-10T15:00:00-05:00');
    await mobilePage.getByRole('button',{name:'Guardar seguimiento'}).click();
    await mobilePage.getByText('Cambio guardado.').waitFor();
    mark('mobile follow-up persists through server authorization',sql("SELECT notes FROM event_crm_lead WHERE event_id=7002") === 'Seguimiento guardado desde móvil');
    await mobilePage.getByLabel('Título de actividad',{exact:true}).fill('Logística de ensayo desde móvil');
    await mobilePage.getByLabel('Notas de logística',{exact:true}).fill('Seguimiento del equipo sin permisos financieros');
    await mobilePage.getByLabel('Inicio de actividad (ISO con zona horaria)',{exact:true}).fill('2026-12-05T10:00:00-05:00');
    await mobilePage.getByRole('button',{name:'Guardar actividad de logística'}).click();
    await mobilePage.getByText('Cambio guardado.').waitFor();
    mark('mobile coordinator saves canonical logistics activity',sql("SELECT count(*) FROM event_logistics_activity WHERE event_id=7002 AND title='Logística de ensayo desde móvil'")==='1');
    mark('mobile coordinator sees no payment confirmation or superior-role controls',await mobilePage.getByRole('button',{name:'Confirmar pago manual con evidencia'}).count()===0);
    mark('mobile administration has no horizontal document overflow',await mobilePage.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth));
    await mobilePage.getByRole('heading',{name:'Interesado móvil de ensayo'}).scrollIntoViewIfNeeded();
    await mobilePage.screenshot({path:path.join(artifacts,'crm-mobile.png'),fullPage:true});
    const download=mobilePage.waitForEvent('download');
    await mobilePage.getByRole('button',{name:'Exportar datos autorizados (archivo privado)'}).click();
    const downloaded=await download;const downloadPath=path.join(scratch,'synthetic-mobile-export.json');await downloaded.saveAs(downloadPath);
    const exportText=readFileSync(downloadPath,'utf8');
    mark('mobile export contains scoped contacts and excludes invitation secret',JSON.parse(exportText).event.id===7002&&!exportText.includes(new URL(link).hash.slice(1)));
    const memberId=sql('SELECT id FROM event_crm_membership WHERE event_id=7002 AND party_id=7108 AND revoked_at IS NULL');
    await cmd(7101,{operation:'membership.revoke',membershipId:memberId},7002);
    await mobilePage.reload();
    await mobilePage.getByText('No tienes acceso al CRM de este evento o el piloto todavía no está habilitado.').waitFor();
    mark('mobile membership withdrawal hides the previously readable CRM',await mobilePage.getByRole('heading',{name:'Interesado móvil de ensayo'}).count()===0);
    await desktop.close();await mobile.close();
    writeFileSync(path.join(scratch,'browser-results.json'),JSON.stringify({checks:checks.filter(x=>/mobile|desktop/.test(x)),viewport:{width:390,height:844},externalOriginsBlocked:[...external],realData:false},null,2)+'\n');
  } finally {
    if(browser)await browser.close();vite.kill('SIGTERM');await new Promise(resolve=>{if(vite.exitCode!==null)return resolve();vite.once('exit',resolve);});closeSync(log);
  }
}
