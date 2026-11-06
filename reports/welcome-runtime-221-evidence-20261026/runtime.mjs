import assert from 'node:assert/strict';
import {readFileSync,writeFileSync} from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
export const root=process.env.TDF_PILOT_SOURCE;
export const out=process.env.TDF_PILOT_OUT;
export const base='http://127.0.0.1:4289',api='http://127.0.0.1:18819';
export const {chromium,devices,expect}=await import(pathToFileURL(path.join(root,'node_modules/@playwright/test/index.mjs')));
export const image='https://pilot-assets.example.test/avatar.png';
const png=readFileSync(path.join(root,'e2e/web/fixtures/thumbnail-valid.png'));
export const observation={mode:process.env.TDF_PILOT_MODE,humanParticipants:process.env.TDF_PILOT_MODE==='manual'?null:0,consent:[],blockedExternalRequests:0,blockedMutations:0,blockedMutationPaths:[],uploads:0,uploadAttempts:0,profileFailures:0,scope:'Consent acknowledgements are not independently verified people or comprehension. No recordings, quotations, production writes or provider certification.'};
let stopped=false;
const allowed=(method,p)=>method==='POST'&&['/signup','/login','/session/logout','/artists/me/activate','/session/onboarding/reconcile','/session/onboarding/complete'].includes(p)
 ||method==='DELETE'&&p==='/radio/presence'
 ||method==='PUT'&&p==='/fans/me/artist-profile'
 ||method==='POST'&&/^\/fans\/me\/follows\/\d+$/.test(p)
 ||['POST','PUT','PATCH'].includes(method)&&p.startsWith('/onboarding/');
export function save(){writeFileSync(path.join(out,'browser.json'),JSON.stringify(observation,null,2)+'\n');}
export async function context(browser,options={},artist=false){
 const c=await browser.newContext({...options,locale:'es-EC',serviceWorkers:'block'});
 let accepted=false;
 await c.route('**/*',async route=>{
  const r=route.request(),u=new URL(r.url());
  if(!accepted)return route.fulfill({status:451,body:'Primero acepta participar; no se ha enviado esta solicitud.'});
  if(r.url()===image&&r.method()==='GET')return route.fulfill({contentType:'image/png',body:png});
  if(![base,api].includes(u.origin)){observation.blockedExternalRequests++;return route.abort('blockedbyclient');}
  if(u.origin===api&&u.pathname==='/drive/upload'&&r.method()==='POST'){
   const headers=await r.allHeaders();
   if(!artist)return route.fulfill({status:403,body:'Solo la persona artista ficticia.'});
   // Authenticate only with headers actually present on this XHR; never borrow
   // the browser context cookie jar, which would hide omitted credentials.
   const auth={...(headers.cookie?{Cookie:headers.cookie}:{}),...(headers.authorization?{Authorization:headers.authorization}:{})};
   const session=await fetch(api+'/session',{headers:auth}),profile=await fetch(api+'/fans/me/artist-profile',{headers:auth});
   if(session.status!==200||profile.status!==200)return route.fulfill({status:401,body:'Falta una sesión artista válida.'});
   const user=await session.json();if(!user.roles?.includes('Artist'))return route.fulfill({status:403,body:'Requiere artista.'});
   let file;try{const form=await new Response(r.postDataBuffer(),{headers:{'Content-Type':headers['content-type']}}).formData();file=form.get('file');}catch{}
   if(!file||file.name!=='tdf-pilot-avatar.png'||!Buffer.from(await file.arrayBuffer()).equals(png))return route.fulfill({status:422,body:'El ensayo solo admite la imagen ficticia provista.'});
   if(process.env.TDF_PILOT_MODE==='rehearsal'&&observation.uploadAttempts++===0)return route.fulfill({status:503,headers:{'Access-Control-Allow-Origin':base,'Access-Control-Allow-Credentials':'true'},body:'Synthetic upload failure'});
   observation.uploads++;return route.fulfill({headers:{'Access-Control-Allow-Origin':base,'Access-Control-Allow-Credentials':'true'},json:{duFileId:'pilot-avatar',duPublicUrl:image,duWebContentLink:image,duWebViewLink:image}});
  }
  if(process.env.TDF_PILOT_MODE==='rehearsal'&&u.origin===api&&u.pathname==='/fans/me/artist-profile'&&r.method()==='PUT'&&observation.profileFailures++===0)return route.fulfill({status:503,headers:{'Access-Control-Allow-Origin':base,'Access-Control-Allow-Credentials':'true'},body:'Synthetic save failure'});
  if(!['GET','HEAD','OPTIONS'].includes(r.method())){
   if(u.origin!==api||!allowed(r.method(),u.pathname)){observation.blockedMutations++;observation.blockedMutationPaths.push(r.method()+' '+u.pathname.replace(/\d+/g,':id'));return route.fulfill({status:403,body:'Acción fuera del ensayo aislado.'});}
   if(u.pathname==='/signup'){
    let data;try{data=r.postDataJSON();}catch{}
    if(!data?.email?.endsWith('@pilot.example.test'))return route.fulfill({status:422,body:'Utiliza solo el correo ficticio del ensayo.'});
   }
  }
  return route.continue();
 });
 c.on('page',p=>{p.on('filechooser',chooser=>chooser.setFiles({name:'tdf-pilot-avatar.png',mimeType:'image/png',buffer:png}));p.on('close',()=>{if(process.env.TDF_PILOT_MODE==='manual')stopped=true;});});
 const originalClose=c.close.bind(c);
 c.close=async options=>{await c.unrouteAll({behavior:'ignoreErrors'});return originalClose(options);};
 const page=await c.newPage();
 await page.exposeFunction('pilotDecision',decision=>{if(decision==='accept')accepted=true;else stopped=true;observation.consent.push({context:observation.consent.length+1,decision,synthetic:process.env.TDF_PILOT_MODE!=='manual',at:new Date().toISOString(),recording:false,quotation:false,productionPublication:false});save();});
 const content=`<!doctype html><html lang="es"><meta charset="utf-8"><title>Consentimiento · ensayo TDF</title><style>body{max-width:680px;margin:30px auto;padding:20px;font:18px/1.6 system-ui}button{font:inherit;padding:12px;margin:10px}label{display:block;margin:15px 0}</style><h1>Ensayo local de TDF</h1><p>Evaluamos el producto, no tu habilidad. Puedes detenerte o retirarte en cualquier momento. Usa exclusivamente datos, correo y contraseña ficticios. No introduzcas cuentas sociales, música inédita, contactos ni pagos.</p><p>El perfil será visible solo dentro de este entorno desechable. No hay publicación en producción ni grabación, audio, citas o invitaciones. Se conservarán únicamente códigos y resultados sanitizados; la base de datos se elimina al cerrar. La portada es una imagen ficticia provista automáticamente: no evaluamos el selector ni Google Drive.</p><p>${artist?'Imagina que quieres que nuevos fans descubran tu música. Crea tu perfil, revisa qué será visible, encuentra la vista pública y su enlace.':'Imagina que recibiste un enlace de un artista que te interesa. Descúbrelo, síguelo y comprueba cómo volver a encontrarlo después de salir e ingresar.'}</p><p>Correo ficticio: <code>${artist?'qar01':'qar02'}@pilot.example.test</code>. Elige una contraseña inventada para este ensayo. No se enviará correo.</p><label><input id="adult" type="checkbox"> Soy adulto/a y acepto participar voluntariamente.</label><label><input id="fiction" type="checkbox"> Entiendo que guardar hace visible el perfil en este ensayo y usaré solo datos ficticios.</label><button id="accept" disabled>Comenzar ensayo</button><button id="decline">No participar / retirarme</button><script>const a=document.getElementById('adult'),f=document.getElementById('fiction'),b=document.getElementById('accept');a.onchange=f.onchange=()=>b.disabled=!(a.checked&&f.checked);b.onclick=async()=>{await window.pilotDecision('accept');location.href=${JSON.stringify(base+'/inicio')}};document.getElementById('decline').onclick=()=>window.pilotDecision('decline');</script></html>`;
 await page.setContent(content);
 if(process.env.TDF_PILOT_MODE==='rehearsal'){
  await expect(page.getByRole('button',{name:'Comenzar ensayo',exact:true})).toBeDisabled();
  const blocked=await c.request.get(api+'/health'); // APIRequestContext is outside page routing: no user action, read-only.
  assert.equal(blocked.status(),200);
  const blockedInBrowser=await page.evaluate(async url=>{try{return(await fetch(url)).status}catch{return 0}},api+'/health');
  assert.notEqual(blockedInBrowser,200);observation.preConsentBrowserRequestBlocked=true;
  await page.locator('#adult').check();await page.locator('#fiction').check();
  await Promise.all([page.waitForURL(url=>url.origin===base,{waitUntil:'domcontentloaded'}),page.getByRole('button',{name:'Comenzar ensayo',exact:true}).click()]);
 }
 return {context:c,page};
}
