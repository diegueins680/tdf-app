import assert from 'node:assert/strict';
import {chromium,expect,base,api,context,save} from './runtime.mjs';
import {writeFileSync} from 'node:fs';
import path from 'node:path';
const out=process.env.TDF_PILOT_OUT,password='Synthetic-activation-42!',email='private-identity-938421@pilot.example.test';
const browser=await chromium.launch();let current;let result={humanParticipants:0,profilePutRequests:0,privateMarkerPublic:false};
try{
 const {context:c,page}=await context(browser,{},true);current=page;
 c.on('request',r=>{if(r.url()===api+'/fans/me/artist-profile'&&r.method()==='PUT')result.profilePutRequests++;});
 await page.goto(base+'/tdf');await page.getByRole('link',{name:'Crear perfil de artista',exact:true}).click();
 const dialog=page.getByRole('dialog');await expect(dialog).toBeVisible();await dialog.locator('input[autocomplete="email"]').fill(email);await dialog.locator('input[autocomplete="new-password"]').fill(password);await dialog.locator('button[type="submit"]').click();
 await expect(page).not.toHaveURL(/\/login/,{timeout:30000});
 if(new URL(page.url()).pathname==='/artista/crear')await page.getByRole('button',{name:'Crear mi perfil de artista',exact:true}).click();
 await expect(page).toHaveURL(base+'/mi-artista',{timeout:30000});
 await expect(page.getByLabel('Nombre artístico',{exact:true})).not.toHaveValue('');result.editorInitialName=await page.getByLabel('Nombre artístico',{exact:true}).inputValue();
 const login=await fetch(api+'/login',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({username:email,password})});assert.equal(login.status,200);const account=await login.json();result.artistId=account.partyId;
 const guest=await browser.newContext();const profile=await guest.request.get(api+'/artists/'+account.partyId+'/public');result.anonymousProfileStatus=profile.status();
 if(profile.ok()){const p=await profile.json();result.publicName=p.apDisplayName;result.privateMarkerPublic=p.apDisplayName?.includes('938421')??false;}
 const search=await guest.request.get(api+'/directory/search?entityType=artist&q=938421');result.anonymousSearchStatus=search.status();
 if(search.ok()){const p=await search.json();result.publicSearchNames=(p.items??[]).filter(x=>x.id==='artist-'+account.partyId).map(x=>x.title);result.privateMarkerPublic ||= result.publicSearchNames.some(x=>x.includes('938421'));}
 assert.equal(result.privateMarkerPublic,false);assert.equal(result.publicName,'Cuenta TDF');assert.equal(result.editorInitialName,'Cuenta TDF');assert.deepEqual(result.publicSearchNames,[]);assert.equal(result.profilePutRequests,0);await page.screenshot({path:path.join(out,'before-first-profile-save.png'),fullPage:true});save();await guest.close();await c.close();
 result.status='VERIFIED-isolated-neutral-default-before-profile-save';
}catch(error){result.status='FAILED';result.error=String(error);if(current)try{result.failureUrl=current.url();result.failureBody=(await current.locator('body').innerText()).slice(0,12000);await current.screenshot({path:path.join(out,'failure.png'),fullPage:true});}catch{}throw error;}finally{await browser.close();result.browserClosed=true;writeFileSync(path.join(out,'privacy.json'),JSON.stringify(result,null,2)+'\n');console.log(JSON.stringify(result));}
