import assert from 'node:assert/strict';
import {chromium,devices,expect,base,api,image,context as pilotContext,observation,save as flushObservation} from './runtime.mjs';
import {writeFileSync} from 'node:fs';
import path from 'node:path';
const out=process.env.TDF_PILOT_OUT;
const password='Synthetic-activation-42!';
const outcome={scope:'Synthetic signup/artist editor and fan UI with real HTTP/PostgreSQL. Only Drive upload/media and one failed profile PUT are isolated adapters. No real provider, native device, human comprehension or production write claim.',artist:null,results:[],adapters:{uploads:0,profileFailures:0}};
async function request(path,{method='GET',body,token,status=200}={}){
 const r=await fetch(api+path,{method,headers:{...(token?{Authorization:`Bearer ${token}`}:{ }),...(body?{'Content-Type':'application/json'}:{})},...(body?{body:JSON.stringify(body)}:{})});
 const text=await r.text();assert.equal(r.status,status,`${method} ${path}: ${text}`);return !text?null:r.status>=400?text:JSON.parse(text);
}
const apiLogin=email=>request('/login',{method:'POST',body:{username:email,password}});
async function context(options={},artist=false){return (await pilotContext(browser,options,artist)).context;}
async function signupUi(page,email){
 const dialog=page.getByRole('dialog');await expect(dialog).toBeVisible();
 await expect(page.locator('#signup-consent-notice')).toBeVisible();
 await dialog.locator('input[autocomplete="email"]').fill(email);
 await dialog.locator('input[autocomplete="new-password"]').fill(password);
 await dialog.locator('button[type="submit"]').click();
 await expect(page).not.toHaveURL(/\/login/,{timeout:30000});
}
async function loginUi(page,email,redirect){
 await page.goto(base+'/login?redirect='+encodeURIComponent(redirect));
 await page.locator('input[autocomplete="username"]').fill(email);
 await page.locator('input[autocomplete="current-password"]').fill(password);
 await page.locator('button[type="submit"]').click();await expect(page).not.toHaveURL(/\/login/,{timeout:30000});
}
const browser=await chromium.launch();
try{
 const artistContext=await context({},true),page=artistContext.pages()[0];
 page.on('pageerror',e=>console.log('Synthetic artist page error'));
 await page.goto(base+'/');
 await expect(page).toHaveURL(base+'/inicio');
 const welcome=page.getByRole('region',{name:'Bienvenido a TDF Records',exact:true});
 await expect(welcome).toBeVisible();
 const photo=welcome.getByRole('img');
 await expect.poll(()=>photo.evaluate(element=>element.naturalWidth)).toBeGreaterThan(0);
 const anonymousSession=await artistContext.request.get(api+'/session');
 assert.equal(anonymousSession.status(),200);assert.equal(await anonymousSession.json(),null);
 await expect(page.getByRole('dialog')).toHaveCount(0);
 await welcome.getByRole('link',{name:'Conoce TDF y sus servicios',exact:true}).focus();
 await welcome.getByRole('link',{name:'Conoce TDF y sus servicios',exact:true}).press('Enter');
 await expect(page).toHaveURL(base+'/tdf');
 await page.getByRole('link',{name:'Conoce el estudio',exact:true}).click();
 await expect(page).toHaveURL(base+'/tdf#estudio');
 await expect(page.locator('#estudio').getByRole('heading')).toBeVisible();
 await page.getByRole('link',{name:'Explora nuestros servicios',exact:true}).click();
 await expect(page).toHaveURL(base+'/tdf#servicios');
 await expect(page.locator('#servicios').getByRole('heading',{level:2})).toBeVisible();
 await expect(page.locator('input[type="password"]')).toHaveCount(0);
 await page.screenshot({path:path.join(out,'welcome-studio-before-signup.png'),fullPage:false});
 outcome.welcome={anonymousSession:true,rootToInicio:true,imageLoaded:true,keyboardNavigation:true,studioAnchor:true,serviceAnchor:true,noSignupBeforeExplicitAction:true};
 await page.getByRole('link',{name:'Crear perfil de artista',exact:true}).click();
 await signupUi(page,'artist-ui@pilot.example.test');
 const buyerDraft=JSON.stringify({name:'Comprador ficticio',email:'buyer-draft@pilot.example.test'}),bookingDraft=JSON.stringify({name:'Reserva ficticia',email:'booking-draft@pilot.example.test'});
 await page.evaluate(({buyerDraft,bookingDraft})=>{
  sessionStorage.setItem('tdf-marketplace-buyer',buyerDraft);sessionStorage.setItem('tdf-public-booking-profile',bookingDraft);
  sessionStorage.setItem('tdf-session-personal-data-keys-v1',JSON.stringify(['tdf-marketplace-buyer','tdf-public-booking-profile']));
 },{buyerDraft,bookingDraft});
 if(new URL(page.url()).pathname==='/artista/crear')await page.getByRole('button',{name:'Crear mi perfil de artista',exact:true}).click();
 await expect(page).toHaveURL(base+'/mi-artista',{timeout:30000});
 const refreshedDrafts=await page.evaluate(()=>[sessionStorage.getItem('tdf-marketplace-buyer'),sessionStorage.getItem('tdf-public-booking-profile')]);
 assert.deepEqual(refreshedDrafts,[buyerDraft,bookingDraft],'Artist role activation must preserve same-party buyer and booking drafts');
 const artist=await apiLogin('artist-ui@pilot.example.test');
 await expect(page.getByLabel('Nombre artístico',{exact:true})).not.toHaveValue('');
 const genres=await request('/catalogs/genres/items?locale=es&page=1&pageSize=500');assert.ok(genres.items.length,'Migrated genre catalog must have a published value');
 const genre=genres.items[0];
 await page.getByLabel('Nombre artístico',{exact:true}).fill('Artista UI Sintética');
 await page.getByLabel('Slug público',{exact:true}).fill('activation-complete-ui');
 await page.getByLabel('Ciudad',{exact:true}).last().fill('Quito');
 await page.getByLabel('Bio',{exact:true}).last().fill('Biografía ficticia revisada antes de guardar.');
 await page.getByLabel('Spotify URL',{exact:true}).fill('https://open.spotify.com/artist/0000000000000000000000');
 await page.getByLabel('YouTube URL',{exact:true}).fill('https://www.youtube.com/@synthetic-activation');
 await page.getByLabel('Sitio web',{exact:true}).fill('https://example.test/synthetic-artist');
 const chooser=async()=>{await page.getByRole('button',{name:'Subir portada a Drive',exact:true}).click();};
 await chooser();await expect(page.getByText('Synthetic upload failure',{exact:true})).toBeVisible();await chooser();
 await expect(page.getByLabel('URL de portada (opcional)',{exact:true})).toHaveValue(image);
 await expect(page.getByRole('img',{name:'Vista previa',exact:true})).toBeVisible();
 const select=page.getByRole('combobox',{name:'Géneros',exact:true});await select.click();await page.getByRole('option',{name:genre.name,exact:true}).click();
 const before=await request('/fans/me/artist-profile',{token:artist.token});assert.notEqual(before.apBio,'Biografía ficticia revisada antes de guardar.');
 const save=page.getByRole('button',{name:/^(Actualizar perfil de artista|Guardar cambios)$/});await expect(save).toBeEnabled();await save.click();
 await expect(page.getByText('Synthetic save failure',{exact:true})).toBeVisible();
 await expect(page.getByLabel('Bio',{exact:true}).last()).toHaveValue('Biografía ficticia revisada antes de guardar.');
 await expect(page.getByLabel('URL de portada (opcional)',{exact:true})).toHaveValue(image);
 await save.click();await expect(page.getByText('Perfil de artista actualizado.',{exact:true})).toBeVisible();
 const profile=await request('/fans/me/artist-profile',{token:artist.token});
 assert.equal(profile.apDisplayName,'Artista UI Sintética');assert.equal(profile.apSlug,'activation-complete-ui');assert.equal(profile.apHeroImageUrl,image);assert.deepEqual(profile.apGenreIds,[genre.id]);assert.equal(profile.apCity,'Quito');assert.equal(profile.apBio,'Biografía ficticia revisada antes de guardar.');
 const canonical=await page.getByRole('button',{name:'Copiar enlace',exact:true}).count();assert.equal(canonical,1);
 const publicProfile=await request('/artists/activation-complete-ui/public');assert.equal(publicProfile.apHeroImageUrl,image);
 const found=await request('/directory/search?entityType=artist&q=Artista%20UI%20Sint%C3%A9tica');assert.ok(found.items.some(x=>x.id===`artist-${artist.partyId}`));
 await artistContext.close();outcome.artist={partyId:artist.partyId,signupUi:true,activationUi:true,editorFields:true,publishedGenreId:genre.id,uploadRecovery:true,saveRecovery:true,unsavedNotPublic:true,canonicalLink:true,publicSearch:true,profile};
 for(const[mode,options]of[['desktop',{}],['mobile-web',devices['Pixel 7']]]){
  const c=await context(options),fanPage=c.pages()[0];const email=`fan-ui-${mode}@pilot.example.test`;
  await expect(fanPage.getByRole('region',{name:'Bienvenido a TDF Records',exact:true})).toBeVisible();
  await expect.poll(()=>fanPage.getByRole('region',{name:'Bienvenido a TDF Records',exact:true}).getByRole('img').evaluate(element=>element.naturalWidth)).toBeGreaterThan(0);
  await fanPage.screenshot({path:path.join(out,'welcome-'+mode+'.png'),fullPage:false});
  await fanPage.goto(base+'/a/activation-complete-ui');await fanPage.getByRole('link',{name:'Crear cuenta o ingresar para seguir',exact:true}).click();
  assert.equal(new URL(new URL(fanPage.url()).searchParams.get('redirect'),base).pathname,'/a/activation-complete-ui');
  await signupUi(fanPage,email);await expect(fanPage).toHaveURL(/\/a\/activation-complete-ui/);
  const fan=await apiLogin(email);const follow=fanPage.getByRole('button',{name:'Seguir ahora',exact:true});
  if(await follow.count())await follow.click();
  await expect(fanPage.getByRole('button',{name:'Dejar de seguir a Artista UI Sintética',exact:true})).toBeEnabled();
  for(let n=0;n<5;n++)await request(`/fans/me/follows/${artist.partyId}`,{method:'POST',body:{},token:fan.token});
  const follows=await request('/fans/me/follows',{token:fan.token});assert.equal(follows.length,1);assert.equal(follows[0].ffArtistId,artist.partyId);
  await fanPage.goto(base+'/fans');await expect(fanPage.getByText('Artista UI Sintética',{exact:true}).first()).toBeVisible();
  const activeSession=await c.request.get(api+'/session');assert.equal(activeSession.status(),200);assert.equal((await activeSession.json()).partyId,fan.partyId);
  const originalCookies=(await c.cookies(api)).map(x=>`${x.name}=${x.value}`).join('; ');
  assert.equal((await(await fetch(api+'/session',{headers:{Cookie:originalCookies}})).json()).partyId,fan.partyId);
  const logoutResponse=fanPage.waitForResponse(r=>r.url()===api+'/session/logout'&&r.request().method()==='POST');
  await fanPage.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();
  await fanPage.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();
  const loggedOut=await logoutResponse;assert.ok([200,204].includes(loggedOut.status()));
  await expect(fanPage).toHaveURL(/\/login/);const afterLogout=await c.request.get(api+'/session');assert.equal(afterLogout.status(),200);assert.equal(await afterLogout.json(),null);
  assert.equal(await(await fetch(api+'/session',{headers:{Cookie:originalCookies}})).json(),null);
  await c.close();
  const returnOptions=mode==='desktop'?devices['Pixel 7']:{};const laterContext=await context(returnOptions),later=laterContext.pages()[0];await loginUi(later,email,'/a/activation-complete-ui');
  await expect(later.getByRole('button',{name:'Dejar de seguir a Artista UI Sintética',exact:true})).toBeEnabled();await laterContext.close();
  outcome.results.push({mode,partyId:fan.partyId,artistId:artist.partyId,signupUi:true,authReturn:true,followPersisted:true,community:true,logoutNewContextLogin:true,logoutUi:true,browserSessionAbsentAfterLogout:true,originalCookieRevoked:true,crossWebFormFactor:mode==='desktop'?'desktop-to-mobile-web':'mobile-web-to-desktop'});
 }
 const artistReturn=await context({},true),artistPage=artistReturn.pages()[0];await loginUi(artistPage,'artist-ui@pilot.example.test','/mi-artista');
 await expect(artistPage.getByText('2 fans',{exact:true}).first()).toBeVisible();await artistReturn.close();
 outcome.artistFollowerCount=(await request('/fans/me/artist-profile',{token:artist.token})).apFollowerCount;assert.equal(outcome.artistFollowerCount,2);
 flushObservation();writeFileSync(path.join(out,'journey.json'),JSON.stringify({status:'VERIFIED-synthetic-only',humanParticipants:0,artistId:artist.partyId,fanIds:outcome.results.map(x=>x.partyId),syntheticFans:2,uiSignup:true,profilePublication:true,uploadRecovery:true,saveRecovery:true,community:true,crossWebFormFactorRelogin:true,logoutUi:true,browserSessionAbsentAfterLogout:true,originalCookieRevoked:true,artistUiFollowerCount:2,googleProviderTested:false,nativeDevicesTested:false},null,2)+'\n');console.log('Complete synthetic artist editor/upload/save recovery + fan signup/follow/relogin + artist UI count passed.');
 const diagnosticContext=await context(),diagnosticPage=diagnosticContext.pages()[0];
 await loginUi(diagnosticPage,'fan-ui-desktop@pilot.example.test','/fans');
 const actorBefore=await diagnosticContext.request.get(api+'/session');assert.equal(actorBefore.status(),200);const actorBeforeRecord=await actorBefore.json();const partyBefore=actorBeforeRecord.partyId;
 const failure=diagnosticPage.waitForEvent('requestfailed',r=>r.url()===api+'/session/logout');
 await diagnosticContext.route(api+'/session/logout',route=>route.abort('internetdisconnected'));
 await diagnosticPage.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();await diagnosticPage.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();await failure;
 await expect(diagnosticPage).toHaveURL(/\/login/);
 const authorityAfter=await diagnosticContext.request.get(api+'/session');assert.equal(authorityAfter.status(),200);assert.equal((await authorityAfter.json()).partyId,partyBefore);
 await expect(diagnosticPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toBeVisible();
 await diagnosticPage.reload();await expect(diagnosticPage).toHaveURL(/\/login/);
 await expect(diagnosticPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toBeVisible();
 assert.equal(await diagnosticPage.getByRole('button',{name:'Abrir menú de sesión',exact:true}).count(),0);
 const preserved=await diagnosticContext.request.get(api+'/session');assert.equal((await preserved.json()).partyId,partyBefore);
 await diagnosticPage.goto(base+'/a/activation-complete-ui');await expect(diagnosticPage.getByRole('link',{name:'Crear cuenta o ingresar para seguir',exact:true})).toBeVisible();
 await diagnosticPage.goto(base+'/login');
 await diagnosticContext.unroute(api+'/session/logout');
 const retryResponse=diagnosticPage.waitForResponse(r=>r.url()===api+'/session/logout'&&r.request().method()==='POST');
 await diagnosticPage.getByRole('button',{name:'Reintentar cierre',exact:true}).click();assert.ok([200,204].includes((await retryResponse).status()));
 await expect(diagnosticPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toHaveCount(0);
 assert.equal(await(await diagnosticContext.request.get(api+'/session')).json(),null);
 await loginUi(diagnosticPage,'fan-ui-desktop@pilot.example.test','/a/activation-complete-ui');
 await expect(diagnosticPage.getByRole('button',{name:'Dejar de seguir a Artista UI Sintética',exact:true})).toBeEnabled();
 // A second failure recovers through fresh credentials, with another tab already open.
 const otherTab=await diagnosticContext.newPage();await otherTab.goto(base+'/fans');await expect(otherTab.getByRole('button',{name:'Abrir menú de sesión',exact:true})).toBeVisible();
 await diagnosticContext.route(api+'/session/logout',route=>route.abort('internetdisconnected'));
 await diagnosticPage.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();await diagnosticPage.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();
 await expect(diagnosticPage).toHaveURL(/\/login/);
 await expect(otherTab.getByRole('button',{name:'Abrir menú de sesión',exact:true})).toHaveCount(0);
 assert.ok(await otherTab.locator('a[href^="/login"]').count()>0);
 assert.notEqual(await otherTab.evaluate(()=>JSON.parse(localStorage.getItem('tdf-hq-ui/logout-intent/v1')).status),'cleared');
 await expect(diagnosticPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toBeVisible();
 await diagnosticContext.unroute(api+'/session/logout');
 await loginUi(diagnosticPage,'fan-ui-desktop@pilot.example.test','/a/activation-complete-ui');
 await expect(diagnosticPage.getByRole('button',{name:'Dejar de seguir a Artista UI Sintética',exact:true})).toBeEnabled();
 // Cookie-only bootstrap remains valid after a fresh explicit login has cleared intent.
 const bootstrapResponse=diagnosticPage.waitForResponse(r=>r.url()===api+'/session'&&r.request().method()==='GET');
 await diagnosticPage.reload();assert.equal((await(await bootstrapResponse).json()).partyId,partyBefore);await expect(diagnosticPage.getByRole('button',{name:'Dejar de seguir a Artista UI Sintética',exact:true})).toBeEnabled();
 // A bearer-only browser must revoke the same real target on explicit retry.
 const bearerContext=await context(),bearerPage=bearerContext.pages()[0];
 const bearerLoginResponse=bearerPage.waitForResponse(r=>r.url()===api+'/login'&&r.request().method()==='POST');
 await loginUi(bearerPage,'fan-ui-desktop@pilot.example.test','/a/activation-complete-ui');
 const bearerToken=(await(await bearerLoginResponse).json()).token;assert.ok(bearerToken);
 await bearerContext.clearCookies();assert.equal(await(await bearerContext.request.get(api+'/session')).json(),null);
 assert.equal((await(await fetch(api+'/session',{headers:{Authorization:`Bearer ${bearerToken}`}})).json()).partyId,partyBefore);
 await bearerContext.route(api+'/session/logout',route=>route.abort('internetdisconnected'));
 await bearerPage.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();await bearerPage.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();
 await expect(bearerPage).toHaveURL(/\/login/);await expect(bearerPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toBeVisible();
 assert.equal((await(await fetch(api+'/session',{headers:{Authorization:`Bearer ${bearerToken}`}})).json()).partyId,partyBefore);
 await bearerContext.unroute(api+'/session/logout');
 const bearerRetry=bearerPage.waitForResponse(r=>r.url()===api+'/session/logout'&&r.request().method()==='POST');
 await bearerPage.getByRole('button',{name:'Reintentar cierre',exact:true}).click();assert.ok([200,204].includes((await bearerRetry).status()));
 assert.equal(await(await fetch(api+'/session',{headers:{Authorization:`Bearer ${bearerToken}`}})).json(),null,'Explicit retry must revoke the original bearer even with no cookie');
 await expect(bearerPage.getByText(/no pudimos confirmar el cierre en el servidor/)).toHaveCount(0);await bearerContext.close();
 // Model delayed cross-document storage delivery while using real A/B cookie sessions.
 await otherTab.goto(base+'/fans');await expect(otherTab.getByRole('button',{name:'Abrir menú de sesión',exact:true})).toBeVisible();
 await diagnosticPage.evaluate(()=>{
  window.__tdfLogoutQueue={paused:true,events:[]};
  window.addEventListener('storage',event=>{
   if(window.__tdfLogoutQueue.paused&&event.key==='tdf-hq-ui/logout-intent/v1'){
    window.__tdfLogoutQueue.events.push({key:event.key,oldValue:event.oldValue,newValue:event.newValue});event.stopImmediatePropagation();
   }
  },true);
 });
 const remoteLogout=otherTab.waitForResponse(r=>r.url()===api+'/session/logout'&&r.request().method()==='POST');
 await otherTab.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();await otherTab.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();assert.ok([200,204].includes((await remoteLogout).status()));
 await loginUi(otherTab,'fan-ui-mobile-web@pilot.example.test','/fans');
 const actualOtherActor=await(await diagnosticContext.request.get(api+'/session')).json();assert.equal(actualOtherActor.partyId,outcome.results[1].partyId);
 const queuedStatuses=await diagnosticPage.evaluate(()=>{
  const state=window.__tdfLogoutQueue;state.paused=false;const events=state.events.splice(0);
  events.forEach(event=>window.dispatchEvent(new StorageEvent('storage',event)));
  return events.map(event=>JSON.parse(event.newValue).status);
 });
 assert.ok(queuedStatuses.includes('cleared'));assert.ok(queuedStatuses.some(x=>x==='pending'||x==='confirmed'||x==='unconfirmed'));
 await expect(diagnosticPage.getByRole('button',{name:'Abrir menú de sesión',exact:true})).toHaveCount(0);
 await expect(diagnosticPage.getByRole('link',{name:'Crear cuenta o ingresar para seguir',exact:true})).toBeVisible();
 const finalResponse=otherTab.waitForResponse(r=>r.url()===api+'/session/logout'&&r.request().method()==='POST');
 await otherTab.getByRole('button',{name:'Abrir menú de sesión',exact:true}).click();await otherTab.getByRole('menuitem',{name:'Cerrar sesión',exact:true}).click();assert.ok([200,204].includes((await finalResponse).status()));
 assert.equal(await(await diagnosticContext.request.get(api+'/session')).json(),null);
 await diagnosticContext.close();
 writeFileSync(path.join(out,'logout-diagnostic.json'),JSON.stringify({status:'VERIFIED-isolated-failed-logout-recovery',humanParticipants:0,logoutClickedInUi:true,logoutNetworkFailureInjected:true,serverCookieRemainedValidDuringFailure:true,reloadRemainsAnonymous:true,unsignedArtistStillReadable:true,explicitRetryRevokesServerCookie:true,freshCredentialsRecoverAfterFailure:true,otherTabSignsOut:true,bearerOnlyRetryRevokesOriginalToken:true,noCookieAvailableDuringBearerRetry:true,queuedLogoutThenLoginDiscardsOldTabScope:true,realSecondCookieActorConfirmed:true,cookieOnlyBootstrapAfterFreshLogin:true,successLogoutUiVerified:true,samePartyArtistActivationPreservesBuyerAndBookingDrafts:true,serverReal:true,productionWrites:false,scope:'Real native browser/API/PostgreSQL. Logout POST failure and queued storage-event delivery are explicit adapters; cookie removal models a bearer-only browser; no offline revocation or full Set-Cookie ordering claim.'},null,2)+'\n');
 writeFileSync(path.join(out,'welcome.json'),JSON.stringify({welcome:outcome.welcome,failedLogoutPrivacyFixTested:true,humanPilotHeld:true},null,2)+'\n');
 console.log('Failed logout remains anonymous across reload; real retry revokes; fresh login and cross-tab privacy pass.');
}catch(error){writeFileSync(path.join(out,'journey-failure.json'),JSON.stringify({scope:outcome.scope,adapters:outcome.adapters,error:error.message},null,2)+'\n');throw error;}finally{await browser.close();}