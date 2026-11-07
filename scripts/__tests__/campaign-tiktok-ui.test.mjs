import assert from 'node:assert/strict';
import {test} from 'node:test';
import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {resolve} from 'node:path';
import vm from 'node:vm';
import {createRequire} from 'node:module';
const require=createRequire(import.meta.url),{JSDOM}=require('jsdom');
const root=resolve(fileURLToPath(new URL('../..',import.meta.url)));
const dir=resolve(root,'services/campaign-platform/ui');
const account='a'.repeat(32),asset='b'.repeat(32),image='c'.repeat(32);
async function setup({blocked=false,sandbox=false,scope='tdf'}={}){
 const prefix=sandbox?'/campaign-platform-sandbox':'/campaign-platform';
 const dom=new JSDOM(readFileSync(resolve(dir,'index.html'),'utf8'),{url:'https://api.tdfrecords.net'+prefix+'/',runScripts:'outside-only'}),calls=[];
 dom.window.HTMLDialogElement.prototype.showModal=function(){this.open=true;};
 dom.window.HTMLElement.prototype.scrollIntoView=function(){};
 dom.window.fetch=async(url,options={})=>{
  const path=String(url).replace(prefix,'');calls.push({path,body:options.body?JSON.parse(options.body):null});
  let value;
  if(path==='/session')value={user:{name:'Artista',admin:false},usage:{used_bytes:0,limit_bytes:5e9,plans:{},notices:[]},accounts:[{id:account,platform:'tiktok',handle:'Artista',scope:'own'}],capabilities:{youtube:{},instagram:{},facebook:{},tiktok:{},tiktok_sandbox_scope:scope}};
  else if(path==='/campaigns')value=[];
  else if(path.endsWith('/tiktok-creator')){
   if(blocked)return {ok:false,status:409,json:async()=>({error:'Aplicación pendiente de configurar'})};
   value={context:'opaque-context',creator:{creator_username:'artista',creator_nickname:'Nombre actual del artista',privacy_level_options:['PUBLIC_TO_EVERYONE','SELF_ONLY'],comment_disabled:true,duet_disabled:false,stitch_disabled:true,max_video_post_duration_sec:180}};
   if(sandbox){value.creator.environment='sandbox';value.creator.privacy_level_options=['SELF_ONLY'];}
  }else if(path.endsWith('/tiktok-consent'))value={review_token:'opaque-consent'};
  else throw Error('Unexpected network boundary '+path);
  return {ok:true,status:200,json:async()=>value};
 };
 const context=dom.getInternalVMContext();vm.runInContext(readFileSync(resolve(dir,'app.js'),'utf8'),context);vm.runInContext(readFileSync(resolve(dir,'tiktok.js'),'utf8'),context);
 await new Promise(r=>setTimeout(r,0));
 vm.runInContext(`current={id:'campaign',timezone:'America/Guayaquil',assets:[{id:'${asset}',status:'ready',name:'clip.mp4'},{id:'${image}',status:'ready',name:'slide.jpg'}]};pieceDialog();document.querySelector('#piece-form').elements.platform.value='tiktok';platformChange();document.querySelector('#piece-form').elements.account.value='${account}';document.querySelector('#piece-form').elements.asset.value='${asset}';`,context);
 await vm.runInContext('refreshTikTok()',context);
 return {dom,context,calls,document:dom.window.document,run:code=>vm.runInContext(code,context),close:()=>dom.window.close()};
}

test('TikTok sandbox labels private testing and keeps requests and previews in its isolated route',async()=>{
 const page=await setup({sandbox:true});try{
  const d=page.document;
  assert.match(d.querySelector('.intro').textContent,/SANDBOX.*Solo yo/);
  assert.match(d.querySelector('#tiktok-options').textContent,/SANDBOX.*Prueba privada/);
  assert.deepEqual([...d.querySelector('#tt-privacy').options].map(x=>x.value),['','SELF_ONLY']);
  assert.equal(d.querySelector('#tt-privacy').value,'');
  assert.equal(d.querySelector('#tt-preview video').getAttribute('src'),'/campaign-platform-sandbox/assets/'+asset);
  assert.deepEqual([...d.querySelector('#piece-form').elements.platform.options].map(x=>x.value),['tiktok']);
  assert.equal(d.querySelector('#connect').textContent,'Conectar tiktok');
 }finally{page.close();}
});

test('TikTok personal sandbox account is connected as its owner instead of a shared TDF destination',async()=>{
 const page=await setup({sandbox:true,scope:'own'});try{
  const input=page.document.querySelector('#tdf-channel');
  assert.equal(input.checked,false);
  assert.equal(input.disabled,true);
  assert.match(page.document.querySelector('.intro').textContent,/SANDBOX/);
 }finally{page.close();}
});

test('TikTok export uses current creator options without selecting privacy or interactions by default',async()=>{
 const page=await setup();try{
  const d=page.document;assert.match(d.querySelector('#tiktok-options').textContent,/Nombre actual del artista/);
  assert.equal(d.querySelector('#tt-privacy').value,'');assert.equal(d.querySelector('#tt-comment').checked,false);assert.equal(d.querySelector('#tt-comment').disabled,true);
  assert.equal(d.querySelector('#tt-duet').disabled,false);assert.equal(d.querySelector('#tt-duet').checked,false);assert.equal(d.querySelector('#tt-stitch').disabled,true);
  assert.equal(d.querySelector('#tt-commercial').checked,false);assert.equal(d.querySelector('#tt-authorize').checked,false);
  assert.equal(d.querySelector('#tt-music').checked,false);assert.equal(d.querySelector('#tt-preview video').getAttribute('src'),'/campaign-platform/assets/'+asset);
 }finally{page.close();}
});

test('TikTok authorization sends explicit consent and refuses missing manual privacy selection',async()=>{
 const page=await setup();try{
  page.run("document.querySelector('#tt-authorize').checked=true;");
  await assert.rejects(page.run("consentTikTok({settings:{}})"),/Selecciona/);
  page.run("document.querySelector('#tt-privacy').value='PUBLIC_TO_EVERYONE';document.querySelector('#tt-music').checked=true;document.querySelector('#tt-original').checked=true;document.querySelector('#tt-preview-confirmed').checked=true;");
  const value=await page.run(`consentTikTok({account:'${account}',format:'video',asset:'${asset}',copy:'Texto revisado',due_at:1,settings:{}})`);
  assert.equal(value.settings.tiktok.review_token,'opaque-consent');const sent=page.calls.find(x=>x.path.endsWith('/tiktok-consent')).body;
  assert.equal(sent.context,'opaque-context');assert.equal(sent.music_agreed,true);assert.equal(sent.preview_confirmed,true);assert.equal(sent.original_content_confirmed,true);assert.equal(sent.settings.tiktok.privacy,'PUBLIC_TO_EVERYONE');
 }finally{page.close();}
});

test('TikTok photo review previews selected photos, hides video interactions and restricts private branded content',async()=>{
 const page=await setup();try{
  page.run("document.querySelector('#piece-form').elements.format.value='photo';");await page.run('refreshTikTok()');
  const d=page.document;assert.equal(d.querySelector('#tt-duet'),null);assert.equal(d.querySelector('#tt-stitch'),null);
  assert.equal(d.querySelector('#tt-images').options.length,1);d.querySelector('#tt-images').options[0].selected=true;page.run('ttPreview()');
  assert.equal(d.querySelector('#tt-preview img').getAttribute('src'),'/campaign-platform/assets/'+image);
  d.querySelector('#tt-privacy').value='SELF_ONLY';d.querySelector('#tt-privacy').dispatchEvent(new page.dom.window.Event('change'));
  assert.equal(d.querySelector('#tt-paid').disabled,true);assert.equal(d.querySelector('#tt-privacy').value,'SELF_ONLY');
 }finally{page.close();}
});

test('TikTok drafts remain editable when the client is not configured and do not authorize an upload',async()=>{
 const page=await setup({blocked:true});try{
  assert.match(page.document.querySelector('#tiktok-options').textContent,/pendiente de configurar/);
  const data=await page.run('consentTikTok({settings:{}})');assert.equal(data.settings.tiktok.review_token,undefined);
  assert.equal(page.calls.filter(x=>x.path.endsWith('/tiktok-consent')).length,0);
 }finally{page.close();}
});


async function youtubeRecoveryFixture(scope='own'){
 const page=await setup();page.run(`session.user.id=1;session.user.admin=false;session.accounts=[{id:'yt',scope:'${scope}'}];current={id:'campaign',owner:1,version:4};open=async()=>{};`);
 const calls=[];page.dom.window.fetch=async(url,options)=>{calls.push({url,body:JSON.parse(options.body)});return {ok:true,status:200,json:async()=>({})};};
 const piece={id:'piece',version:3,external_id:'same-private-video',account:'yt',due_at:Date.now()/1000-7200,late_seconds:60};
 return {...page,calls,piece,invoke:()=>page.run('confirmYouTubeReview('+JSON.stringify(piece)+')')};
}
test('YouTube recovery requires a chosen future date and two explicit confirmations without posting on cancel',async()=>{
 const page=await youtubeRecoveryFixture();try{
  page.dom.window.prompt=()=>null;page.dom.window.confirm=()=>{throw Error('unexpected confirmation');};await page.invoke();assert.equal(page.calls.length,0);
  page.dom.window.prompt=()=>new Date(Date.now()+300000).toISOString();page.dom.window.confirm=()=>false;await page.invoke();assert.equal(page.calls.length,0);
  let confirms=0;page.dom.window.confirm=()=>++confirms===1;await page.invoke();assert.equal(confirms,2);assert.equal(page.calls.length,0);
  page.dom.window.confirm=()=>true;await page.invoke();assert.equal(page.calls.length,1);
  assert.deepEqual(page.calls[0].body,{piece:'piece',version:3,video_id:'same-private-video',checks_passed:true,campaign_version:4,expected_due_at:page.piece.due_at,reschedule_at:page.calls[0].body.reschedule_at,schedule_reapproved:true,rights_confirmed:true,reschedule_only:false});
 }finally{page.close();}
});
test('YouTube shared-channel schedule recovery does not claim Studio administrator approval',async()=>{
 const page=await youtubeRecoveryFixture('tdf');try{
  page.dom.window.prompt=()=>new Date(Date.now()+300000).toISOString();let text='';page.dom.window.confirm=message=>{text=message;return true;};
  await page.invoke();assert.equal(page.calls.length,1);assert.equal(page.calls[0].body.reschedule_only,true);assert.equal(page.calls[0].body.checks_passed,false);assert.match(text,/administrador/);
 }finally{page.close();}
});
test('YouTube recovery rejects a nonowner or an invalid date before any request',async()=>{
 const page=await youtubeRecoveryFixture();try{
  page.run('current.owner=99');await assert.rejects(page.invoke(),/propietario/);assert.equal(page.calls.length,0);page.run('current.owner=1');
  for(const value of ['not-a-date',new Date(Date.now()-1000).toISOString(),new Date(Date.now()+32*86400000).toISOString()]){page.dom.window.prompt=()=>value;await assert.rejects(page.invoke(),/fecha futura/);}
  assert.equal(page.calls.length,0);
 }finally{page.close();}
});
