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
async function setup({blocked=false}={}){
 const dom=new JSDOM(readFileSync(resolve(dir,'index.html'),'utf8'),{url:'https://api.tdfrecords.net/campaign-platform/',runScripts:'outside-only'}),calls=[];
 dom.window.HTMLDialogElement.prototype.showModal=function(){this.open=true;};
 dom.window.HTMLElement.prototype.scrollIntoView=function(){};
 dom.window.fetch=async(url,options={})=>{
  const path=String(url).replace('/campaign-platform','');calls.push({path,body:options.body?JSON.parse(options.body):null});
  let value;
  if(path==='/session')value={user:{name:'Artista',admin:false},usage:{used_bytes:0,limit_bytes:5e9,plans:{},notices:[]},accounts:[{id:account,platform:'tiktok',handle:'Artista',scope:'own'}],capabilities:{youtube:{},instagram:{},facebook:{},tiktok:{}}};
  else if(path==='/campaigns')value=[];
  else if(path.endsWith('/tiktok-creator')){
   if(blocked)return {ok:false,status:409,json:async()=>({error:'Aplicación pendiente de configurar'})};
   value={context:'opaque-context',creator:{creator_username:'artista',creator_nickname:'Nombre actual del artista',privacy_level_options:['PUBLIC_TO_EVERYONE','SELF_ONLY'],comment_disabled:true,duet_disabled:false,stitch_disabled:true,max_video_post_duration_sec:180}};
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
