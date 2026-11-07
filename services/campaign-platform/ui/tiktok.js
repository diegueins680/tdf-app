'use strict';
let tiktokContext=null,tiktokInfo=null,tiktokRequest=0;
const TT_PRIVACY={PUBLIC_TO_EVERYONE:'Todo el mundo',MUTUAL_FOLLOW_FRIENDS:'Amistades mutuas',FOLLOWER_OF_CREATOR:'Seguidores',SELF_ONLY:'Solo yo'};
function ttCheck(id,text,parent){const label=el('label'),input=el('input',undefined,{type:'checkbox',id});label.append(input,document.createTextNode(' '+text));parent.append(label);return input;}
function ttOptions(){const f=$('#piece-form');return {privacy:$('#tt-privacy')?.value||'',allow_comment:!!$('#tt-comment')?.checked,allow_duet:f.elements.format.value==='video'&&!!$('#tt-duet')?.checked,allow_stitch:f.elements.format.value==='video'&&!!$('#tt-stitch')?.checked,commercial:!!$('#tt-commercial')?.checked,brand_own:!!$('#tt-own')?.checked,brand_paid:!!$('#tt-paid')?.checked,is_aigc:!!$('#tt-aigc')?.checked,title:$('#tt-title')?.value||'',images:[...($('#tt-images')?.selectedOptions||[])].map(x=>x.value),cover_index:Number($('#tt-cover')?.value||0)};}
async function refreshTikTok(){
 const serial=++tiktokRequest,f=$('#piece-form'),root=$('#tiktok-options');tiktokContext=null;tiktokInfo=null;root.replaceChildren();root.hidden=f.elements.platform.value!=='tiktok';
 if(root.hidden)return;
 const note=el('p','Selecciona una cuenta y revisa sus opciones actuales. TikTok puede tardar unos minutos en procesar el contenido después del envío.');root.append(el('h3','Revisar para TikTok'),note,el('p','Puedes guardar un borrador. El envío requiere revisar los archivos y autorizar por separado esta pieza para TikTok.'));
 root.append(action('Actualizar opciones de TikTok',refreshTikTok));
 if(!f.elements.account.value){note.textContent='Conecta y selecciona la cuenta de TikTok que administras.';return;}
 const account=f.elements.account.value;
 try{
  const response=await post('/campaigns/'+current.id+'/tiktok-creator',{account});
  if(serial!==tiktokRequest||account!==f.elements.account.value)return;
  tiktokContext=response.context;tiktokInfo=response.creator;note.textContent='Publicarás en '+tiktokInfo.creator_nickname+' (@'+tiktokInfo.creator_username+'). Duración máxima: '+tiktokInfo.max_video_post_duration_sec+' segundos.';
  if(tiktokInfo.environment==='sandbox')note.textContent='SANDBOX · Prueba privada «Solo yo». '+note.textContent;
  const label=el('label','Quién podrá ver esta pieza'),select=el('select',undefined,{id:'tt-privacy',required:true});fillOptions(select,tiktokInfo.privacy_level_options.map(x=>[x,TT_PRIVACY[x]]),'Selecciona la visibilidad');select.required=false;label.append(select);root.append(label);
  const interactions=el('div');ttCheck('tt-comment','Permitir comentarios',interactions).disabled=tiktokInfo.comment_disabled;
  if(f.elements.format.value==='video'){ttCheck('tt-duet','Permitir Duet',interactions).disabled=tiktokInfo.duet_disabled;ttCheck('tt-stitch','Permitir Stitch',interactions).disabled=tiktokInfo.stitch_disabled;}
  root.append(interactions);
  if(f.elements.format.value==='photo'){
   const tl=el('label','Título de las fotos (opcional)'),title=el('input',undefined,{id:'tt-title',maxlength:90});tl.append(title);root.append(tl);
   const photos=el('label','Fotos, en el orden mostrado en la lista'),images=el('select',undefined,{id:'tt-images',multiple:true,size:5});fillOptions(images,current.assets.filter(a=>a.status==='ready'&&/\.(jpe?g|webp)$/i.test(a.name)).map(a=>[a.id,a.name]));photos.append(images);root.append(photos);
   const coverLabel=el('label','Número de portada, empezando por 0'),cover=el('input',undefined,{id:'tt-cover',type:'number',min:0,value:0});coverLabel.append(cover);root.append(coverLabel);
   root.append(el('p','Se conservarán las fotos sin añadir música automáticamente. Revisa la selección completa antes de autorizarla.'));
  }
  const commercial=ttCheck('tt-commercial','Este contenido promociona una marca, producto o servicio',root),brands=el('div');brands.hidden=true;
  const own=ttCheck('tt-own','Mi marca o mi actividad',brands),paid=ttCheck('tt-paid','Colaboración con otra marca',brands),brandNote=el('p');brands.append(brandNote);root.append(brands);
  const update=()=>{brands.hidden=!commercial.checked;if(!commercial.checked){own.checked=false;paid.checked=false;}paid.disabled=select.value==='SELF_ONLY';if(paid.disabled)paid.checked=false;brandNote.textContent=paid.checked?'TikTok etiquetará el contenido como colaboración pagada.':own.checked?'TikTok etiquetará el contenido como promocional.':'Indica qué marca se promociona. La colaboración con otra marca no admite Solo yo.';};[commercial,own,paid,select].forEach(x=>x.addEventListener('change',update));
  ttCheck('tt-aigc','Este material requiere la etiqueta de contenido generado con IA',root);
  const preview=el('div',undefined,{id:'tt-preview'});root.append(preview);ttPreview();
  ttCheck('tt-authorize','Autorizar el envío de esta pieza a TikTok cuando se ejecute su versión aprobada',root);
  ttCheck('tt-preview-confirmed','Revisé la vista previa y autorizo enviar este archivo y este texto a la cuenta indicada',root);
  const original=ttCheck('tt-original','Revisé la originalidad, los derechos y las reglas de marcas de agua de TikTok para este material',root);original.parentElement.append(document.createTextNode(' '),el('a','Consultar reglas',{href:'https://developers.tiktok.com/docs/en/content-sharing-guidelines',target:'_blank',rel:'noopener'}));
  const music=ttCheck('tt-music','Al publicar, acepto la confirmación de uso de música de TikTok',root);music.parentElement.append(document.createTextNode(' '),el('a','Confirmación de uso de música',{href:'https://www.tiktok.com/legal/page/global/music-usage-confirmation/en',target:'_blank',rel:'noopener'}));
  const brandPolicy=ttCheck('tt-brand-policy','Acepto la política de contenido de marca cuando corresponde',root);brandPolicy.parentElement.append(document.createTextNode(' '),el('a','Política de contenido de marca',{href:'https://www.tiktok.com/legal/page/global/bc-policy/en',target:'_blank',rel:'noopener'}));
  $('#tt-images')?.addEventListener('change',()=>{ttPreview();$('#tt-preview-confirmed').checked=false;});
 }catch(error){if(serial===tiktokRequest)note.textContent=error.message;}
}
function ttPreview(){const root=$('#tt-preview');if(!root)return;root.replaceChildren();const f=$('#piece-form'),ids=f.elements.format.value==='photo'?ttOptions().images:[f.elements.asset.value];for(const id of ids){const asset=current.assets.find(a=>a.id===id);if(!asset)continue;const media=el(f.elements.format.value==='photo'?'img':'video',undefined,{src:BASE+'/assets/'+asset.id});if(media.tagName==='VIDEO'){media.controls=true;media.preload='metadata';}else media.alt=asset.name;root.append(el('p',asset.name),media);}}
async function consentTikTok(data){
 if(!$('#tt-authorize')?.checked){data.settings={...data.settings,tiktok:ttOptions()};return data;}
 if(!tiktokContext||!tiktokInfo)throw Error('Consulta las opciones actuales de TikTok antes de autorizar.');
 const value=ttOptions();if(!value.privacy)throw Error('Selecciona quién podrá ver esta pieza en TikTok.');
 if(value.commercial&&!value.brand_own&&!value.brand_paid)throw Error('Indica qué marca se promociona.');
 data.settings={...data.settings,tiktok:value};
 const response=await post('/campaigns/'+current.id+'/tiktok-consent',{...data,context:tiktokContext,music_agreed:$('#tt-music').checked,preview_confirmed:$('#tt-preview-confirmed').checked,original_content_confirmed:$('#tt-original').checked,brand_policy_agreed:$('#tt-brand-policy').checked});
 data.settings.tiktok.review_token=response.review_token;return data;
}
$('#piece-form').elements.account.addEventListener('change',()=>perform(refreshTikTok));
$('#piece-form').elements.format.addEventListener('change',()=>perform(refreshTikTok));
$('#piece-form').elements.asset.addEventListener('change',()=>{ttPreview();if($('#tt-preview-confirmed'))$('#tt-preview-confirmed').checked=false;});
