"""Real-frame graphic templates. Text is supplied by the campaign; no synthetic imagery."""
import hashlib,json,os,subprocess,time,textwrap
from pathlib import Path
from domain import uid,Denied

def generate(store,config,cid,actor,source_id,artist,frames):
 root=Path(config['STATE_DIR'])/'assets';font='/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
 with store.db() as d:
  campaign=dict(d.execute('SELECT * FROM campaigns WHERE id=?',(cid,)).fetchone());source=dict(d.execute('SELECT * FROM assets WHERE id=?',(source_id,)).fetchone())
 items=[('S'+str(i+1).zfill(2),text,1080,1920) for i,text in enumerate(['La sesión completa ya tiene su lugar.','Escucha de cerca.','Quédate con la música.','Un momento para volver.','Sigue la interpretación.','Una pausa para escuchar.','Descubre la sesión completa.','Vuelve a darle play.'])]
 items += [('CAR'+str(i+1).zfill(2),text,1080,1350) for i,text in enumerate([artist,'Una sesión para escuchar de cerca.','Dale espacio a la interpretación.','Ponte audífonos y vuelve al detalle.','Conoce al equipo en los créditos.','Mira la sesión completa.'])]
 results={}
 for i,(name,headline,width,height) in enumerate(items):
  photo_height=720 if height==1920 else 600
  aid=store.reserve_asset(actor,cid,name+'.jpg',2_500_000);target=root/aid;photo=root/(uid()+'.jpg');label=root/(uid()+'.txt');heading=root/(uid()+'.txt');footer=root/(uid()+'.txt');at=frames[i%len(frames)]['start']
  subprocess.run(['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-filter_threads','1','-filter_complex_threads','1','-threads','1','-ss',str(at),'-i',str(root/source['path']),'-frames:v','1','-vf',f'scale={width}:{photo_height}:force_original_aspect_ratio=decrease,pad={width}:{photo_height}:(ow-iw)/2:(oh-ih)/2:color=0x160f12','-threads','1',str(photo)],check=True,capture_output=True,timeout=45)
  label.write_text('\n'.join(textwrap.wrap(headline[:70],40)));heading.write_text('\n'.join(textwrap.wrap(artist[:56],28)));footer.write_text('Sesión completa\nEn el enlace de la bio' if name.startswith('CAR') else 'Escucha la sesión completa')
  # Positions leave room for a native link sticker on Stories. Never assert a clickable image URL.
  photo_y=520 if height==1920 else 450;bottom=height-260;title_y=220 if height==1920 else 180;label_y=370 if height==1920 else 335;title_size=56 if height==1920 else 48
  vf=f'pad={width}:{height}:0:{photo_y}:color=0x160f12,drawtext=fontfile={font}:textfile={heading}:fontcolor=white:fontsize={title_size}:x=70:y={title_y},drawtext=fontfile={font}:textfile={label}:fontcolor=0xffb2a0:fontsize=33:x=70:y={label_y},drawtext=fontfile={font}:textfile={footer}:fontcolor=white:fontsize=38:x=70:y={bottom}'
  use_logo='tdf' in campaign['name'].lower() or 'tdf' in campaign['objective'].lower();command=['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-filter_threads','1','-filter_complex_threads','1','-i',str(photo)]
  if use_logo:
   command+=['-i',config['LOGO_PNG'],'-filter_complex',f'[0:v]{vf}[canvas];[1:v]scale=350:-1[logo];[canvas][logo]overlay=70:80']
  else:command+=['-vf',vf]
  command+=['-frames:v','1','-q:v','2','-threads','1','-f','image2','-update','1',str(target)];subprocess.run(command,check=True,capture_output=True,timeout=45);size=target.stat().st_size
  if size>2_500_000:raise Denied('El gráfico excede la reserva; se conservó para revisión.')
  with store.db(True) as d:d.execute('UPDATE assets SET size=?,uploaded=?,reserved=?,status="ready",sha256=? WHERE id=?',(size,size,size,hashlib.sha256(target.read_bytes()).hexdigest(),aid));store.audit(d,None,'graphic.generated',cid,aid,{'source_frame':at,'name':name,'width':width,'height':height,'review_required':True,'tdf_logo':use_logo})
  for f in (photo,label,heading,footer):f.unlink()
  results[name]=aid
 return results
