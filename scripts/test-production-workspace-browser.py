#!/usr/bin/env python3
"""Run real HTTP, playback and mobile-browser checks on a disposable production-schema restore."""
import argparse
from contextlib import ExitStack
from datetime import date, timedelta
import hashlib
import io
import json
import math
import os
from pathlib import Path
import secrets
import shutil
import socket
import struct
import subprocess
import tempfile
import time
from urllib.request import urlopen
from uuid import uuid4
import wave
from playwright.sync_api import expect, sync_playwright

ROOT = Path(__file__).resolve().parents[1]


def free_port():
    with socket.socket() as probe:
        probe.bind(('127.0.0.1', 0))
        return probe.getsockname()[1]


def waveform(frequency):
    out = io.BytesIO()
    with wave.open(out, 'wb') as wav:
        wav.setparams((1, 2, 16000, 0, 'NONE', ''))
        wav.writeframes(b''.join(struct.pack('<h', int(12000*math.sin(2*math.pi*frequency*i/16000))) for i in range(16000)))
    return out.getvalue()


def scenario(browser, api, web, actors, offering):
    accounts = {}
    for name, (party, token) in actors.items():
        ctx = browser.new_context(viewport={'width':390 if name=='Artista' else 1100,'height':844},locale='es-EC')
        ctx.add_cookies([dict(name='tdf_session',value=token,url=web,httpOnly=True,sameSite='Lax')])
        accounts[name] = ctx

    def http(who, method, path, expected=200, **args):
        res = accounts[who].request.fetch(api+path, method=method, **args)
        assert res.status==expected, (method,path,res.status,res.text()[:350])
        return res

    def project(who):
        return next(p for p in http(who,'GET','/productions').json() if p['id']==pid)

    def action(who, **body):
        return http(who,'POST',f'/productions/{pid}/actions',data={'revision':project(who)['revision'],**body}).json()['id']

    def mix(who):
        return next(t for t in project(who)['tasks'] if t['id']==mix_id)

    def verify_state(state, owner):
        for name in ['Ingeniera','Artista']:
            t=mix(name)
            next_actor=t['approver'] if t['status']=='En revisión' else None if t['status']=='Completada' else t['owner']
            assert (t['status'],next_actor)==(state,owner)
            pg,card=task_page(name)
            label=next((n for n,(party,_) in actors.items() if party==owner),None)
            expect(card.get_by_text('Próxima acción: '+label if label else 'Sin acción pendiente',exact=False)).to_be_visible()
            pg.get_by_role('button',name='Mis tareas',exact=True).click()
            expect(pg.get_by_role('heading',name='Mezcla de Luna',exact=True)).to_have_count(int(owner==actors[name][0]))
            pg.close()

    producer, engineer, artist = [actors[n][0] for n in ['Productor','Ingeniera','Artista']]
    today=date.today()
    delivery=(today+timedelta(days=60)).isoformat()
    recording=(today+timedelta(days=20)).isoformat()
    page=accounts['Productor'].new_page()
    page.goto(web+'/producciones')
    expect(page.get_by_text('Nueva producción',exact=True)).to_be_visible(timeout=90000)
    page.get_by_text('Nueva producción',exact=True).click()
    page.get_by_label('Nombre del proyecto').fill('EP · Tres canciones')
    page.get_by_label('Formato',exact=True).select_option('EP')
    page.get_by_label('Entrega final').fill(delivery)
    page.get_by_label('Canciones, una por línea').fill('Luna\nMar\nSal')
    with page.expect_response(lambda res: res.url==api+'/productions' and res.request.method=='POST') as created:
        page.get_by_role('button',name='Crear producción privada').click()
    assert created.value.status==200, created.value.text()
    pid=created.value.json()['id']
    page.get_by_text('Invitar al equipo',exact=True).click()
    for username,role in [('workspace-engineer','engineer'),('workspace-artist','client')]:
        page.get_by_label('Usuario de la cuenta TDF').fill(username)
        page.get_by_label('Permiso del proyecto').select_option(role)
        with page.expect_response(lambda r:r.url.endswith('/actions')) as invitation:
            page.get_by_role('button',name='Dar acceso al proyecto').click()
        assert invitation.value.status==200,invitation.value.text()
    booked=http('Productor','POST','/bookings',data=dict(cbTitle='Batería EP',cbStartsAt=recording+'T09:00:00-05:00',
        cbEndsAt=recording+'T12:00:00-05:00',cbStatus='Confirmed',cbServiceOfferingId=offering,cbEngineerPartyId=engineer)).json()['bookingId']
    p=project('Productor')
    form=page.locator('details').filter(has=page.get_by_text('Nueva tarea de proyecto o de canciones',exact=True)).last
    form.locator('summary').click()
    form.locator('[name=title]').fill('Batería compartida')
    form.get_by_label('Etapa o hito').fill('Grabación')
    form.get_by_label('Fecha comprometida').fill(recording)
    form.get_by_label('Canciones (vacío para todo el proyecto)').select_option([s['id'] for s in p['songs']])
    form.get_by_label('Responsable',exact=True).select_option(str(engineer))
    form.get_by_label('Número de reserva').fill(str(booked))
    with page.expect_response(lambda r:r.url.endswith('/actions')) as created_task:
        form.get_by_role('button',name='Añadir tarea').click()
    assert created_task.value.status==200,created_task.value.text()
    drums=created_task.value.json()['id']
    pg=accounts['Ingeniera'].new_page()
    pg.goto(web+'/producciones')
    cards=pg.locator('.MuiCard-root').filter(has=pg.get_by_role('heading',name='Batería compartida',exact=True))
    expect(cards).to_have_count(3)
    cards.first.locator('summary').first.click()
    with pg.expect_response(lambda r:r.url.endswith('/actions')) as completed:
        cards.first.get_by_role('button',name='Completar',exact=True).click()
    assert completed.value.status==200,completed.value.text()
    expect(cards.get_by_text('Grabación · Completada',exact=False)).to_have_count(3)
    pg.close()
    shared=next(t for t in project('Ingeniera')['tasks'] if t['id']==drums)
    assert len(shared['songs'])==3 and shared['booking']['id']==booked and shared['status']=='Completada'
    song=p['songs'][0]['id']
    tracks=[t for t in p['tasks'] if song in t['songs']]
    for title in ['Preproducción','Grabación','Edición']:
        action('Productor',op='progress',task=next(t['id'] for t in tracks if t['title']==title),status='Completada')
    t=next(t for t in tracks if t['title']=='Mezcla')
    mix_id=t['id']
    action('Productor',op='edit',task=mix_id,title='Mezcla de Luna',stage=t['stage'],owner=engineer,approver=artist,
           duration=t['duration'],due=t['due'],songs=[song])
    assert mix('Ingeniera')['owner']==engineer and not project('Artista')['tasks']

    def task_page(name):
        page=accounts[name].new_page()
        page.goto(web+'/producciones')
        card=page.locator('.MuiCard-root').filter(has=page.get_by_role('heading',name='Mezcla de Luna',exact=True)).last
        card.locator('summary').first.click(timeout=45000)
        return page,card

    def publish(number, tone):
        pg,card=task_page('Ingeniera')
        card.get_by_text('Entregar versión de audio').click()
        card.get_by_label('Número de versión').fill(str(number))
        card.locator('input[type=file]').set_input_files(dict(name=f'mezcla-v{number}.wav',mimeType='audio/wav',buffer=tone))
        with pg.expect_response(lambda res: res.url.endswith('/actions') and res.request.method=='POST',timeout=45000) as posted:
            card.get_by_role('button',name='Compartir entrega').click()
        assert posted.value.status==200,posted.value.text()
        pg.close()
        return mix('Artista')['versions'][0]['id']

    def hear(card, number, tone):
        card.get_by_role('button',name=f'Escuchar v{number}',exact=True).click()
        audio=card.get_by_label(f'Mezcla de Luna v{number}',exact=True)
        expect(audio).to_be_visible()
        result=audio.evaluate("""async el => {
          const bytes=await (await fetch(el.src)).arrayBuffer();
          const digest=await crypto.subtle.digest('SHA-256',bytes);
          const ctx=new (window.AudioContext || window.webkitAudioContext)();
          const decoded=await ctx.decodeAudioData(bytes); await ctx.close();
          await el.play();
          await new Promise(resolve=>el.addEventListener('timeupdate',resolve,{once:true}));
          el.pause();
          return [Array.from(new Uint8Array(digest),v=>v.toString(16).padStart(2,'0')).join(''),decoded.duration,el.currentTime];
        }""")
        assert result[0]==hashlib.sha256(tone).hexdigest() and result[1]==1 and result[2]>0
        print(f'PASS decoded and played v{number}: {result[0]}',flush=True)

    low,high=waveform(330),waveform(550)
    v3=publish(3,low)
    verify_state('En revisión',artist)
    pg,card=task_page('Artista')
    hear(card,3,low)
    card.get_by_label('Observación sobre v3').fill('Subir la voz')
    with pg.expect_response(lambda r:r.url.endswith('/actions')) as changed:
        card.get_by_role('button',name='Solicitar cambios').click()
    assert changed.value.status==200
    verify_state('Cambios solicitados',engineer)
    pg.close()
    v4=publish(4,high)
    verify_state('En revisión',artist)
    http('Artista','POST',f'/productions/{pid}/actions',409,data=dict(op='feedback',kind='approve',version=v3,request=str(uuid4())))
    pg,card=task_page('Artista')
    hear(card,4,high)
    hear(card,3,low)
    with pg.expect_response(lambda r:r.url.endswith('/actions')) as approved:
        card.get_by_role('button',name='Aprobar v4').click()
    assert approved.value.status==200
    verify_state('Completada',None)
    expect(card.get_by_text('Subir la voz',exact=False)).to_be_visible()
    assert pg.evaluate('document.documentElement.scrollWidth <= innerWidth')
    history=mix('Ingeniera')['versions']
    assert [(v['number'],v['feedback'][0]['kind']) for v in history]==[(4,'approve'),(3,'changes')]

    other_mix=next(t for t in project('Productor')['tasks'] if t['title']=='Mezcla')
    master=next(t for t in project('Productor')['tasks'] if t['title']=='Masterización' and other_mix['songs'][0] in t['songs'])
    booked_master=http('Productor','POST','/bookings',data=dict(cbTitle='Masterización EP',cbStartsAt=master['due']+'T09:00:00-05:00',
        cbEndsAt=master['due']+'T12:00:00-05:00',cbStatus='Confirmed',cbServiceOfferingId=offering,cbEngineerPartyId=engineer)).json()['bookingId']
    action('Productor',op='edit',task=master['id'],title=master['title'],stage=master['stage'],owner=producer,
           duration=master['duration'],due=master['due'],songs=master['songs'],booking=booked_master)
    plan=action('Ingeniera',op='propose',task=other_mix['id'],finish=(today+timedelta(days=63)).isoformat())
    p=project('Productor')
    proposal=next(v for v in p['proposals'] if v['id']==plan)
    assert p['delivery']==delivery and any(c['title']=='Masterización' for c in proposal['changes'])
    http('Productor','POST',f'/productions/{pid}/actions',409,data=dict(op='confirm',proposal=plan))
    assert project('Productor')['delivery']==delivery
    suggested=next(c['suggested'] for c in proposal['changes'] if c['task']==master['id'])
    http('Productor','PUT',f'/bookings/{booked_master}',data=dict(ubStartsAt=suggested+'T09:00:00-05:00',ubEndsAt=suggested+'T12:00:00-05:00'))
    page.set_viewport_size({'width':390,'height':844})
    page.reload()
    page.get_by_role('button',name='Cronograma del proyecto').click()
    expect(page.get_by_text('Propuesta de ajuste',exact=True)).to_be_visible()
    with page.expect_response(lambda res:res.url.endswith('/actions')) as confirmed:
        page.get_by_role('button',name='Confirmar ajuste').click()
    assert confirmed.value.status==200
    updated=project('Productor')['delivery']
    assert updated>delivery
    second=http('Productor','POST','/productions',data=dict(title='Segundo proyecto',format='Sencillo',delivery=updated,
        songs=[dict(title='Otro tema')],template=[dict(name='Entrega coincidente',days=0)])).json()['id']
    assert len(http('Artista','GET','/productions').json())==1
    http('Artista','GET',f'/productions/{second}/versions/{v4}/audio',404)
    assert http('Otra cuenta','GET','/productions').json()==[]
    http('Otra cuenta','GET',f'/productions/{pid}/versions/{v4}/audio',404)
    page.reload()
    page.get_by_role('button',name='Calendario entre proyectos').click()
    expect(page.get_by_text('Segundo proyecto · Entrega coincidente')).to_be_visible()
    expect(page.get_by_text('Sin superposiciones',exact=False)).to_be_visible()
    assert page.evaluate('document.documentElement.scrollWidth <= innerWidth')
    print('PASS real TDF booking, six-stage EP, shared drums, four account scopes, both action owners, version history, stale decision, schedule, mobile viewport, cross-project workload', flush=True)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--backend',required=True,type=Path)
    args=parser.parse_args()
    binary=args.backend.resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix='tdf-production-browser-') as tmp, ExitStack() as cleanup:
        name='tdf-workspace-browser-'+uuid4().hex
        password=secrets.token_hex(24)
        subprocess.run(['docker','run','--rm','-d','--name',name,'-p','127.0.0.1::5432','-e','POSTGRES_PASSWORD='+password,
                        '-e','POSTGRES_DB=production_test','pgvector/pgvector:pg17'],check=True,stdout=subprocess.DEVNULL)
        cleanup.callback(subprocess.run,['docker','stop',name],stdout=subprocess.DEVNULL,check=False)
        for _ in range(160):
            if subprocess.run(['docker','exec',name,'pg_isready','-h','127.0.0.1','-U','postgres'],capture_output=True).returncode==0: break
            time.sleep(.25)

        def sql(text):
            return subprocess.run(['docker','exec','-i',name,'psql','-XqAt','-U','postgres','-d','production_test','-v','ON_ERROR_STOP=1'],
                                  input=text,text=True,capture_output=True,check=True).stdout.strip()
        for f in ['production-schema-20260814.sql','catalog-production-source-fixture.sql']:
            sql((ROOT/'scripts/__tests__/fixtures'/f).read_text())
        sql(subprocess.check_output(['node','scripts/render-production-migration-batch.mjs'],cwd=ROOT,text=True))
        actors={}
        for label,user,role in [('Productor','producer','studio-manager'),('Ingeniera','engineer','engineer'),('Artista','artist','artist'),('Otra cuenta','outside','customer')]:
            party=int(sql(f"INSERT INTO party(display_name,is_org,created_at) VALUES('{label}',false,now()) RETURNING id"))
            token=secrets.token_hex(32)
            sql(f"""INSERT INTO user_credential(party_id,username,password_hash,active) VALUES({party},'workspace-{user}','fixture',true);
                 INSERT INTO party_security_role(party_id,role_id) SELECT {party},id FROM security_role WHERE code='{role}';
                 INSERT INTO api_token(token,party_id,active,label) VALUES('{token}',{party},true,'fixture');
                 INSERT INTO content_storage_account(party_id,legacy_usage_verified) VALUES({party},true) ON CONFLICT DO NOTHING;""")
            actors[label]=(party,token)
        offering=sql("SELECT id FROM service_offering WHERE active AND code='band-recording'")
        port=subprocess.check_output(['docker','port',name,'5432/tcp'],text=True).strip().rsplit(':',1)[1]
        api_port,web_port=free_port(),free_port()
        api=f'http://127.0.0.1:{api_port}'
        web=f'http://127.0.0.1:{web_port}'
        def start(command, env, cwd, check, log):
            stream=cleanup.enter_context(open(Path(tmp)/log,'w+'))
            proc=subprocess.Popen(command,cwd=cwd,env=env,stdout=stream,stderr=subprocess.STDOUT)
            def stop():
                proc.terminate()
                try: proc.wait(timeout=10)
                except subprocess.TimeoutExpired: proc.kill(); proc.wait()
            cleanup.callback(stop)
            for _ in range(240):
                if proc.poll() is not None:
                    stream.seek(0)
                    raise RuntimeError(log+' failed: '+stream.read()[-1500:].replace(password,'[redacted]'))
                try:
                    with urlopen(check,timeout=1) as res:
                        if res.status==200: return
                except OSError: time.sleep(.25)
            raise RuntimeError('Timed out starting '+log)
        base_env={k:os.environ[k] for k in ['PATH','LANG','HOME','LD_LIBRARY_PATH'] if k in os.environ}
        start([str(binary),'+RTS','-N2','-RTS'],dict(base_env,DATABASE_URL=f'postgresql://postgres:{password}@127.0.0.1:{port}/production_test',
            APP_PORT=str(api_port),APP_ENV='test',DEFAULT_LOCALE='es',RUN_MIGRATIONS='false',RESET_DB='false',SEED_DB='false',HQ_ASSETS_DIR=str(ROOT/'tdf-hq/assets'),
            ALLOWED_ORIGINS=web,ALLOW_ALL_ORIGINS='false',EVENT_DISCOVERY_ENABLED='false'),tmp,api+'/health','api.log')
        start([str(ROOT/'node_modules/.bin/vite'),'--host','127.0.0.1','--port',str(web_port),'--strictPort'],
              dict(base_env,VITE_API_BASE=api),ROOT/'tdf-hq-ui',web,'vite.log')
        with sync_playwright() as pw:
            browser=pw.chromium.launch(executable_path=shutil.which('chromium'),args=['--autoplay-policy=no-user-gesture-required','--no-sandbox'])
            try: scenario(browser,api,web,actors,offering)
            finally: browser.close()


if __name__=='__main__':
    main()
