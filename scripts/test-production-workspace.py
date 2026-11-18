#!/usr/bin/env python3
"""Exercise the production SQL API on a new network-isolated PostgreSQL 17 container."""
import concurrent.futures
import json
from pathlib import Path
import subprocess
import time
from uuid import uuid4

ROOT = Path(__file__).resolve().parents[1]
container = 'tdf-workspace-test-' + uuid4().hex
migration = ROOT / 'tdf-hq/sql/2026-11-18_production_workspace.sql'


def sql(statement):
    result = subprocess.run(['docker','exec','-i',container,'psql','-XAtq','-v','ON_ERROR_STOP=1','-U','postgres'],
                            input=statement, text=True, capture_output=True)
    if result.returncode:
        raise RuntimeError(result.stderr)
    return result.stdout.strip()


def literal(value):
    return "'" + str(value).replace("'", "''") + "'"


def call(name, *params):
    return json.loads(sql('select production_' + name + '(' + ','.join(params) + ');'))


def snapshot(actor):
    return call('workspace', str(actor))


def create(title, songs):
    return call('create', '1', literal(json.dumps(dict(title=title, songs=[{'title':s} for s in songs], format='EP', delivery='2027-05-31',
        template=[dict(name=n,days=d) for n,d in [('Mezcla',2),('Masterización',2),('Entrega',0)]]))))


def exercise():
    sql("""
      CREATE TABLE party(id bigint primary key, display_name text);
      INSERT INTO party VALUES (1,'Productor'),(2,'Ingeniera'),(3,'Artista'),(4,'Otra cuenta'),(5,'Cliente');
      CREATE TABLE user_credential(username text,active boolean,party_id bigint);
      INSERT INTO user_credential VALUES('audio',true,2),('artista',true,3),('cliente',true,5);
      CREATE TABLE label_track(id uuid PRIMARY KEY,title text,owner_party_id bigint);
      CREATE TABLE booking(id bigint PRIMARY KEY,starts_at timestamptz,ends_at timestamptz,status text,party_id bigint,engineer_party_id bigint);
      INSERT INTO booking VALUES(7,'2027-05-23T09:00:00-05:00','2027-05-23T13:00:00-05:00','Confirmed',1,2);
      CREATE TABLE booking_resource(booking_id bigint,resource_id bigint);
      CREATE TABLE identity_party_archive(party_id bigint);
    """)
    guard = (ROOT / 'tdf-hq/sql/2026-09-17_identity_reconciliation.sql').read_text()
    sql(guard[guard.index('CREATE OR REPLACE FUNCTION identity_reject_archived_reference()'):guard.index('CREATE OR REPLACE FUNCTION identity_reject_archived_party_write()')])
    sql((ROOT / 'tdf-hq/sql/2026-11-04_content_storage.sql').read_text())
    sql(migration.read_text())
    sql(migration.read_text())
    pid = create('Tres canciones',['Luna','Mar','Sal'])['id']

    def project(actor=1):
        return next(p for p in snapshot(actor) if p['id']==pid)

    def command(actor=1, **body):
        body.setdefault('revision', project()['revision'])
        return call('command',str(actor),'false',literal(pid),literal(json.dumps(body)))

    def ok(actor=1, **body):
        receipt = command(actor, **body)
        assert 'id' in receipt, receipt
        return receipt['id']

    def progress(task, actor):
        p = project(actor)
        t = next(t for t in p['tasks'] if t['id']==task)
        return t['status'], t['approver'] if t['status']=='En revisión' else None if t['status']=='Completada' else t['owner']

    for username,role in [('audio','engineer'),('artista','client'),('cliente','client')]:
        ok(op='invite', username=username, role=role)
    songs = project()['songs']
    drums = ok(op='task', title='Batería', stage='Grabación', songs=[s['id'] for s in songs], duration=1,
               due='2027-05-23', owner=2, booking=7)
    assert len(next(t for t in project()['tasks'] if t['id']==drums)['songs'])==3
    ok(2,op='progress',task=drums,status='Completada')
    assert progress(drums,2)==('Completada',None)
    mix = next(t for t in project()['tasks'] if t['title']=='Mezcla' and songs[0]['id'] in t['songs'])
    ok(op='edit',**{k:mix[k] for k in ['title','songs','stage','duration','due']},task=mix['id'],owner=2,approver=3)
    assert project(3)['tasks']==[] and progress(mix['id'],2)==('Pendiente',2)
    def upload(number):
        file = str(uuid4())
        sql(f"""INSERT INTO content_storage_account(party_id) VALUES(2) ON CONFLICT DO NOTHING;
          INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,
          media_type,sha256,status,lease_id,lease_until,completed_at) VALUES('{file}',2,repeat('{number}',64),repeat('a',64),100,
          'tdf','private','mezcla.wav','audio/wav',repeat('{number}',64),'completed',gen_random_uuid(),now(),now());""")
        return ok(2,op='publish',task=mix['id'],file=file,number=number),file
    v3,file3 = upload(3)
    assert progress(mix['id'],3)==progress(mix['id'],2)==('En revisión',3)
    assert not project(5)['tasks'] and not snapshot(4)
    req = str(uuid4())
    change = dict(op='feedback',kind='changes',version=v3,note='Subir la voz',request=req)
    receipt = ok(3,**change)
    assert progress(mix['id'],3)==progress(mix['id'],2)==('Cambios solicitados',2)
    v4,file4 = upload(4)
    assert ok(3,**change)==receipt
    stale = command(3,op='feedback',kind='approve',version=v3,request=str(uuid4()))
    assert stale['error']==409
    assert command(2,op='feedback',kind='approve',version=v4,request=str(uuid4()))['error']==403
    assert command(4,op='feedback',kind='approve',version=v4,request=str(uuid4()))['error']==404
    assert file3!=file4 and sql(f"select file_id from production_version where id='{v3}'")==file3
    assert sql(f"select file_id from production_version where id='{v4}'")==file4

    # The second connection must enter while the first still owns the project lock.
    # pg_stat_activity observes the first inside pg_sleep after its mutation.
    def race(first, second):
        def statement(body):
            return f"select production_command({body.pop('actor')},false,'{pid}',{literal(json.dumps(body))})"
        marker = 'prodtest' + uuid4().hex
        a,b = statement(first),statement(second)
        with concurrent.futures.ThreadPoolExecutor() as pool:
            leading = pool.submit(sql,f"set application_name='{marker}'; begin; {a}; select pg_sleep(2); commit;")
            for _ in range(60):
                if sql(f"select exists(select from pg_stat_activity where application_name='{marker}' and wait_event='PgSleep')")=='t': break
                time.sleep(.025)
            else: raise AssertionError('transaction did not reach overlap barrier')
            following = pool.submit(sql,b)
            results = json.loads(leading.result()),json.loads(following.result())
        return results

    req4 = str(uuid4())
    a,b = race(dict(actor=3,op='feedback',version=v4,kind='approve',request=req4),
               dict(actor=3,op='feedback',version=v4,kind='changes',note='Otra decisión',request=str(uuid4())))
    assert 'id' in a and b['error']==409
    assert ok(3,op='feedback',version=v4,kind='approve',request=req4)==a['id']
    assert progress(mix['id'],3)==progress(mix['id'],2)==('Completada',None)
    versions=project(3)['tasks'][0]['versions']
    assert [(v['number'],v['feedback'][0]['kind']) for v in versions]==[(4,'approve'),(3,'changes')]
    assert all(len(v['feedback'])==1 for v in versions)
    assert sql(f"select production_visible_version(5,'{v4}'),production_visible_version(3,'{v3}'),production_visible_version(4,'{v4}')")=='f|t|f'
    try: sql(f"update content_storage_upload set status='cancelled',completed_at=null where id='{file3}'")
    except RuntimeError as error: assert 'production revision' in str(error)
    else: raise AssertionError('historical file deleted')

    pending=[t for t in project()['tasks'] if t['title']=='Mezcla' and t['status']=='Pendiente']
    x,y=[t['id'] for t in pending]
    rev=project()['revision']
    a,b=race(dict(actor=1,op='dependency',task=y,needs=x,revision=rev),
             dict(actor=1,op='dependency',task=x,needs=y,revision=rev))
    assert 'id' in a and b['error']==409
    assert command(op='dependency',task=x,needs=y)['error']==409
    before=project()['tasks']
    proposal=ok(2,op='propose',task=x,finish='2027-06-02')
    plan=project()['proposals'][0]
    assert any(c['title']=='Masterización' and c['cause']=='Mezcla' for c in plan['changes'])
    assert before==project()['tasks'] and project()['delivery']=='2027-05-31'
    assert command(2,op='confirm',proposal=proposal)['error']==403
    rev=project()['revision']
    a,b=race(dict(actor=1,op='block',task=x,note='Falta una toma',revision=rev),
             dict(actor=1,op='confirm',proposal=proposal))
    assert 'id' in a and b['error']==409 and project()['delivery']=='2027-05-31'
    fresh=ok(op='propose',task=x,finish='2027-06-02')
    ok(op='confirm',proposal=fresh)
    assert project()['delivery']>'2027-06-02'
    # Booking adjustment must occur at the booking authority first.
    reserved=ok(op='task',title='Sesión B',stage='Grabación',songs=[],due='2027-05-28',owner=1,duration=1)
    sql("INSERT INTO booking VALUES(8,'2027-05-28T09:00:00-05:00','2027-05-28T13:00:00-05:00','Confirmed',1,2)")
    ok(op='edit',task=reserved,title='Sesión B',stage='Grabación',songs=[],due='2027-05-28',owner=1,duration=1,booking=8)
    booking_plan=ok(op='propose',task=reserved,finish='2027-06-15')
    assert command(op='confirm',proposal=booking_plan)['error']==409
    assert next(t for t in project()['tasks'] if t['id']==reserved)['due']=='2027-05-28'
    sql("UPDATE booking SET starts_at=starts_at+interval '18 days', ends_at=ends_at+interval '18 days' WHERE id=8")
    ok(op='confirm',proposal=booking_plan)
    assert next(t for t in project()['tasks'] if t['id']==reserved)['due']=='2027-06-15'
    back=ok(op='backplan',finish='2027-07-15')
    proposed=next(a for a in project()['proposals'] if a['id']==back)
    assert proposed['backward'] and proposed['delivery']=='2027-07-15' and project()['delivery']=='2027-06-15'
    dates={c['task']:c['suggested'] for c in proposed['changes']}
    assert dates[x] < dates[y]
    second=create('Otro proyecto',['Una canción'])['id']
    assert second in [p['id'] for p in snapshot(1)] and second not in [p['id'] for p in snapshot(3)]
    assert len(snapshot(1))==2 and len(snapshot(2))==1
    print('PASS three-song EP, one booking, two-project privacy, review history, exact file identity, three overlapping PostgreSQL writer pairs, immutable files and schedule confirmation')


if __name__=='__main__':
    subprocess.run(['docker','run','--rm','-d','--name',container,'--network','none','-e','POSTGRES_HOST_AUTH_METHOD=trust','postgres:17-alpine'],check=True,stdout=subprocess.DEVNULL)
    try:
        for _ in range(120):
            if subprocess.run(['docker','exec',container,'pg_isready','-U','postgres'],capture_output=True).returncode==0: break
            time.sleep(.25)
        exercise()
    finally:
        subprocess.run(['docker','stop',container],check=True,stdout=subprocess.DEVNULL)
