#!/usr/bin/env python3
"""Observe one interrupted HTTP mutation on the exact legacy production image.

Runs only via the owned empty-host fixture and explicit machine-id acknowledgement.
The production stop/capture validators remain unchanged: exit255 must be rejected.
The committed-state observation is not a guarantee of all interrupted outcomes.
"""
import importlib.util,json,os,time,subprocess,selectors
from pathlib import Path
from unittest.mock import patch

root=Path(__file__).resolve().parent.parent
s=importlib.util.spec_from_file_location('legacy_owned_fixture',root/'scripts/test-production-writer-fence-linux.py')
f=importlib.util.module_from_spec(s);s.loader.exec_module(f)
assert os.environ.get('TDF_SYNTHETIC_WRITER_FENCE_HOST')==Path('/etc/machine-id').read_text().strip()
expected='diegueins680/tdf-hq@sha256:38e6264b82db2d81a5b51c3a78740b6a305538b4cdae8d53ced067ccbb1e8fe0'
assert os.environ['TDF_CANARY_TEST_IMAGE']==expected
execute=f.w.execute;perform=f.w.WriterFence._perform
observed={};rejected=False


HTTP = r"""
import http.client,json,sys
c=http.client.HTTPConnection('127.0.0.1',8080,timeout=25)
try:
 c.request('POST','/rooms',json.dumps({'rcName':sys.argv[1]}),{'Content-Type':'application/json','Authorization':'Bearer synthetic-legacy-stop'})
 r=c.getresponse();r.read(65536);print(json.dumps({'code':r.status,'transportFailure':False}))
except (OSError,http.client.HTTPException) as e:print(json.dumps({'code':None,'transportFailure':True,'error':type(e).__name__}))
finally:c.close()
"""

def active_request(row):
    ids=f.run(f.DOCKER+['ps','--all','--quiet','--no-trunc']).split()
    rows=[f.inspect(cid) for cid in ids]
    db=next(v['Id'] for v in rows if v['Config']['Labels'].get('com.docker.compose.service')=='db')
    assert f.inspect(db)['Config']['Labels'].get(f.LABEL)==row['Config']['Labels'][f.LABEL]
    psql=f.DOCKER+['exec','-i',db,'psql','-X','-qAt','-v','ON_ERROR_STOP=1','-U','postgres','-d','tdf_hq']
    def sql(value):
        p=subprocess.run(psql,input=value,text=True,capture_output=True,timeout=10)
        assert p.returncode==0,p.stderr
        return p.stdout.strip()
    sql("WITH actor AS (INSERT INTO party(display_name,is_org,created_at) VALUES ('Synthetic stop actor',false,now()) RETURNING id) INSERT INTO api_token(token,party_id,label,active) SELECT 'synthetic-legacy-stop',id,'Synthetic',true FROM actor; INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT a.party_id,r.id,'bootstrap',true FROM api_token a CROSS JOIN security_role r WHERE a.token='synthetic-legacy-stop' AND r.code='admin';")
    fd=os.open('/proc/'+str(row['State']['Pid'])+'/ns/net',os.O_RDONLY)
    http=['nsenter','--net=/proc/self/fd/'+str(fd),'python3','-c',HTTP]
    barrier=request=None
    try:
        positive=subprocess.run(http+['Synthetic committed room'],pass_fds=(fd,),text=True,capture_output=True,timeout=30)
        assert positive.returncode==0 and json.loads(positive.stdout)['code']==201,positive.stdout
        sql("CREATE FUNCTION synthetic_stop_barrier() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_advisory_xact_lock(6080611); RETURN NEW; END $$; CREATE TRIGGER synthetic_stop_barrier BEFORE INSERT ON room FOR EACH ROW EXECUTE FUNCTION synthetic_stop_barrier();")
        barrier=subprocess.Popen(psql,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
        barrier.stdin.write("BEGIN; SET LOCAL idle_in_transaction_session_timeout='40s'; SELECT 'LOCKED' FROM (SELECT pg_advisory_xact_lock(6080611)) held;\n");barrier.stdin.flush()
        deadline=time.monotonic()+8
        while time.monotonic()<deadline:
            with selectors.DefaultSelector() as sel:
                sel.register(barrier.stdout,selectors.EVENT_READ)
                if not sel.select(1):continue
            if barrier.stdout.readline().strip()=='LOCKED':break
        else:raise AssertionError('barrier readiness')
        request=subprocess.Popen(http+['Synthetic ambiguous room'],pass_fds=(fd,),text=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        deadline=time.monotonic()+8
        while time.monotonic()<deadline:
            if sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND state='active' AND wait_event_type='Lock' AND query ILIKE '%INSERT INTO%room%';")=='1':break
            assert request.poll() is None,request.communicate()
            time.sleep(.05)
        else:raise AssertionError('HTTP insert not blocked')
        return fd,barrier,request,sql
    except BaseException:
        if request is not None and request.poll() is None:request.terminate();request.wait(timeout=5)
        if barrier is not None and barrier.poll() is None:barrier.communicate('ROLLBACK;\n',timeout=5)
        os.close(fd);raise


def signal(command):
    if command[:len(f.DOCKER)+1]==f.DOCKER+['stop']:
        row=f.inspect(command[-1])
        if row['Config']['Labels'].get('com.docker.compose.service')=='api':
            assert not observed and row['Config']['Image']==expected and row['State']['Running']
            assert row['Config']['Labels'].get(f.LABEL)
            row['Mounts']=sorted(row['Mounts'],key=lambda m:m['Destination'])
            before={key:row[key] for key in ('Id','Image','Config','HostConfig','Mounts')}
            fd,barrier,request,sql=active_request(row)
            try:
                assert request.poll() is None, "HTTP request finished before the stop request"
                start=time.monotonic()
                reply=execute(command[:-1]+['--signal=SIGINT',command[-1]])
                stdout,stderr=request.communicate(timeout=10)
                assert request.returncode==0,stderr
                result=json.loads(stdout)
                assert result['transportFailure'] is True and result['code'] is None,result
                assert result.get('error') in ('RemoteDisconnected','ConnectionResetError'),result
                barrier.communicate('ROLLBACK;\n',timeout=5)
                deadline=time.monotonic()+5
                while time.monotonic()<deadline:
                    remaining=sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND state='active' AND query ILIKE '%INSERT INTO%room%';")
                    if remaining=='0':break
                    time.sleep(.05)
                assert remaining=='0'
                retained=sql("SELECT count(*) FROM room WHERE name='Synthetic committed room';")
                ambiguous=sql("SELECT count(*) FROM room WHERE name='Synthetic ambiguous room';")
                assert retained=='1' and ambiguous in ('0','1')
                observed.update(activeHttpInsertObserved=True,clientOutcome=result,committedRoomRetained=True,
                                interruptedInsertCommitted=ambiguous=='1',allRequestOutcomesKnown=False)
            finally:
                if request.poll() is None:request.terminate();request.wait(timeout=5)
                if barrier.poll() is None:barrier.communicate('ROLLBACK;\n',timeout=5)
                os.close(fd)
            after=f.inspect(command[-1]);state=after['State']
            after['Mounts']=sorted(after['Mounts'],key=lambda m:m['Destination'])
            assert {key:after[key] for key in before}==before and reply.strip()==after['Id']
            observed.update(exitCode=state['ExitCode'],running=state['Running'],oomKilled=state['OOMKilled'],
                            restarting=state['Restarting'],elapsedSeconds=time.monotonic()-start,
                            exactContainerUnchanged=True,stopReplyAcknowledged=True)
            return reply
    return execute(command)

def record(self,phase,effect):
    try:return perform(self,phase,effect)
    except ValueError:
        if phase=='stop-writers' and observed:
            observed['journalPendingStage']=self.journal.status()['pendingStage']
        raise

try:
    with patch.object(f.w,'execute',side_effect=signal),patch.object(f.w.WriterFence,'_perform',record):f.main()
except ValueError:rejected=True
print(json.dumps({'captureRejected':rejected,'observedStop':observed}),flush=True)
assert rejected and observed['exitCode']==255 and not observed['running'] and not observed['oomKilled'] and not observed['restarting']
assert observed['journalPendingStage']=='stop-writers'
assert not f.run(f.DOCKER+['ps','--all','--quiet']) and not os.path.lexists(f.DIRECTORY)
print(json.dumps({'status':'legacy-active-SIGINT-observed-and-capture-rejected','image':expected,
                 'observed':observed,'ownedFixtureCleaned':True,'productionChanged':False,
                 'scope':'One real blocked HTTP insert on exact legacy PID1; client outcome lost and actual committed state recorded. No worker drain or capture compatibility qualification.'}))
