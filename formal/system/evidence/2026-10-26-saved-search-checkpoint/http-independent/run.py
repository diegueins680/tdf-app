"""Replay the untouched B705 financial/checkpoint SQL+HTTP on owned loopback resources."""
from pathlib import Path
import datetime, hashlib, json, os, re, shutil, signal, socket, subprocess, sys, tempfile, time

if sys.flags.optimize: raise RuntimeError('Unoptimized isolation assertions required')
root = Path('/private/tmp/tdf-ledger-checkpoint-http-root-20261026')
source = 'b7053f9426d624f2fdaf8a9731389e8ef52e3bdf'
compiled = 'fe179de45d9b0e085bb7cd2f00a9704991fa1652'
main = 'a1238fbf6d98062904a4bd80d2f9df35ebc40706'
binary_source = Path('/private/tmp/tdf-alert-parity-repro-20261025/bin-post551/tdf-hq-exe')
binary_hash = 'cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a'
packet = Path(__file__).resolve().parent
def git(*args): return subprocess.check_output(['git', *args], cwd=root, text=True).strip()
assert git('rev-parse', 'HEAD') == source and not git('status', '--porcelain')
inputs = ['tdf-hq/src', 'tdf-hq/app', 'tdf-hq/tdf-hq.cabal', 'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock']
subprocess.run(['git', 'diff', '--exit-code', compiled, source, '--', *inputs], cwd=root, check=True)
baseline = json.loads(git('show', main + ':scripts/production-migrations.json'))['migrations']
manifest = json.loads((root / 'scripts/production-migrations.json').read_text())['migrations']
assert len(baseline) == 221 and len(manifest) == 222 and manifest[:221] == baseline
assert manifest[-1]['id'] == '2026-10-25_ledger_posted_header'
for entry in manifest: subprocess.run(['git', 'merge-base', '--is-ancestor', entry['introducedBy'], source], cwd=root, check=True)
assert hashlib.sha256(binary_source.read_bytes()).hexdigest() == binary_hash
assert subprocess.check_output(['node', '--version'], text=True).strip() == 'v22.14.0'
assert sys.platform == 'darwin' and Path('/usr/bin/sandbox-exec').exists()
assert subprocess.run(['lsof', '-tiTCP:55592', '-sTCP:LISTEN'], capture_output=True).returncode != 0
out = Path(tempfile.mkdtemp(prefix='tdf-ledger-checkpoint-http-root-20261026-', dir='/private/tmp')); os.chmod(out, 0o700)
print(str(out), flush=True)
env = {k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ']}
env['PATH'] = '/usr/local/opt/postgresql@16/bin:' + env['PATH']
env.update(PGHOST='127.0.0.1', PGPORT='55592', PGUSER=os.getlogin(), PGPASSFILE=str(out/'no-passfile'), TMPDIR=str(out), CI='false')
database = 'tdf_ledger_checkpoint_http_root_20261026_test'
url = 'postgresql://' + env['PGUSER'] + '@127.0.0.1:55592/' + database
report = {'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(), 'source':source, 'tree':git('rev-parse',source+'^{tree}'), 'protectedMain':main, 'compiledSource':compiled, 'compiledInputsByteExact':inputs, 'actualExecutableSha256':binary_hash, 'migrations':222, 'unchangedMainPrefix':221, 'status':'running', 'steps':[], 'cleanup':{}, 'productionOperations':False, 'newCompilation':False, 'scope':'Untouched canonical source SQL/checkpoint and real HTTP. Native executable retains fe179 identity. Only explicit test-bind adapter and owned network sandbox wrap the executable; wrapper hash is separate from executable hash.'}
report['harnessSha256'] = {name:hashlib.sha256((packet/name).read_bytes()).hexdigest() for name in ['run.py','loopback-bind.c']}
started = created = False
profile = '(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote ip "localhost:*"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
def save(): (out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(name, command, extra=None, timeout=300):
    with (out/(name+'.log')).open('wb') as log:
        if name == 'canonical-sql-http':
            child = subprocess.Popen(command, cwd=root, env={**env,**(extra or {})}, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            try:
                deadline = time.monotonic() + timeout
                while child.poll() is None:
                    owner_path = out / 'api-owner.json'
                    if owner_path.exists() and not report.get('apiLoopbackVerified'):
                        owner_info = json.loads(owner_path.read_text())
                        listening = subprocess.run(['lsof','-nP','-iTCP:'+str(owner_info['port']),'-sTCP:LISTEN'],capture_output=True,text=True)
                        if listening.returncode == 0:
                            assert '127.0.0.1:'+str(owner_info['port']) in listening.stdout and '*:'+str(owner_info['port']) not in listening.stdout
                            try:
                                with socket.create_connection((lan,owner_info['port']),timeout=1): raise AssertionError('LAN API listener exposed')
                            except OSError: pass
                            report['apiLoopbackVerified'] = True
                    if time.monotonic() > deadline: raise TimeoutError('Canonical runner deadline')
                    time.sleep(.1)
            finally:
                try: os.killpg(child.pid, signal.SIGTERM)
                except ProcessLookupError: pass
                if child.poll() is None: child.wait(timeout=10)
            code = child.returncode
        else:
            code = subprocess.run(command,cwd=root,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,timeout=timeout).returncode
    report['steps'].append({'name':name,'exitCode':code});save()
    if code: raise RuntimeError(name+' failed; inspect private log')
    print(name+' PASS',flush=True)
def sql(query): return subprocess.check_output(['psql',url,'-XqAt','-v','ON_ERROR_STOP=1','-c',query],env=env,text=True).strip()
try:
    (out/'socket').mkdir();(out/'assets').mkdir()
    binary = out/'tdf-hq-exe';shutil.copyfile(binary_source,binary);binary.chmod(0o700)
    assert hashlib.sha256(binary.read_bytes()).hexdigest() == binary_hash
    dylib=out/'loopback-bind.dylib'
    run('compile-loopback-adapter',['clang','-dynamiclib','-Wall','-Wextra','-Werror',str(packet/'loopback-bind.c'),'-o',str(dylib)])
    lan=None
    for interface in ['en1','en0']:
        result=subprocess.run(['ipconfig','getifaddr',interface],capture_output=True,text=True)
        if result.returncode==0:lan=result.stdout.strip();break
    assert lan
    # SBPL localhost includes this host's LAN interfaces; it is not a literal
    # loopback-address filter. Test nonlocal egress against an already-authorized
    # GitHub endpoint using TCP handshakes only, with no application data.
    remote = None
    for addr in socket.getaddrinfo('api.github.com',443,family=socket.AF_INET,type=socket.SOCK_STREAM):
        candidate = addr[4][0]
        try:
            with socket.create_connection((candidate,443),timeout=3): pass
        except OSError: continue
        remote = candidate; break
    assert remote, 'Positive external connectivity control required'
    check='import socket,sys\ntry:socket.create_connection((sys.argv[1],443),timeout=2)\nexcept PermissionError:sys.exit(0)\nexcept OSError:sys.exit(3)\nelse:sys.exit(2)'
    run('deny-nonlocal-egress',['sandbox-exec','-p',profile,sys.executable,'-c',check,remote])
    report['networkIsolation']={'policy':'Local-machine destinations only; services separately bound to literal127.0.0.1','externalTcpControl':'api.github.com:443, handshake only, no application data','externalAllowedOutsideDeniedInsideSandbox':True,'externalAddressRetained':False}
    wrapper=out/'native-wrapper'
    wrapper.write_text('#!'+sys.executable+'\nimport os,sys,hashlib\nfrom pathlib import Path\np=Path('+repr(str(binary))+')\nif hashlib.sha256(p.read_bytes()).hexdigest()!='+repr(binary_hash)+': raise RuntimeError("Executable changed")\nport=os.environ["APP_PORT"]\nif not port.isdecimal() or not 1024<int(port)<65536: raise RuntimeError("Invalid owned port")\nPath('+repr(str(out/'api-owner.json'))+').write_text(__import__("json").dumps({"pid":os.getpid(),"port":int(port)}))\nos.environ["DYLD_INSERT_LIBRARIES"]='+repr(str(dylib))+'\nos.environ["TDF_PILOT_API_PORT"]=port\nos.execv(str(p),[str(p)])\n')
    wrapper.chmod(0o700);report['testWrapperSha256']=hashlib.sha256(wrapper.read_bytes()).hexdigest();save()
    run('init',['initdb','-D',str(out/'cluster'),'--username='+env['PGUSER'],'--encoding=UTF8','--locale=en_US.UTF-8','--auth=trust'])
    run('start',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o','-p 55592 -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
    run('create',['createdb','--template=template0',database]);created=True
    run('canonical-sql-http',['sandbox-exec','-p',profile,'node','scripts/test-directory-saved-search-parity.mjs'],{'TDF_DIRECTORY_ALERT_PARITY_DATABASE_URL':url,'TDF_DIRECTORY_ALERT_PARITY_SERVER_BIN':str(wrapper),'TDF_DIRECTORY_ALERT_PARITY_COMPILED_COMMIT':compiled,'HQ_ASSETS_DIR':str(out/'assets')},timeout=300)
    text=(out/'canonical-sql-http.log').read_text()
    match=re.search(r'PASS: (\d+) real HTTP requests',text);assert match and int(match[1])==107
    assert report.get('apiLoopbackVerified') is True
    provenance=[json.loads(line) for line in text.splitlines() if line.startswith('{"qualificationSourceCommit"')];assert len(provenance)==1
    assert provenance[0]['qualificationSourceCommit']==source and provenance[0]['compiledCommit']==compiled
    assert provenance[0]['binarySha256']==report['testWrapperSha256']
    report['driverMeasuredProvenance']=provenance[0];report['driverBinaryHashIsTestWrapper']=True
    report['realHttpRequests']=107
    report['schemaWitness']=sql("SELECT (SELECT count(*) FROM tdf_schema_migration)||'|'||(SELECT jsonb_array_length(prefix_ids) FROM directory_alert_qualification.before_target)||'|'||(SELECT tgenabled FROM pg_trigger WHERE tgrelid='commerce_ledger_transaction'::regclass AND tgname='trg_commerce_ledger_header_immutable');")
    assert report['schemaWitness']=='222|220|O'
    assert git('rev-parse','HEAD')==source and not git('status','--porcelain')
    report['status']='PASS_SCOPED_UNMODIFIED_222_SQL_AND107_HTTP'
except BaseException as error:
    report['status']='FAILED';report['failureType']=type(error).__name__;save();raise
finally:
    owner=out/'api-owner.json'
    if owner.exists():
        info=json.loads(owner.read_text());pid=info['pid']
        command=subprocess.run(['ps','-p',str(pid),'-o','command='],capture_output=True,text=True).stdout.strip()
        if str(out/'tdf-hq-exe') in command:
            os.killpg(pid,signal.SIGTERM)
        report['cleanup']['apiListenerClosed']=subprocess.run(['lsof','-tiTCP:'+str(info['port']),'-sTCP:LISTEN'],capture_output=True).returncode!=0
    else:report['cleanup']['apiNeverStarted']=True
    if created:
        try:run('drop',['dropdb',database]);report['cleanup']['databaseRemoved']=True
        finally:
            if started:run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
    elif started:
        run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
    report['cleanup']['postgresListenerClosed']=subprocess.run(['lsof','-tiTCP:55592','-sTCP:LISTEN'],capture_output=True).returncode!=0
    report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
