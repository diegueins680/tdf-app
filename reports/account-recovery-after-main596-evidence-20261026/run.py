"""Owned disposable ST-001 qualification; no production/provider resources."""
import argparse, datetime, hashlib, json, os, re, signal, socket, subprocess, sys, tempfile, time, urllib.request
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source', required=True)
parser.add_argument('--expected-source', required=True)
parser.add_argument('--reuse-build-receipt', help='Reuse only a successful same-source web build from a cleaned owned run')
args = parser.parse_args()
if sys.flags.optimize: parser.error('Unoptimized Python required for isolation assertions')
root = Path(args.source).resolve()
packet = Path(__file__).resolve().parent
binary = Path('/private/tmp/tdf-account-recovery-main624-native-20261026/bin/tdf-hq-exe')
expected_binary = '8dbe98ed4719573d83ae91e37b5df4c84a6dc1e48485c6ae4ac1b0f6d51cd1a5'
baseline = '2e01bbe1263e8761fa2692bd951f828c01142449'
def git(*args): return subprocess.check_output(['git', *args], cwd=root, text=True).strip()
assert git('rev-parse', 'HEAD') == args.expected_source
assert not git('status', '--porcelain', '--untracked-files=no'), 'Tracked source must be clean'
subprocess.run(['git', 'diff', '--exit-code', baseline, args.expected_source, '--', 'tdf-hq/src', 'tdf-hq/app', 'tdf-hq/tdf-hq.cabal', 'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock'], cwd=root, check=True)
manifest = json.loads((root / 'scripts/production-migrations.json').read_text())['migrations']
protected_prefix = json.loads(git('show', '2e01bbe1263e8761fa2692bd951f828c01142449:scripts/production-migrations.json'))['migrations']
assert len(protected_prefix) == 222 and len(manifest) == 223 and manifest[:222] == protected_prefix
assert manifest[-2]['id'] == '2026-10-25_ledger_posted_header' and manifest[-1]['id'] == '2026-10-16_provider_domain_payment_binding'
assert manifest == json.loads(git('show', 'f9e77b73f6ff12ea5f5d892e6386751678fa65a0:scripts/production-migrations.json'))['migrations']
assert hashlib.sha256(binary.read_bytes()).hexdigest() == expected_binary
assert sys.platform == 'darwin' and Path('/usr/bin/sandbox-exec').exists()
assert subprocess.check_output(['node', '--version'], text=True).strip() == 'v22.14.0'
ports = [55589, 18859, 4295, 18860]
for port in ports:
    assert subprocess.run(['lsof', '-tiTCP:' + str(port), '-sTCP:LISTEN'], capture_output=True).returncode != 0, 'Occupied port; no shared process will be stopped'
out = Path(tempfile.mkdtemp(prefix='tdf-account-recovery-20261026-', dir='/private/tmp')); os.chmod(out, 0o700)
print(str(out), flush=True)
env = {k: v for k, v in os.environ.items() if k in ['HOME', 'PATH', 'USER', 'LOGNAME', 'SHELL', 'LANG', 'LC_CTYPE', 'TZ', 'TMPDIR']}
env['PATH'] = '/usr/local/opt/postgresql@16/bin:' + env['PATH']
env.update(PGHOST='127.0.0.1', PGPORT='55589', PGUSER=os.getlogin(), PGPASSFILE=str(out / 'no-passfile'), CI='false')
database = 'tdf_account_recovery_20261026_test'
url = 'postgresql://' + env['PGUSER'] + '@127.0.0.1:55589/' + database
report = {'startedAt': datetime.datetime.now(datetime.timezone.utc).isoformat(), 'source': args.expected_source, 'tree': git('rev-parse', args.expected_source + '^{tree}'), 'binarySource': baseline, 'binarySha256': expected_binary, 'sourceStatus': git('status', '--porcelain').splitlines(), 'steps': [], 'cleanup': {}, 'status': 'running', 'humanParticipants': 0, 'productionWrites': False, 'mailScope': 'Actual native MIME generation/SMTP transport to non-relaying loopback synthetic inbox; no real mailbox, TLS/provider/deliverability claim.'}
report['harnessSha256'] = {name: hashlib.sha256((packet / name).read_bytes()).hexdigest() for name in ['run.py', 'smtp.py', 'browser.mjs', 'loopback-bind.c']}
processes = []; started = created = False
def save(): (out / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
def run(name, command, extra=None, timeout=300):
    with (out / (name + '.log')).open('wb') as log:
        result = subprocess.run(command, cwd=root, env={**env, **(extra or {})}, stdout=log, stderr=subprocess.STDOUT, timeout=timeout)
    report['steps'].append({'step': name, 'exitCode': result.returncode}); save()
    if result.returncode: raise RuntimeError(name + ' failed; see owned private log')
    print(name + ' PASS', flush=True)
def scalar(query): return subprocess.check_output(['psql', url, '-XqAt', '-v', 'ON_ERROR_STOP=1', '-c', query], env=env, text=True).strip()
profile = '(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:55589"))(allow network-outbound (remote tcp "localhost:18860"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
def launch(name, command, variables):
    log = (out / (name + '.log')).open('wb')
    process = subprocess.Popen(command, cwd=root, env=variables, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
    processes.append((process, log))
try:
    dylib = out / 'loopback-bind.dylib'
    run('compile-loopback-bind-adapter', ['clang', '-dynamiclib', '-Wall', '-Wextra', '-Werror', str(packet / 'loopback-bind.c'), '-o', str(dylib)])
    lan = None
    for interface in ['en1', 'en0']:
        result = subprocess.run(['ipconfig', 'getifaddr', interface], text=True, capture_output=True)
        if result.returncode == 0: lan = result.stdout.strip(); break
    assert lan, 'LAN address required for isolation negative control'
    listener = socket.socket(); listener.bind(('0.0.0.0', 0)); listener.listen(2)
    try:
        testport = listener.getsockname()[1]
        with socket.create_connection((lan, testport), timeout=1): pass
        control = 'import socket,sys\ntry:socket.create_connection((sys.argv[1],int(sys.argv[2])),timeout=1)\nexcept PermissionError:sys.exit(0)\nexcept OSError:sys.exit(3)\nelse:sys.exit(2)'
        run('deny-nonloopback-egress', ['sandbox-exec', '-p', profile, sys.executable, '-c', control, lan, str(testport)])
    finally: listener.close()
    remote = None
    for item in socket.getaddrinfo('api.github.com', 443, family=socket.AF_INET, type=socket.SOCK_STREAM):
        candidate = item[4][0]
        try:
            with socket.create_connection((candidate, 443), timeout=3): pass
        except OSError: continue
        remote = candidate; break
    assert remote, 'Positive nonlocal TCP control required'
    run('deny-nonlocal-egress', ['sandbox-exec', '-p', profile, sys.executable, '-c', control, remote, '443'])
    (out / 'socket').mkdir(); (out / 'empty-env').mkdir()
    run('init', ['initdb', '-D', str(out / 'cluster'), '--username=' + env['PGUSER'], '--encoding=UTF8', '--locale=en_US.UTF-8', '--auth=trust'])
    run('start', ['pg_ctl', '-D', str(out / 'cluster'), '-l', str(out / 'postgres.log'), '-o', '-p 55589 -h 127.0.0.1 -k ' + str(out / 'socket'), 'start']); started = True
    run('create', ['createdb', '--template=template0', database]); created = True
    run('full-schema', ['node', 'scripts/test-music-integration-rehearsal.mjs'], {'TDF_MUSIC_REHEARSAL_DATABASE_URL': url})
    report['migrations'] = int(scalar('SELECT count(*) FROM tdf_schema_migration'))
    assert report['migrations'] == len(manifest) == 223
    report['ledgerHeaderTriggerEnabled'] = scalar("SELECT tgenabled::text FROM pg_trigger WHERE tgrelid='commerce_ledger_transaction'::regclass AND tgname='trg_commerce_ledger_header_immutable'")
    assert report['ledgerHeaderTriggerEnabled'] == 'O'
    build = out / 'build'
    if args.reuse_build_receipt:
        previous_path = Path(args.reuse_build_receipt).resolve()
        assert previous_path.parent.name.startswith('tdf-account-recovery-20261026-') and previous_path.parent.parent == Path('/private/tmp')
        previous = json.loads(previous_path.read_text())
        assert previous['source'] == args.expected_source and previous['tree'] == report['tree']
        assert all(previous['cleanup'].get(key) is True for key in ['databaseRemoved', 'clusterStopped', 'runtimesStopped'])
        assert {'step': 'build-web', 'exitCode': 0} in previous['steps']
        build = previous_path.parent / 'build'
        report['reusedBuildReceipt'] = str(previous_path)
    config = out / 'vite.mjs'
    config.write_text('import {loadConfigFromFile,mergeConfig} from ' + json.dumps(str(root / 'node_modules/vite/dist/node/index.js')) + ';\nconst loaded=await loadConfigFromFile({command:"build",mode:"production"},' + json.dumps(str(root / 'tdf-hq-ui/vite.config.ts')) + ');\nexport default mergeConfig(loaded.config,' + json.dumps({'root': str(root / 'tdf-hq-ui'), 'envDir': str(out / 'empty-env'), 'build': {'outDir': str(build), 'emptyOutDir': False}, 'preview': {'host': '127.0.0.1', 'port': 4295, 'strictPort': True}}) + ');\n')
    webenv = {'VITE_API_BASE': 'http://127.0.0.1:18859', 'VITE_POSTHOG_KEY': '', 'VITE_GOOGLE_CLIENT_ID': ''}
    if not args.reuse_build_receipt:
        run('build-web', ['node', 'node_modules/vite/bin/vite.js', 'build', '--config', str(config)], webenv)
    entry = re.search(r'<script[^>]+src="([^"]+)"', (build / 'index.html').read_text()).group(1)
    entry_bytes = (build / entry.lstrip('/')).read_bytes()
    assert args.expected_source.encode() in entry_bytes, 'Web build identity must match selected source'
    report['frontend'] = {'entry': entry, 'sha256': hashlib.sha256(entry_bytes).hexdigest(), 'sourceVerified': True, 'freshCompilation': not bool(args.reuse_build_receipt)}
    backend = {**env, 'APP_ENV': 'test', 'DATABASE_URL': url, 'APP_PORT': '18859', 'RESET_DB': 'false', 'RUN_MIGRATIONS': 'false', 'SEED_DB': 'false', 'DEFAULT_LOCALE': 'es', 'ALLOWED_ORIGINS': 'http://127.0.0.1:4295', 'HQ_APP_URL': 'http://127.0.0.1:4295', 'HQ_ASSETS_DIR': str(out), 'SMTP_HOST': '127.0.0.1', 'SMTP_PORT': '18860', 'SMTP_USERNAME': 'synthetic', 'SMTP_PASSWORD': 'synthetic-only', 'SMTP_FROM': 'sender@persona.test', 'SMTP_TLS': 'false'}
    for flag in ['EVENT_DISCOVERY_ENABLED', 'ARTIST_ENRICHMENT_ENABLED', 'EVENT_LOGISTICS_RECHECK_ENABLED', 'OPERATIONS_WORKER_ENABLED', 'CONTEXTUAL_REPUTATION_ENABLED', 'PUBLIC_REPUTATION_PROJECTION_ENABLED', 'COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED']: backend[flag] = 'false'
    wrapper = 'import os,sys;os.environ["DYLD_INSERT_LIBRARIES"]=sys.argv[1];os.environ["TDF_PILOT_API_PORT"]="18859";os.execv(sys.argv[2],[sys.argv[2]])'
    launch('smtp', [sys.executable, str(packet / 'smtp.py'), str(out)], env)
    launch('backend', ['sandbox-exec', '-p', profile, sys.executable, '-c', wrapper, str(dylib), str(binary)], backend)
    launch('web', ['node', 'node_modules/vite/bin/vite.js', 'preview', '--config', str(config)], {**env, **webenv})
    http = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    for endpoint in ['http://127.0.0.1:18859/health', 'http://127.0.0.1:4295/']:
        for _ in range(180):
            try:
                with http.open(endpoint, timeout=1) as response:
                    if response.status == 200: break
            except OSError: pass
            assert all(p.poll() is None for p, _ in processes), 'Owned runtime exited'
            time.sleep(.5)
        else: raise RuntimeError('Runtime startup deadline')
    for port in ports:
        listeners = subprocess.check_output(['lsof', '-nP', '-iTCP:' + str(port), '-sTCP:LISTEN'], text=True)
        assert '127.0.0.1:' + str(port) in listeners and '*:' + str(port) not in listeners
        try:
            with socket.create_connection((lan, port), timeout=1): raise AssertionError('LAN listener exposed')
        except OSError: pass
    report['networkIsolation'] = {'fourListenersLoopbackOnly': True, 'lanRequestsDenied': True, 'unlistedLanPortDeniedWithPositiveControl': True, 'nonlocalTcpDeniedWithPositiveControl': True, 'scope': 'Services forced to literal127.0.0.1 and four LAN ingress probes refused. SBPL allows local-machine PostgreSQL/SMTP destination ports only; separate positive-outside/denied-inside GitHub443 TCP control sends no application data. Bounded observed controls, not a theorem about all network traffic.'}
    with http.open('http://127.0.0.1:18859/version') as response: report['actualVersion'] = json.load(response)
    assert report['actualVersion']['commit'] == baseline
    run('browser', ['node', str(packet / 'browser.mjs')], {'TDF_RECOVERY_SOURCE': str(root), 'TDF_RECOVERY_OUT': str(out), 'TDF_RECOVERY_DATABASE_URL': url}, timeout=300)
    report['journey'] = json.loads((out / 'journey.json').read_text())
    report['status'] = 'VERIFIED-synthetic-local-only'
except BaseException as error:
    report['status'] = 'FAILED'; report['failureType'] = type(error).__name__; save(); raise
finally:
    for process, log in reversed(processes):
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try: process.wait(timeout=10)
            except subprocess.TimeoutExpired: os.killpg(process.pid, signal.SIGKILL); process.wait(timeout=10)
        log.close()
    report['cleanup']['runtimesStopped'] = all(p.poll() is not None for p, _ in processes)
    if created:
        try: run('drop', ['dropdb', database]); report['cleanup']['databaseRemoved'] = True
        finally:
            if started: run('stop', ['pg_ctl', '-D', str(out / 'cluster'), '-m', 'fast', '-w', 'stop']); report['cleanup']['clusterStopped'] = True
    elif started:
        run('stop', ['pg_ctl', '-D', str(out / 'cluster'), '-m', 'fast', '-w', 'stop']); report['cleanup']['clusterStopped'] = True
    report['finishedAt'] = datetime.datetime.now(datetime.timezone.utc).isoformat(); save()
