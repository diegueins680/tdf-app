from pathlib import Path
import os, subprocess, json, hashlib, time, signal, getpass

work = Path('/private/tmp/tdf-music-worker-completion-20261029')
out = Path('/private/tmp/tdf-worker-concurrency-completion-20261029')
out.mkdir()
data = Path('/private/tmp/tdf-correctness-cycle-pg-20261028')
pgbin = Path('/usr/local/opt/postgresql@16/bin')
env = os.environ.copy()
for key in ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE', 'PGPASSWORD', 'DATABASE_URL', 'CI']:
    env.pop(key, None)
env['PATH'] = '/private/tmp/tdf-correctness-bin-20261028:/Users/macpro/.nvm/versions/node/v24.13.1/bin:' + str(pgbin) + ':' + env['PATH']
env.update(PGHOST='127.0.0.1', PGPORT='55728', PGUSER='postgres')
record = {'startedAt': time.time(), 'baseline': 'e302dc7fa76ac1c20e4e95cb946c82019ee17bb2',
          'scope': 'Seven actual music concurrency scenarios with the corrected observation helper; same30s/close-based completion and product worker bytes. Read-only owned process/PG samples perturb scheduling; local bounded empirical assurance, not hosted qualification or product shutdown repair.'}
def pg(query):
    return subprocess.check_output([str(pgbin / 'psql'), '-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', 'postgres', '-c', query], env=env, text=True, timeout=5).strip()
def git(*args): return subprocess.check_output(['git', '-C', str(work), *args])
assert git('rev-parse', 'HEAD').decode().strip() == record['baseline']
assert set(git('diff','--name-only').decode().splitlines())=={'formal/system/requirements.json','scripts/quality-repo.sh','scripts/test-music-release-worker-runtime.mjs'}
assert set(git('ls-files','--others','--exclude-standard').decode().splitlines())=={'formal/system/music-worker-completion-observation.md','scripts/__tests__/test-child-completion.test.mjs','scripts/lib/test-child-completion.mjs'}
paths = ['scripts/test-music-release-worker-runtime.mjs', 'scripts/run-music-release-worker-once.sh', 'scripts/lib/music-test-database.mjs','scripts/lib/test-child-completion.mjs','scripts/__tests__/test-child-completion.test.mjs','scripts/quality-repo.sh','formal/system/requirements.json','formal/system/music-worker-completion-observation.md']
record['sourceSha256'] = {p: hashlib.sha256((work / p).read_bytes()).hexdigest() for p in paths}
started = False
proc = None
samples = []
try:
    status = subprocess.run([str(pgbin / 'pg_ctl'), '-D', str(data), 'status'], env=env, capture_output=True)
    assert status.returncode == 3, 'Expected stopped own synthetic cluster; do not share an active server'
    with (out / 'postgres.log').open('w') as log:
        subprocess.run([str(pgbin / 'pg_ctl'), '-D', str(data), '-l', str(out / 'postgres-server.log'), '-o', '-h127.0.0.1 -p55728 -k/private/tmp', 'start'], env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=20)
    started = True
    assert Path(pg('SHOW data_directory')).resolve() == data.resolve()
    record['dataDirectoryVerified'] = str(data)
    assert pg("SELECT count(*) FROM pg_stat_activity WHERE datname IS NOT NULL AND pid<>pg_backend_pid()") == '0', 'Unknown existing clients'
    env['TDF_MUSIC_WORKER_TEST_DATABASE_URL'] = 'postgresql://postgres@127.0.0.1:55728/tdf_worker_probe_test'
    env['MUSIC_WORKER_DIAGNOSTICS'] = 'true'
    with (out / 'worker-runtime.txt').open('w') as log:
        proc = subprocess.Popen(['node', 'scripts/test-music-release-worker-runtime.mjs', '--concurrency-only'], cwd=work, env=env, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        record['runnerPid'] = proc.pid
        owned = {proc.pid}
        deadline = time.monotonic() + 300
        while proc.poll() is None and time.monotonic() < deadline:
            rows = []
            for line in subprocess.check_output(['ps', '-axo', 'pid=,ppid=,pgid=,stat=,comm='], text=True, timeout=5).splitlines():
                f = line.split(None, 4)
                if len(f) == 5: rows.append({'pid': int(f[0]), 'ppid': int(f[1]), 'pgid': int(f[2]), 'state': f[3], 'command': f[4]})
            for _ in range(20):
                before = len(owned)
                owned.update(r['pid'] for r in rows if r['ppid'] in owned)
                if len(owned) == before: break
            pattern = '^tdf_music_worker_' + str(proc.pid) + '_[a-f0-9]{32}$'
            snapshot = {'at': time.time(), 'processes': [r for r in rows if r['pid'] in owned]}
            try:
                snapshot['postgresActivity'] = json.loads(pg("SELECT coalesce(json_agg(s),'[]') FROM (SELECT pid,state,wait_event_type,wait_event,query_start,left(query,600) AS query FROM pg_stat_activity WHERE datname ~ '" + pattern + "')s"))
                snapshot['postgresLocks'] = json.loads(pg("SELECT coalesce(json_agg(s),'[]') FROM (SELECT l.pid,l.locktype,l.mode,l.granted,l.relation,l.transactionid FROM pg_locks l JOIN pg_stat_activity a ON a.pid=l.pid WHERE a.datname ~ '" + pattern + "')s"))
            except Exception as error: snapshot['observationError'] = type(error).__name__ + ': ' + str(error)[:200]
            samples.append(snapshot)
            time.sleep(.5)
        if proc.poll() is None:
            record['outerDeadlineExceeded'] = True
            os.killpg(proc.pid, signal.SIGTERM)
            try: proc.wait(timeout=8)
            except subprocess.TimeoutExpired:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.wait(timeout=5)
        record['actualExit'] = proc.returncode
    record['remainingOwnedFixtureSessions'] = pg("SELECT count(*) FROM pg_stat_activity WHERE datname ~ '^tdf_music_worker_" + str(proc.pid) + "_[a-f0-9]{32}$'")
    record['remainingOwnedFixtureDatabases'] = pg("SELECT count(*) FROM pg_database WHERE datname ~ '^tdf_music_worker_" + str(proc.pid) + "_[a-f0-9]{32}$'")
finally:
    (out / 'observations.json').write_text(json.dumps(samples, indent=2) + '\n')
    if started:
        # Stop this exclusively started synthetic cluster; preserve every DB.
        stopped = subprocess.run([str(pgbin / 'pg_ctl'), '-D', str(data), '-m', 'fast', 'stop'], env=env, capture_output=True, text=True, timeout=20)
        record['postgresStopExit'] = stopped.returncode
        (out / 'postgres-stop.txt').write_text(stopped.stdout + stopped.stderr)
    record['finishedAt'] = time.time()
    record['sourceUnchanged'] = all(hashlib.sha256((work / p).read_bytes()).hexdigest() == digest for p, digest in record['sourceSha256'].items())
    (out / 'execution.json').write_text(json.dumps(record, indent=2) + '\n')
assert record.get('actualExit')==0 and 'Music worker runtime: 7 scenarios passed' in (out/'worker-runtime.txt').read_text()
print(json.dumps(record, indent=2), flush=True)
raise SystemExit(record.get('actualExit', 1))
