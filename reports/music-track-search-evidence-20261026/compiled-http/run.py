#!/usr/bin/env python3
"""PREPARED ONLY: exact-c88 compiled search HTTP, owned fixtures, no media access."""
from pathlib import Path
import datetime
import getpass
import hashlib
import json
import os
import re
import secrets
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid

ROOT = Path('/private/tmp/tdf-universal-music-search-20261026')
PACKET = Path('/private/tmp/tdf-track-search-http-20261026')
BUILD = Path('/private/tmp/tdf-track-search-build-20261026')
EXPECTED = 'c88e3849f33f52619f786bf7f6ef4d2819b26fab'
BINARY = ROOT / 'tdf-hq/.stack-work/dist/x86_64-osx/ghc-9.10.3/build/tdf-hq-exe/tdf-hq-exe'
PG_PORT, API_PORT = 55585, 18835
API = f'http://127.0.0.1:{API_PORT}'
ALLOW_PATHS = frozenset(('/health', '/version', '/music/search'))


def sha(data):
    return hashlib.sha256(data).hexdigest()


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT, text=True).strip()


def validate_backend_paths(original, indexed, untracked, allowed_docs):
    assert set(allowed_docs) == {'tdf-hq/docs/openapi/music-search.yaml'}, 'Only the reviewed nonruntime OpenAPI document may be additional'
    assert original <= indexed and indexed - original <= set(allowed_docs), 'Tracked backend source/build path set differs from checkpoint'
    assert untracked <= set(allowed_docs), 'Unknown uncommitted backend additions forbidden'


def verify_backend():
    entries = subprocess.check_output(['git', 'ls-tree', '-rz', EXPECTED, '--', 'tdf-hq'], cwd=ROOT).split(b'\0')
    paths = set()
    for entry in entries:
        if not entry:
            continue
        metadata, relative = entry.split(b'\t', 1)
        mode, kind, object_id = metadata.decode().split()
        relative = relative.decode()
        assert kind == 'blob'
        path = ROOT / relative
        content = os.readlink(path).encode() if mode == '120000' else path.read_bytes()
        actual_mode = '120000' if path.is_symlink() else '100755' if path.stat().st_mode & 0o111 else '100644'
        assert mode == actual_mode and hashlib.sha1(b'blob ' + str(len(content)).encode() + b'\0' + content).hexdigest() == object_id, relative
        assert sha(content) == manifest['files'][relative], relative
        paths.add(relative)
    assert paths == set(manifest['files'])
    allowed_docs = review['allowedAdditionalBackendDocumentation']
    assert set(allowed_docs) == {'tdf-hq/docs/openapi/music-search.yaml'}, 'Only the reviewed nonruntime OpenAPI document may be additional'
    for relative, expected_hash in allowed_docs.items():
        document = ROOT / relative
        assert document.is_file() and not document.is_symlink() and not document.stat().st_mode & 0o111
        assert sha(document.read_bytes()) == expected_hash, 'Additional nonruntime documentation changed: ' + relative
    indexed_paths = {value.decode() for value in subprocess.check_output(['git', 'ls-files', '-z', '--', 'tdf-hq'], cwd=ROOT).split(b'\0') if value}
    untracked_paths = {value.decode() for value in subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard', '-z', '--', 'tdf-hq'], cwd=ROOT).split(b'\0') if value}
    validate_backend_paths(paths, indexed_paths, untracked_paths, allowed_docs)


def validate_request(path, method):
    parts = urllib.parse.urlsplit(path)
    assert method in ('GET', 'HEAD'), 'Only GET/HEAD metadata requests permitted'
    assert path.startswith('/') and not path.startswith('//')
    assert not parts.scheme and not parts.netloc and not parts.fragment
    assert parts.path in ALLOW_PATHS, 'Only health/version/music-search paths permitted'


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


# All source/build admission precedes creation of any output, database or process.
assert not sys.flags.optimize, 'Assertions must remain enabled'
assert sys.platform == 'darwin' and Path('/usr/bin/sandbox-exec').exists()
assert git('rev-parse', 'HEAD') == EXPECTED, 'Preserve the backend checkpoint until qualification'
assert not Path('/app/COMMIT').exists(), 'Runtime revision override forbidden'
manifest_bytes = (PACKET / 'backend-committed-inputs.json').read_bytes()
manifest = json.loads(manifest_bytes)
assert manifest['sourceRevision'] == EXPECTED and len(manifest['files']) == 973
assert (BUILD / 'backend-committed-inputs.json').read_bytes() == manifest_bytes
assert git('rev-parse', EXPECTED + ':tdf-hq') == manifest['backendGitTree']
review = json.loads((PACKET / 'review-manifest.json').read_text())
verify_backend()
for name, expected_hash in review['files'].items():
    assert sha((PACKET / name).read_bytes()) == expected_hash, name
for relative, expected_hash in review['frozenRuntimeHelperInputs'].items():
    assert sha((ROOT / relative).read_bytes()) == expected_hash, relative
assert BINARY.is_file() and (BUILD / 'stack-build.exit').read_text().strip() == '0', 'Actual fresh canonical application build exit0 required'
assert BINARY.stat().st_mtime >= datetime.datetime.fromisoformat(manifest['recordedAt']).timestamp(), 'Old linked binary forbidden'

OUT = Path(tempfile.mkdtemp(prefix='tdf-track-search-http-c88-', dir='/private/tmp'))
os.chmod(OUT, 0o700)
print(OUT, flush=True)
env = {k: v for k, v in os.environ.items() if k in ('HOME', 'USER', 'LOGNAME', 'SHELL', 'TZ', 'TMPDIR')}
env.update(PATH='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-node22-20261026/node-v22.14.0-darwin-x64/bin:/usr/local/bin:/usr/bin:/bin',
           LANG='en_US.UTF-8', LC_CTYPE='en_US.UTF-8', PGHOST='127.0.0.1', PGPORT=str(PG_PORT),
           PGUSER=getpass.getuser(), PGPASSFILE=str(OUT / 'no-passfile'), CI='false')
assert re.fullmatch(r'[A-Za-z_][A-Za-z0-9_]*', env['PGUSER'])
db = 'tdf_track_search_c88_' + secrets.token_hex(4) + '_test'
url = f"postgresql://{env['PGUSER']}@127.0.0.1:{PG_PORT}/{db}"
http = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())
procs = []
started = created = False


def context_snapshot():
    paths = set(git('diff', '--name-only', 'HEAD', '--', 'tdf-hq-ui', 'formal', 'docs', 'reports').splitlines())
    paths.update(git('ls-files', '--others', '--exclude-standard', '--', 'tdf-hq-ui/src', 'formal', 'docs', 'reports').splitlines())
    paths.update(review['allowedAdditionalBackendDocumentation'])
    return {p: sha((ROOT / p).read_bytes()) for p in sorted(paths) if (ROOT / p).is_file() and not (ROOT / p).is_symlink()}


report = {'status': 'RUNNING', 'source': EXPECTED, 'backendGitTree': manifest['backendGitTree'],
          'backendManifestSha256': sha(manifest_bytes), 'backendFilesVerified': len(manifest['files']),
          'additionalNonruntimeDocumentation': review['allowedAdditionalBackendDocumentation'],
          'reviewManifestSha256': sha((PACKET / 'review-manifest.json').read_bytes()),
          'binarySha256': sha(BINARY.read_bytes()), 'wholeCheckoutCleanClaimed': False,
          'frontendMetadataContextBefore': context_snapshot(), 'sourceHelperSha256': review['frozenRuntimeHelperInputs'],
          'startedAt': datetime.datetime.now(datetime.timezone.utc).isoformat(), 'steps': [], 'assertions': [], 'cleanup': {},
          'scope': 'Actual compiled GET/HEAD search on owned PostgreSQL16/full222schema and synthetic publication fixtures. No signup/login, HTTP mutation, S3/signing/media/object/browser/production/SLO claim.'}


def save():
    (OUT / 'results.json').write_text(json.dumps(report, indent=2, ensure_ascii=False) + '\n')


def run(label, args, extra=None, timeout=300):
    with (OUT / (label + '.log')).open('wb') as log:
        result = subprocess.run(args, cwd=ROOT, env={**env, **(extra or {})}, stdout=log, stderr=subprocess.STDOUT, timeout=timeout)
    report['steps'].append({'label': label, 'exitCode': result.returncode})
    save()
    assert result.returncode == 0, label + ' failed; inspect retained log'
    print(label + ' PASS', flush=True)


def sql(text):
    return subprocess.check_output(['psql', url, '-XqAt', '-v', 'ON_ERROR_STOP=1'], input=text, cwd=ROOT, env=env, text=True, timeout=60).strip()


def check(condition, label):
    assert condition, label
    report['assertions'].append(label)
    save()
    print(label + ' PASS', flush=True)


def request(path, method='GET', country='EC', expected_status=200, cache=False, timeout=10):
    validate_request(path, method)
    assert 0 < timeout <= 10, 'HTTP timeout must be positive and capped at10seconds'
    headers = {} if country is None else {'CF-IPCountry': country}
    req = urllib.request.Request(API + path, headers=headers, method=method)
    before = time.monotonic()
    try:
        response = http.open(req, timeout=timeout)
    except urllib.error.HTTPError as failure:
        response = failure
    body = response.read().decode()
    assert response.status == expected_status, (method, path, response.status, body[:300])
    if cache:
        check(response.headers.get_all('Cache-Control') == ['private, no-store'], 'Exactly one private/no-store ' + method + ' status' + str(expected_status))
    value = json.loads(body) if body and 'json' in response.headers.get('Content-Type', '') else body
    report['steps'].append({'label': method + ' ' + path.split('?')[0], 'status': response.status,
                            'durationMs': round((time.monotonic() - before) * 1000, 2)})
    return value, response.headers


def search(query='', country='EC', method='GET', expected_status=200, cache=True, **params):
    return request('/music/search?' + urllib.parse.urlencode({'q': query, **params}), method, country, expected_status, cache)[0]


def ids(rows):
    return [row['recordingId'] for row in rows]


def quote(value):
    return 'NULL' if value is None else "'" + str(value).replace("'", "''") + "'"


def make(slug, title, mode='full', reuse=None, publish=True):
    assert re.fullmatch(r'[a-z][a-z0-9-]{0,60}', slug)
    assert mode in ('full', 'album', 'mixed', 'us', 'preview', 'none', 'missing')
    if reuse is not None:
        assert str(uuid.UUID(reuse)) == reuse
    release, version = sql('SELECT release_id,version_id FROM music_track_search_publishable(' + ','.join(map(quote, (slug, title, mode, reuse))) + '::uuid)').split('|')
    recording = sql('SELECT recording_id FROM music_release_track WHERE release_version_id=' + quote(version) + ' AND track_number=1')
    item = {'release': release, 'version': version, 'recording': recording}
    if publish:
        publish_item(item)
    return item


def publish_item(item):
    sql("UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id=" + quote(item['version']) + '; SELECT pg_sleep(0.01)')
    sql('SELECT music_publish_due(100)')
    assert sql('SELECT state FROM music_release_version WHERE id=' + quote(item['version'])) == 'published'


def flag(enabled):
    value = sql("UPDATE revenue_feature_flag SET enabled=" + ('TRUE' if enabled else 'FALSE') + ",reason='Owned track-search HTTP synthetic fixture' WHERE environment='sandbox' AND flag_key='music_releases.public' RETURNING enabled")
    assert value == ('t' if enabled else 'f'), 'Require the one existing owned sandbox flag row'


profile = f'(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:{PG_PORT}"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
try:
    for port in (PG_PORT, API_PORT):
        assert subprocess.run(['lsof', '-tiTCP:' + str(port), '-sTCP:LISTEN'], capture_output=True).returncode != 0, 'Occupied port; never reuse a server'
        with socket.socket() as probe:
            probe.bind(('127.0.0.1', port))
    run('node-version', ['node', '--version'])
    report['nodeVersion'] = subprocess.check_output(['node', '--version'], env=env, text=True).strip()
    assert report['nodeVersion'] == 'v22.14.0'
    report['postgresVersion'] = subprocess.check_output(['pg_ctl', '--version'], env=env, text=True).strip()
    assert '16.' in report['postgresVersion']
    dylib = OUT / 'loopback-bind.dylib'
    adapter = ROOT / 'reports/welcome-club-integration-evidence-20261026/loopback-bind.c'
    report['bindAdapterSha256'] = sha(adapter.read_bytes())
    run('compile-bind-adapter', ['clang', '-dynamiclib', '-Wall', '-Wextra', '-Werror', str(adapter), '-o', str(dylib)])
    lan = None
    for interface in ('en1', 'en0'):
        result = subprocess.run(['ipconfig', 'getifaddr', interface], capture_output=True, text=True)
        if result.returncode == 0:
            lan = result.stdout.strip()
            break
    assert lan, 'Required LAN isolation control address unavailable'
    listener = socket.socket()
    listener.bind(('0.0.0.0', 0))
    listener.listen(2)
    try:
        port = listener.getsockname()[1]
        with socket.create_connection((lan, port), timeout=1):
            pass
        probe = 'import socket,sys\ntry:socket.create_connection((sys.argv[1],int(sys.argv[2])),timeout=1)\nexcept PermissionError:sys.exit(0)\nexcept OSError:sys.exit(3)\nelse:sys.exit(2)'
        run('egress-denied-positive-control', ['sandbox-exec', '-p', profile, sys.executable, '-c', probe, lan, str(port)])
    finally:
        listener.close()
    (OUT / 'socket').mkdir()
    run('initdb', ['initdb', '-D', str(OUT / 'cluster'), '--username=' + env['PGUSER'], '--encoding=UTF8', '--locale=en_US.UTF-8', '--auth=trust'])
    run('start-pg', ['pg_ctl', '-D', str(OUT / 'cluster'), '-l', str(OUT / 'postgres.log'), '-o', f'-p {PG_PORT} -h 127.0.0.1 -k ' + str(OUT / 'socket'), 'start'])
    started = True
    run('create-owned-db', ['createdb', '--template=template0', db])
    created = True
    check(sql('SELECT current_database()') == db and sql('SHOW data_directory') == str(OUT / 'cluster'), 'Exact fresh owned PostgreSQL identity')
    run('full222-production-schema', ['node', 'scripts/test-music-integration-rehearsal.mjs'], {'TDF_MUSIC_REHEARSAL_DATABASE_URL': url})
    check(sql('SELECT count(*) FROM tdf_schema_migration') == '222', 'Full222 ledger verified')
    run('generate-exact-authoritative-fixture', ['node', str(PACKET / 'fixture.mjs'), str(OUT / 'fixture.sql')])
    report['fixtureSqlSha256'] = sha((OUT / 'fixture.sql').read_bytes())
    sql((OUT / 'fixture.sql').read_text())
    album = make('album', 'A opening', 'album')
    b = sql('SELECT recording_id FROM music_release_track WHERE release_version_id=' + quote(album['version']) + ' AND track_number=2')
    flag(False)
    backend = {**env, 'APP_ENV': 'test', 'DEFAULT_LOCALE': 'es', 'DATABASE_URL': url, 'APP_PORT': str(API_PORT),
               'RESET_DB': 'false', 'RUN_MIGRATIONS': 'false', 'SEED_DB': 'false', 'ALLOWED_ORIGINS': 'http://127.0.0.1:4187',
               'ALLOW_ALL_ORIGINS': 'false', 'CORS_DISABLE_DEFAULTS': 'true', 'HQ_ASSETS_DIR': str(ROOT / 'tdf-hq/assets'),
               'MUSIC_TRUST_CF_IPCOUNTRY': 'true', 'COMMERCE_CHECKOUT_ENV': 'sandbox'}
    for name in ('EVENT_DISCOVERY_ENABLED', 'ARTIST_ENRICHMENT_ENABLED', 'EVENT_LOGISTICS_RECHECK_ENABLED', 'OPERATIONS_WORKER_ENABLED', 'CONTEXTUAL_REPUTATION_ENABLED', 'PUBLIC_REPUTATION_PROJECTION_ENABLED', 'COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED', 'TAX_INVOICE_WORKER_ENABLED', 'TICKET_CONFIRMATION_EMAIL_ENABLED'):
        backend[name] = 'false'
    wrapper = 'import os,sys;os.environ["DYLD_INSERT_LIBRARIES"]=sys.argv[1];os.environ["TDF_PILOT_API_PORT"]="18835";os.execv(sys.argv[2],[sys.argv[2],"+RTS","-N2","-RTS"])'
    log = (OUT / 'backend.log').open('wb')
    process = subprocess.Popen(['sandbox-exec', '-p', profile, sys.executable, '-c', wrapper, str(dylib), str(BINARY)], cwd=ROOT, env=backend, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
    procs.append((process, log))
    startup_started = time.monotonic()
    startup_deadline = startup_started + 90
    startup_ready = False
    report['startupBounds'] = {'monotonicDeadlineSeconds': 90, 'maximumRequestTimeoutSeconds': 10,
                               'requestTimeoutAlsoCappedToRemainingBudget': True,
                               'maximumPollingOverrunAllowanceSeconds': 10}
    while time.monotonic() < startup_deadline:
        remaining = startup_deadline - time.monotonic()
        if remaining <= 0:
            break
        assert process.poll() is None, 'Backend exited; retained backend.log'
        try:
            if request('/health', country=None, timeout=min(10, remaining))[0].get('status') == 'ok':
                startup_ready = True
                break
        except (OSError, AssertionError, AttributeError):
            pass
        assert process.poll() is None, 'Backend exited; retained backend.log'
        time.sleep(min(.5, max(0, startup_deadline - time.monotonic())))
    assert startup_ready, 'Startup deadline'
    assert time.monotonic() <= startup_deadline + 10, 'Startup exceeded bounded request-overrun allowance'
    report['startupElapsedSeconds'] = round(time.monotonic() - startup_started, 3)
    for port in (PG_PORT, API_PORT):
        listening = subprocess.check_output(['lsof', '-nP', '-iTCP:' + str(port), '-sTCP:LISTEN'], text=True)
        assert '127.0.0.1:' + str(port) in listening and '*:' + str(port) not in listening
        try:
            with socket.create_connection((lan, port), timeout=1):
                raise AssertionError('Listener accessible via LAN')
        except OSError:
            pass
    report['networkIsolation'] = {'listenersLoopback': True, 'lanDenied': True, 'nonLoopbackEgressDeniedPositiveControl': True, 'credentialsInherited': False}
    version, _ = request('/version', country=None)
    check(version['commit'] == EXPECTED, 'Actual compiled version matches c88 without environment revision override')
    report['actualVersion'] = version
    search('cancion noct', expected_status=404)
    check(True, 'Disabled public feature returns handler404/private-no-store')
    flag(True)
    for blank in ('', '   '):
        check(search(blank) == [], 'Blank query returns empty array')
    selected = search('cancion noct')
    check(ids(selected) == [b] and selected[0]['trackId'] == sql('SELECT id FROM music_release_track WHERE release_version_id=' + quote(album['version']) + ' AND track_number=2'), 'Specific B identity returned; never first track A')
    for query in ('Canción', 'CANCION', 'nocturna', 'B Canción', '  canción noct  ', 'Guest Artist'):
        check(ids(search(query)) == [b], 'Normalized title/artist match returns exact B: ' + query)
    check(len(search('Spec album')) == 2, 'Release match returns both authorized album tracks')
    check(search('x' * 200) == [], '200-character boundary accepted')
    search('x' * 201, expected_status=400)
    for parameters in ({'limit': 0}, {'limit': 101}, {'offset': -1}):
        search('Spec', expected_status=400, **parameters)
    search('Spec', expected_status=400, cache=False, limit='invalid')
    check(True, 'Invalid integer parser rejects400; no handler-cache promise for parser failure')
    check(search('cancion', method='HEAD') == '', 'Actual HEAD resolves search with private-no-store and no body')

    mixed = make('mixed', 'US hidden sentinel', 'mixed')
    private = make('private', 'Private sentinel', publish=False)
    withdrawn = make('withdrawn', 'Withdrawn sentinel')
    sql("UPDATE music_release_version SET state='suspended' WHERE id=" + quote(withdrawn['version']) + "; UPDATE music_release_version SET state='withdrawn' WHERE id=" + quote(withdrawn['version']))
    us = make('us', 'US release sentinel', 'us')
    denied = make('none', 'Listening denied sentinel', 'none')
    missing = make('missing', 'Missing rendition sentinel', 'missing', publish=False)
    check(int(sql('SELECT count(*) FROM music_check_submission(' + quote(missing['version']) + ") WHERE error_code='track_assets_missing'")) > 0, 'Real publication check rejects missing ready rendition; no trigger bypass')
    for item in (private, withdrawn, us, denied, missing):
        check(item['recording'] not in ids(search('Spec Artist')), 'Broad artist match excludes unavailable recording ' + item['recording'])
    for title in ('Private sentinel', 'Withdrawn sentinel', 'Listening denied sentinel', 'Missing rendition sentinel'):
        check(search(title) == [], 'Exact unavailable sentinel title never disclosed: ' + title)
    for query in ('US hidden sentinel', 'Spec mixed', 'Spec Artist'):
        check(mixed['recording'] not in ids(search(query)), 'Mixed hidden track remains absent even on broad context match: ' + query)
    check(ids(search('US hidden sentinel', country='US')) == [mixed['recording']], 'Actual trusted US header authorizes US-only track')
    check(ids(search('US release sentinel', country='US')) == [us['recording']], 'US-only release positive control')
    for country in (None, 'XX', 'T1', 'invalid'):
        check(search('US hidden sentinel', country=country) == [], 'Unknown/invalid territory fails closed: ' + str(country))
    check(search('US hidden sentinel', territory='US') == [], 'Untrusted territory query parameter cannot override trusted EC header')
    preview = make('preview', 'Preview sentinel', 'preview')
    check([x['role'] for x in search('Preview sentinel')[0]['sources']] == ['preview_audio'], 'Preview-only listening never yields full rendition')
    full = search('A opening')[0]
    check(sorted(x['role'] for x in full['sources']) == ['preview_audio', 'stream_audio', 'stream_audio'], 'Only full/current preview renditions returned; no master/pending/stale preview')
    check(len({x['assetId'] for x in full['sources']}) == 3, 'Quality variants do not duplicate source identities')
    duplicate = make('duplicate-context', 'A opening', reuse=album['recording'])
    check(ids(search('A opening')) == [album['recording']] and search('A opening')[0]['releaseId'] == duplicate['release'], 'Deduplicate canonical recording before pagination; newest matching context')
    check(next(row for row in search('Spec album') if row['recordingId'] == album['recording'])['releaseId'] == album['release'], 'Context matching precedes canonical deduplication')
    unicode = make('unicode', '夜の波')
    check(ids(search('夜の')) == [unicode['recording']] and search('不存在') == [], 'Non-Latin literal query never becomes match-all')
    check(search('%_') == [], 'Wildcards remain literal before matching fixture exists')
    literal = make('literal', 'Mix 100%_ready')
    check(ids(search('%_')) == [literal['recording']], 'Literal wildcard-containing title matches without broadening')
    check(search("'; DROP TABLE music_recording; --") == [], 'Actual parameter binding rejects SQL injection text as a match')
    for index in range(20):
        make('paging-' + str(index), 'Paging recording ' + str(index))
    all_rows = search('Spec Artist', limit=100)
    check(len(all_rows) >= 25 and len(ids(all_rows)) == len(set(ids(all_rows))), 'Enough unique authorized canonical recordings for13/12 lookahead pages')
    first, second = search('Spec Artist', limit=13, offset=0), search('Spec Artist', limit=13, offset=12)
    check(first == all_rows[:13] and second == all_rows[12:25] and first[:12] + second[:12] == all_rows[:24], 'Actual limit/offset bindings preserve13-request12-display ordering')
    check(search('Spec Artist', limit=1, offset=len(all_rows)) == [] and search('Spec Artist', limit=100) == all_rows, 'Terminal offset empty and repeated ordering stable')
    expected_keys = {'trackId', 'recordingId', 'releaseId', 'releaseVersionId', 'releaseSlug', 'releaseTitle', 'title', 'displayArtist', 'durationMs', 'sources'}
    for row in all_rows:
        assert set(row) == expected_keys and row['sources']
        for asset in row['sources']:
            assert set(asset) == {'assetId', 'role', 'mediaType', 'technicalMetadata'}
            assert all(key in ('bitrate_kbps', 'loudness_lufs') and type(value) in (int, float) for key, value in asset['technicalMetadata'].items())
            assert sql('SELECT music_public_asset_accessible(' + quote(asset['assetId']) + ",'EC')") == 't'
            assert sql('SELECT count(*) FROM music_asset WHERE id=' + quote(asset['assetId']) + ' AND release_version_id=' + quote(row['releaseVersionId']) + ' AND recording_id=' + quote(row['recordingId'])) == '1'
    check(True, 'All HTTP hits/source DTOs whitelist metadata and exact authorized context; no keys/URLs/owners/failure text')
    sql("UPDATE music_release_version SET state='suspended' WHERE id=" + quote(album['version']) + "; UPDATE music_release_version SET state='withdrawn' WHERE id=" + quote(album['version']))
    check(b not in ids(search('cancion noct')) and search('Spec album') == [], 'SQL withdrawal immediately removes fresh exact/context search disclosure')
    assert git('rev-parse', 'HEAD') == EXPECTED, 'Revision changed during qualification'
    verify_backend()
    for relative, expected_hash in review['frozenRuntimeHelperInputs'].items():
        assert sha((ROOT / relative).read_bytes()) == expected_hash, relative
    for name, expected_hash in review['files'].items():
        assert sha((PACKET / name).read_bytes()) == expected_hash, name
    check(sha(BINARY.read_bytes()) == report['binarySha256'], 'Compiled binary unchanged throughout qualification')
    report['frontendMetadataContextAfter'] = context_snapshot()
    report['frontendMetadataChangedDuringRun'] = report['frontendMetadataContextAfter'] != report['frontendMetadataContextBefore']
    report['status'] = 'FUNCTIONAL_CHECKS_PASSED_CLEANUP_PENDING'
    save()
except BaseException as failure:
    report.update(status='FAILED', failureType=type(failure).__name__, failure=str(failure)[:1000])
    save()
    raise
finally:
    for process, log in reversed(procs):
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=10)
        log.close()
    report['cleanup']['backendStopped'] = all(process.poll() is not None for process, _ in procs)
    if created:
        try:
            run('drop-owned-db', ['dropdb', db])
            report['cleanup']['databaseRemoved'] = True
        finally:
            if started:
                run('stop-owned-pg', ['pg_ctl', '-D', str(OUT / 'cluster'), '-m', 'fast', '-w', 'stop'])
                report['cleanup']['clusterStopped'] = True
    elif started:
        run('stop-owned-pg', ['pg_ctl', '-D', str(OUT / 'cluster'), '-m', 'fast', '-w', 'stop'])
        report['cleanup']['clusterStopped'] = True
    if report['status'] == 'FUNCTIONAL_CHECKS_PASSED_CLEANUP_PENDING':
        assert all(report['cleanup'].get(k) is True for k in ('backendStopped', 'databaseRemoved', 'clusterStopped')), 'Cleanup incomplete; never finalize PASS'
        report['status'] = 'PASS-real-compiled-track-search-http-synthetic-metadata-only'
    report['finishedAt'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
    save()
    (PACKET / 'latest-result-path.txt').write_text(str(OUT) + '\n')
