"""Failure/recovery through the real application and its nonce-owned PostgreSQL DB."""
import json
import re
import time
import urllib.error
import urllib.request


def classify_health(status, body, headers):
    if headers.get('Cache-Control') != 'no-store':
        return 'invalid'
    if status == 200 and body == {'status': 'ok', 'db': 'ok'}:
        return 'ready'
    if (status == 503 and body == {'status': 'degraded', 'db': 'unavailable'}
            and headers.get('Retry-After') == '5'):
        return 'unavailable'
    return 'invalid'


def record_health(output, observations, phase, attempt, status, body, headers):
    kind = classify_health(status, body, headers)
    # Never retain arbitrary response bodies or headers: a broken handler may
    # itself disclose private data. Record only status and fixed-contract tests.
    observations.append({'phase': phase, 'attempt': attempt, 'httpStatus': status,
                         'responseKind': kind,
                         'cacheControlNoStore': headers.get('Cache-Control') == 'no-store',
                         'retryAfterFive': headers.get('Retry-After') == '5'})
    (output / 'readiness-responses.json').write_text(json.dumps(observations, indent=2) + '\n')
    return kind


def require_recovery(health, check, record, *, clock=time.monotonic, pause=time.sleep):
    # makePool retains up to10 connections. Reopening the database does not
    # revive terminated idle connections; fresh probes must discard them first.
    # This fixture allows only the documented503 while draining and bounds both
    # attempts and elapsed time. An arbitrary error can never be retried to green.
    deadline = clock() + 30
    recovered = False
    for attempt in range(1, 17):
        if clock() >= deadline:
            break
        status, body, headers = health()
        kind = record('restored', attempt, status, body, headers)
        check('readiness recovery response matches the documented contract ' + str(attempt),
              kind in ('ready', 'unavailable'))
        if kind == 'ready':
            recovered = clock() <= deadline
            break
        if attempt < 16 and clock() < deadline:
            pause(0.1)
    check('readiness recovers after PostgreSQL connections are restored', recovered)


def verify_readiness(run, name, port, check, output):
    assert re.fullmatch(r'tdf_booking_conformance_[a-f0-9]{12}_test', name)
    assert 0 < port < 65536

    def maintenance(sql):
        # Existing fixture authority, fixed maintenance DB, and only the exact
        # disposable database created by its owning caller may be modified.
        return run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'postgres', '-c', sql],
                   capture_output=True, text=True).stdout.strip()

    def health():
        opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        try:
            response = opener.open('http://127.0.0.1:'+str(port)+'/health', timeout=8)
        except urllib.error.HTTPError as error:
            response = error
        with response:
            return response.status, json.loads(response.read()), dict(response.headers)

    observations = []
    def record(phase, attempt, status, body, headers):
        return record_health(output, observations, phase, attempt, status, body, headers)

    status, body, headers = health()
    record('available', 1, status, body, headers)
    check('readiness freshly checks PostgreSQL with uncacheable success',
          status == 200 and body == {'status': 'ok', 'db': 'ok'} and headers.get('Cache-Control') == 'no-store')
    try:
        maintenance('ALTER DATABASE "'+name+'" ALLOW_CONNECTIONS false;')
        check('readiness fixture disabled only its owned database connections',
              maintenance("SELECT NOT datallowconn FROM pg_database WHERE datname='"+name+"';") == 't')
        maintenance("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='"+name+"' AND backend_type='client backend';")
        for attempt in range(3):
            status, body, headers = health()
            record('disabled', attempt + 1, status, body, headers)
            check('readiness rejects unavailable PostgreSQL without private details '+str(attempt),
                  status == 503 and body == {'status': 'degraded', 'db': 'unavailable'} and
                  headers.get('Cache-Control') == 'no-store' and headers.get('Retry-After') == '5')
    finally:
        maintenance('ALTER DATABASE "'+name+'" ALLOW_CONNECTIONS true;')
    require_recovery(health, check, record)
