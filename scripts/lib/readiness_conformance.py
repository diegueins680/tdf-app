"""Failure/recovery through the real application and its nonce-owned PostgreSQL DB."""
import json
import re
import urllib.error
import urllib.request


def verify_readiness(run, name, port, check):
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

    status, body, headers = health()
    check('readiness freshly checks PostgreSQL with uncacheable success',
          status == 200 and body == {'status': 'ok', 'db': 'ok'} and headers.get('Cache-Control') == 'no-store')
    try:
        maintenance('ALTER DATABASE "'+name+'" ALLOW_CONNECTIONS false;')
        check('readiness fixture disabled only its owned database connections',
              maintenance("SELECT NOT datallowconn FROM pg_database WHERE datname='"+name+"';") == 't')
        maintenance("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='"+name+"' AND backend_type='client backend';")
        for attempt in range(3):
            status, body, headers = health()
            check('readiness rejects unavailable PostgreSQL without private details '+str(attempt),
                  status == 503 and body == {'status': 'degraded', 'db': 'unavailable'} and
                  headers.get('Cache-Control') == 'no-store' and headers.get('Retry-After') == '5')
    finally:
        maintenance('ALTER DATABASE "'+name+'" ALLOW_CONNECTIONS true;')
    status, body, headers = health()
    check('readiness recovers after PostgreSQL connections are restored',
          status == 200 and body == {'status': 'ok', 'db': 'ok'} and headers.get('Cache-Control') == 'no-store')
