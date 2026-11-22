#!/usr/bin/env python3
"""Exercise public card fields through the compiled API and an owned full-schema DB."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import socket
import subprocess
import time
import urllib.error
import urllib.request
import uuid


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--output', required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    binary = Path(args.binary).resolve()
    assert binary.is_file() and not (root / '.env').exists()
    output = Path(args.output).resolve()
    output.mkdir(parents=True, mode=0o700, exist_ok=False)
    ci = os.environ.get('GITHUB_ACTIONS') == 'true'
    native = os.environ.get('TDF_TEST_NATIVE_POSTGRES') == '1'
    assert ci or native, 'Only the CI service or an explicit native test server is allowed'
    assert not any(os.environ.get(key) for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE', 'PGOPTIONS'))
    host = 'postgres' if ci and not native else '127.0.0.1'
    port = '5432' if ci and not native else os.environ.get('TDF_TEST_NATIVE_POSTGRES_PORT', '55486')
    role = 'postgres' if ci and not native else os.environ.get('TDF_TEST_NATIVE_POSTGRES_USER', 'postgres')
    assert port.isdigit() and 0 < int(port) < 65536
    assert re.fullmatch(r'[a-z_][a-z0-9_]*', role)
    env = {key: value for key, value in os.environ.items()
           if key in ('HOME', 'PATH', 'USER', 'LOGNAME', 'SHELL', 'LANG', 'LC_CTYPE', 'TZ', 'TMPDIR')}
    if ci or os.environ.get('TDF_TEST_POSTGRES_PASSWORD'):
        assert os.environ.get('TDF_TEST_POSTGRES_PASSWORD'), 'Explicit test password required'
        env['PGPASSWORD'] = os.environ['TDF_TEST_POSTGRES_PASSWORD']
    database = 'tdf_public_event_cards_test_' + uuid.uuid4().hex[:12]
    routing = ['-h', host, '-p', port, '-U', role, '-w']
    manifest = json.loads((root / 'scripts/production-migrations.json').read_text())
    expected_ids = sorted(entry['id'] for entry in manifest['migrations'])
    paths = subprocess.check_output(['git', 'ls-files', '--', 'tdf-hq/src', 'tdf-hq/app',
                                     'tdf-hq/tdf-hq.cabal', 'tdf-hq/stack.yaml',
                                     'tdf-hq/stack.yaml.lock'], cwd=root, text=True).splitlines()
    paths += [entry['path'] for entry in manifest['migrations']]
    paths += ['scripts/production-migrations.json', 'scripts/test-public-event-card-tickets-http.py']

    def sources():
        return {path: hashlib.sha256((root / path).read_bytes()).hexdigest() for path in sorted(set(paths))}

    report = {'status': 'NOT_EXECUTED', 'head': subprocess.check_output(
        ['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip(),
        'worktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=root)),
        'binarySha256': hashlib.sha256(binary.read_bytes()).hexdigest(), 'sources': sources(),
        'requests': [], 'assertions': [], 'database': database}
    owned = False
    server = None
    backend_log = None
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))

    def run(command, timeout=180):
        result = subprocess.run(command, cwd=root, env=env, capture_output=True, timeout=timeout)
        assert result.returncode == 0, (command[0], result.stderr.decode()[-1500:])
        return result.stdout

    def sql(statement):
        return run(['psql', *routing, '-d', database, '-XqAt', '-v', 'ON_ERROR_STOP=1',
                    '-c', statement]).decode().strip()

    def apply(path):
        run(['psql', *routing, '-d', database, '-Xq', '-v', 'ON_ERROR_STOP=1', '-f', str(path)])

    def ledger():
        return json.loads(sql("SELECT coalesce(jsonb_agg(migration_id ORDER BY migration_id),'[]'::jsonb) FROM tdf_schema_migration"))

    def check(label, condition):
        assert condition, label
        report['assertions'].append(label)

    try:
        run(['createdb', *routing, database])
        owned = True
        apply(root / 'scripts/__tests__/fixtures/production-schema-20260814.sql')
        apply(root / 'scripts/__tests__/fixtures/catalog-production-source-fixture.sql')
        batch = run(['node', 'scripts/render-production-migration-batch.mjs'])
        (output / 'migrations.sql').write_bytes(batch)
        apply(output / 'migrations.sql')
        check('The complete canonical migration manifest is installed', ledger() == expected_ids)
        report['migrationCount'] = len(expected_ids)
        report['postgresVersion'] = sql('SHOW server_version')
        state = sql("SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code='social-event-lifecycle' AND state.code='on_sale' AND state.active AND workflow.active")
        planning = sql("SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code='social-event-lifecycle' AND state.code='planning' AND state.active AND workflow.active")
        assert uuid.UUID(state) and uuid.UUID(planning)
        for number in range(1, 8):
            metadata = json.dumps({'isPublic': number != 6, 'currency': 'USD'})
            workflow = planning if number == 7 else state
            capacity = '1' if number == 5 else 'NULL'
            sql(f"INSERT INTO social_event(id,title,start_time,end_time,price_cents,capacity,workflow_state_id,metadata) VALUES(918758000+{number},'Synthetic ticket card {number}',now()+interval '1 day',now()+interval '2 days',0,{capacity},'{workflow}','{metadata}')")
        sql("INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active) VALUES (918758001,'STANDARD','Synthetic standard',2500,'USD',10,0,true),(918758001,'CHEAP','Synthetic cheaper sold out',1500,'USD',10,10,true),(918758002,'STANDARD','Synthetic sold out',1500,'USD',10,10,true),(918758004,'USD','Synthetic USD',1500,'USD',10,0,true),(918758004,'EUR','Synthetic EUR',1200,'EUR',10,0,true),(918758005,'SOLD','Synthetic capacity consumed',1500,'USD',1,1,true),(918758005,'REMAINING','Synthetic tier stock',2000,'USD',10,0,true),(918758006,'PRIVATE','Synthetic private',900,'USD',10,0,true),(918758007,'PLANNING','Synthetic planning',900,'USD',10,0,true)")
        with socket.socket() as probe:
            probe.bind(('127.0.0.1', 0))
            api_port = probe.getsockname()[1]
        origin = f'http://127.0.0.1:{api_port}'
        assets = output / 'assets'
        assets.mkdir()
        runtime_env = {**env, 'APP_ENV': 'test', 'APP_PORT': str(api_port),
                       'DATABASE_URL': f'postgresql://{role}@{host}:{port}/{database}',
                       'RESET_DB': 'false', 'RUN_MIGRATIONS': 'false',
                       'AUTO_APPLY_PRODUCTION_MIGRATIONS': 'false', 'SEED_DB': 'false',
                       'EVENT_DISCOVERY_ENABLED': 'false', 'ARTIST_ENRICHMENT_ENABLED': 'false',
                       'EVENT_LOGISTICS_RECHECK_ENABLED': 'false', 'OPERATIONS_WORKER_ENABLED': 'false',
                       'HQ_ASSETS_DIR': str(assets), 'DEFAULT_LOCALE': 'es'}
        backend_log = (output / 'backend.log').open('wb')
        server = subprocess.Popen([str(binary)], cwd=root, env=runtime_env,
                                  stdin=subprocess.DEVNULL, stdout=backend_log,
                                  stderr=subprocess.STDOUT, start_new_session=True)
        for _ in range(120):
            assert server.poll() is None, 'Owned API exited during startup'
            try:
                with opener.open(origin + '/health', timeout=1) as response:
                    if response.status == 200:
                        break
            except OSError:
                pass
            time.sleep(.5)
        else:
            raise AssertionError('Owned API startup deadline')

        def request(path):
            with opener.open(origin + path, timeout=20) as response:
                report['requests'].append({'path': path, 'status': response.status})
                assert response.status == 200
                return json.load(response)

        report['actualVersion'] = request('/version')

        def cards():
            rows = request('/social-events/upcoming?limit=200')
            return {row['publicUpcomingEventId']: row for row in rows}

        def summary(row):
            return (row['publicUpcomingEventTicketFromPriceCents'],
                    row['publicUpcomingEventTicketCurrency'], row['publicUpcomingEventTicketsSoldOut'])

        rows = cards()
        check('Only in-stock tiers set the lowest advertised price', summary(rows['918758001']) == (2500, 'USD', False))
        check('On-sale tiers with no stock report sold out', summary(rows['918758002']) == (None, None, True))
        check('A reference price alone creates no ticket signal', summary(rows['918758003']) == (None, None, False))
        check('Mixed in-stock currencies create no single price', summary(rows['918758004']) == (None, None, False))
        check('Event-wide capacity caps otherwise available tier stock', summary(rows['918758005']) == (None, None, True))
        check('Private metadata never enters the anonymous list', '918758006' not in rows)
        check('A non-listable workflow never enters the anonymous list', '918758007' not in rows)
        sql("UPDATE event_ticket_tier SET quantity_sold=9 WHERE event_id=918758001 AND code='CHEAP'")
        check('The next HTTP read reflects changed canonical stock', summary(cards()['918758001']) == (1500, 'USD', False))
        sql('UPDATE event_ticket_tier SET is_active=false WHERE event_id=918758001')
        check('Inactive tiers create neither price nor sold-out signal', summary(cards()['918758001']) == (None, None, False))
        check('Read-only requests leave the migration ledger intact', ledger() == expected_ids)
        check('Executed backend and migration source inputs did not change', sources() == report['sources'])
        report['status'] = 'PASS'
    except BaseException as error:
        report['status'] = 'FAIL'
        report['error'] = str(error)
        raise
    finally:
        if server and server.poll() is None:
            os.killpg(server.pid, signal.SIGTERM)
            try:
                server.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(server.pid, signal.SIGKILL)
                server.wait(timeout=10)
        if backend_log:
            backend_log.close()
        if owned:
            run(['dropdb', *routing, database])
        report['databaseRemoved'] = owned
        (output / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'status': report['status'], 'assertions': len(report['assertions']),
                      'requests': len(report['requests']), 'migrations': report['migrationCount']}))


if __name__ == '__main__':
    main()
