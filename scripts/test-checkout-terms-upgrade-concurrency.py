#!/usr/bin/env python3
"""Witness a writer between terms preflight and trigger installation on test DBs."""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
import uuid

ROOT = Path(__file__).resolve().parent.parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--migration', type=Path, default=ROOT/'tdf-hq/sql/2026-10-13_checkout_policy_terms_identity.sql')
parser.add_argument('--scope', choices=('course', 'event'), help='Run one independently mutated boundary')
options = parser.parse_args()
env = dict(os.environ)
for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE', 'PGOPTIONS'):
    env.pop(key, None)
url, container = env.get('TDF_TERMS_PGURL'), env.get('TDF_TERMS_CONTAINER')
if url:
    subprocess.run(['node', '--input-type=module', '-e',
        "import {disposablePostgresUrl} from './scripts/lib/disposable-postgres-url.mjs';"
        "disposablePostgresUrl(process.argv[1], {ci:process.env.CI==='true'});", url],
        cwd=ROOT, env=env, check=True)
else:
    assert container and re.fullmatch(r'tdf-terms-identity-migration-\d+', container), 'Owned test container required'


def command(name='terms-upgrade-observer'):
    psql = ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1']
    if url:
        return psql + ['-d', url], {**env, 'PGAPPNAME': name}
    return ['docker', 'exec', '-i', '-e', 'PGAPPNAME='+name, container,
            *psql, '-U', 'postgres', '-d', 'tdf_terms_identity_test'], env


def sql(statement):
    args, variables = command()
    return subprocess.check_output(args+['-c', statement], env=variables, text=True, timeout=15).strip()


def wait_for(predicate, label):
    deadline = time.monotonic()+10
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(.025)
    raise AssertionError(label)


def waiting(name, lock):
    return sql("SELECT count(*) FROM pg_stat_activity WHERE application_name='"+name+
               "' AND "+lock) == '1'


source = options.migration.read_text()
rollback = (ROOT/'tdf-hq/sql/2026-10-13_checkout_policy_terms_identity_rollback.sql').read_text()
marker = 'CREATE OR REPLACE FUNCTION course_checkout_policy_terms_identity()'
assert source.count(marker) == 1, 'Expected one preflight/install boundary'
for kind in ((options.scope,) if options.scope else ('course', 'event')):
    table = 'course_checkout_policy' if kind == 'course' else 'event_ticket_checkout_policy'
    parent = 'course' if kind == 'course' else 'social_event'
    scope, text_column = kind+'_id', 'cancellation_policy' if kind == 'course' else 'refund_policy'
    assert sql('SELECT count(*) FROM '+parent+' WHERE id=1001') == '0', 'Fixture scope already exists'
    sql(rollback)
    if kind == 'course':
        sql("INSERT INTO course(id,slug,title,price_cents,currency,capacity) "
            "VALUES(1001,'terms-upgrade-race','Race',100,'USD',1)")
    else:
        sql("INSERT INTO social_event(id,title,start_time,end_time) "
            "VALUES(1001,'Race',now(),now()+interval '1 hour')")
    extra, value = (',price_minor', ',100') if kind == 'course' else ('', '')

    def insert(version, text):
        return (f'INSERT INTO {table}({scope},policy_version,currency,terms_version,terms_summary,'
                f"{text_column}{extra}) VALUES(1001,'race-{version}','USD','race-terms','{text}',"
                f"'policy'{value})")

    sql(insert('original', 'Original terms'))
    key, name = int(uuid.uuid4().hex[:14], 16), 'terms-upgrade-'+uuid.uuid4().hex
    holder = migration = writer = None
    with tempfile.TemporaryFile(mode='w+') as output:
        try:
            args, variables = command(name+'-holder')
            holder = subprocess.Popen(args, env=variables, stdin=subprocess.PIPE,
                                      stdout=output, stderr=output, text=True)
            holder.stdin.write(f"SELECT pg_advisory_lock({key}); SELECT 'held';\n")
            holder.stdin.flush()
            wait_for(lambda: waiting(name+'-holder', "state='idle'"), 'Barrier holder not ready')
            # Test-only scheduling point: the unmodified preflight has completed;
            # the unmodified trigger DDL has not started. Never persisted in SQL.
            injected = source.replace(marker, f'SELECT pg_advisory_lock({key});\n'
                                      f'SELECT pg_advisory_unlock({key});\n'+marker)
            args, variables = command(name)
            migration = subprocess.Popen(args, env=variables, stdin=subprocess.PIPE,
                                         stdout=output, stderr=output, text=True)
            migration.stdin.write(injected)
            migration.stdin.close()
            wait_for(lambda: waiting(name, "wait_event='advisory'"), 'Migration did not reach post-preflight barrier')
            args, variables = command(name+'-writer')
            writer = subprocess.Popen(args+['-c', insert('conflict', 'Changed terms')], env=variables,
                                      stdout=output, stderr=output, text=True)
            wait_for(lambda: writer.poll() is not None or waiting(name+'-writer', "wait_event_type='Lock'"),
                     'Writer neither completed nor waited for a lock')
            blocked = writer.poll() is None
            holder.stdin.write(f'SELECT pg_advisory_unlock({key});\n\\q\n')
            holder.stdin.flush()
            holder.wait(timeout=10)
            migration.wait(timeout=15)
            writer.wait(timeout=15)
            variants = int(sql(f'SELECT count(DISTINCT terms_summary) FROM {table} '
                               f"WHERE {scope}=1001 AND terms_version='race-terms'"))
            result = {'table': table, 'migrationExit': migration.returncode, 'writerExit': writer.returncode,
                      'writerBlockedBeforeInstall': blocked, 'variantsAfterInstallation': variants}
            print(json.dumps(result), flush=True)
            output.seek(0)
            diagnostic = output.read()
            assert migration.returncode == 0, diagnostic
            assert blocked and writer.returncode != 0 and variants == 1, \
                'Concurrent terms writer crossed the preflight/install boundary: '+json.dumps(result)
            assert 'already names different terms' in diagnostic, diagnostic
        finally:
            for process in (holder, migration, writer):
                if process is not None and process.poll() is None:
                    process.terminate()
                    process.wait(timeout=10)
            sql(f'DELETE FROM {table} WHERE {scope}=1001')
            sql(f'DELETE FROM {parent} WHERE id=1001')
print('Terms upgrade writer fencing passed for both policy tables.')
