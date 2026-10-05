#!/usr/bin/env python3
"""Actual PostgreSQL lock-wait expiry control in the dedicated synthetic fixture."""
import os
from pathlib import Path
import select
import subprocess
import time
import uuid

ROOT = Path(__file__).resolve().parent.parent
DSN = os.environ['TICKET_CONFIRMATION_TEST_DSN']
# Admit before the first connection, including direct invocation of this runner.
subprocess.run(['node', '--input-type=module', '-', DSN], cwd=ROOT, input="""
import assert from 'node:assert/strict';
import { disposablePostgresUrl } from './scripts/lib/disposable-postgres-url.mjs';
const url = disposablePostgresUrl(process.argv[2], {ci:process.env.CI === 'true'});
assert.equal(url.pathname, '/tdf_ticket_confirmation_worker_test');
""", text=True, check=True)
ENV = {**os.environ, 'PGCONNECT_TIMEOUT':'5', 'PGOPTIONS':'-c statement_timeout=10000 -c lock_timeout=8000'}
CMD = ['psql', DSN, '-XqAt', '-v', 'ON_ERROR_STOP=1']
def sql(statement):
    return subprocess.check_output(CMD + ['-c', statement], env=ENV, text=True).strip()
def apply(path):
    subprocess.run(CMD + ['-f', str(ROOT / path)], env=ENV, check=True, stdout=subprocess.DEVNULL)
assert sql('SELECT current_database()') == 'tdf_ticket_confirmation_worker_test'

def fresh_claim(expected):
    # Compare server-side anchors; client scheduling delays cannot fail a valid lease.
    sql("DELETE FROM event_ticket_confirmation_delivery; SELECT event_ticket_queue_confirmation(1)")
    result = sql("BEGIN; SELECT pg_sleep(1.2); CREATE TEMP TABLE claim_anchor AS SELECT clock_timestamp() AS at; "
                 "SELECT event_ticket_claim_confirmation('" + str(uuid.uuid4()) + "'); "
                 "SELECT lease_expires_at >= (SELECT at FROM claim_anchor)+interval '119.5 seconds' "
                 "FROM event_ticket_confirmation_delivery WHERE order_id=1; COMMIT")
    assert result.splitlines()[-1] == expected, result

def skip_locked_recovery(expected):
    sql("DELETE FROM event_ticket_confirmation_delivery; SELECT event_ticket_queue_confirmation(1)")
    sql("SELECT event_ticket_claim_confirmation('" + str(uuid.uuid4()) + "'); UPDATE event_ticket_confirmation_delivery SET lease_expires_at=clock_timestamp()-interval '1 second'")
    blocker = subprocess.Popen(CMD, env=ENV, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)
    try:
        blocker.stdin.write("BEGIN; SELECT order_id FROM event_ticket_confirmation_delivery WHERE order_id=1 FOR UPDATE;\n")
        blocker.stdin.flush()
        assert select.select([blocker.stdout], [], [], 5)[0]
        assert blocker.stdout.readline().strip() == '1'
        result = subprocess.run(CMD + ['-c', "SET lock_timeout='250ms'; SELECT event_ticket_claim_confirmation('" + str(uuid.uuid4()) + "') IS NULL"], env=ENV, text=True, capture_output=True, timeout=5)
        if expected:
            assert result.returncode == 0 and result.stdout.strip() == 't', result.stderr
        else:
            assert result.returncode != 0 and 'lock timeout' in result.stderr, result.stderr
    finally:
        if blocker.poll() is None:
            blocker.stdin.write('ROLLBACK;\n'); blocker.stdin.close()
            blocker.wait(timeout=5)

def waited_completion(expected):
    lease = str(uuid.uuid4())
    sql("DELETE FROM event_ticket_confirmation_delivery; SELECT event_ticket_queue_confirmation(1)")
    assert sql("SELECT event_ticket_claim_confirmation('" + lease + "')") == '1'
    sql("UPDATE event_ticket_confirmation_delivery SET lease_expires_at=clock_timestamp()+interval '2 seconds' WHERE order_id=1")
    blocker = subprocess.Popen(CMD, env=ENV, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)
    reader = None
    try:
        blocker.stdin.write("BEGIN; SELECT order_id FROM event_ticket_confirmation_delivery WHERE order_id=1 FOR UPDATE;\n")
        blocker.stdin.flush()
        assert select.select([blocker.stdout], [], [], 5)[0], 'Blocker did not acquire row'
        assert blocker.stdout.readline().strip() == '1'
        name = 'tdf_confirmation_expiry_' + uuid.uuid4().hex
        statement = "SET application_name='" + name + "'; SELECT event_ticket_finish_confirmation(1,'" + lease + "','accepted')"
        reader = subprocess.Popen(CMD + ['-c', statement], env=ENV, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        deadline = time.monotonic() + 5
        while sql("SELECT count(*) FROM pg_stat_activity WHERE application_name='" + name + "' AND wait_event_type='Lock'") != '1':
            assert time.monotonic() < deadline, 'Finisher never waited on the row lock'
            time.sleep(.02)
        while sql("SELECT lease_expires_at<=clock_timestamp() FROM event_ticket_confirmation_delivery WHERE order_id=1") != 't':
            assert time.monotonic() < deadline, 'Fixture lease did not expire'
            time.sleep(.02)
        blocker.stdin.write('COMMIT;\n'); blocker.stdin.close()
        assert blocker.wait(timeout=5) == 0
        out, err = reader.communicate(timeout=5)
        assert reader.returncode == 0, err
        assert out.strip() == expected, (expected, out)
        assert sql('SELECT state FROM event_ticket_confirmation_delivery WHERE order_id=1') == ('accepted' if expected == 't' else 'processing')
    finally:
        for process in [reader, blocker]:
            if process is not None and process.poll() is None:
                process.terminate(); process.wait(timeout=5)

# Known-invalid historical implementation must reproduce acceptance after expiry.
# This database is a dedicated synthetic fixture, never the application database.
try:
    apply('tdf-hq/sql/2026-10-05_ticket_confirmation_delivery.sql')
    waited_completion('t')
    fresh_claim('f')
    skip_locked_recovery(False)
    print('PASS negative control: transaction-stable clock admits expired waiting lease')
finally:
    apply('tdf-hq/sql/2026-10-05_ticket_confirmation_lease_clock.sql')
    apply('tdf-hq/sql/2026-10-05_ticket_confirmation_claim_clock.sql')
apply('tdf-hq/sql/2026-10-05_ticket_confirmation_lease_clock.sql')
waited_completion('f')
print('PASS real-clock lease completion rejects expiry during observed row-lock wait')

apply('tdf-hq/sql/2026-10-05_ticket_confirmation_claim_clock.sql')
fresh_claim('t')
print('PASS lease creation uses post-wait real time; historical transaction-clock mutation fails')
skip_locked_recovery(True)
print('PASS locked expired intent does not block recovery polling; historical mutation times out')
