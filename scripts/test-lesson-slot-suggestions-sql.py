#!/usr/bin/env python3
"""SCHOOL-SLOT-SUGGESTION-001: the schedule migration and the snapshot query on PostgreSQL.

Nothing is restated here that the source defines:
  * the migration is tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql;
  * the query text is extracted from tdf-hq/src/TDF/Trials/SlotSuggestions.hs;
  * the table shapes are extracted from the production schema fixture and from
    the migration that created service_booking_resource_allocation;
  * with ORACLE_BIN, feasibility is asked of the compiled Haskell core
    (tdf-slot-suggestions-oracle), not recomputed.

It creates one nonce schema in a disposable database and always drops it.

Usage: test-lesson-slot-suggestions-sql.py DATABASE_URL [ORACLE_BIN]

Without ORACLE_BIN the correspondence between the Haskell predicate and the
exclusion constraint is NOT checked, and the last line says so.
"""
import json
import os
import re
import subprocess
import sys
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tdf-hq/src/TDF/Trials/SlotSuggestions.hs'
MIGRATION = ROOT / 'tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql'
ROLLBACK = ROOT / 'tdf-hq/sql/2026-11-21_school_schedule_occupancy_rollback.sql'
ALLOCATION = ROOT / 'tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql'
PRODUCTION_SCHEMA = ROOT / 'scripts/__tests__/fixtures/production-schema-20260814.sql'
SCHEMA = 'lesson_slot_test_' + uuid.uuid4().hex[:12]
TABLES = ['resource', 'booking', 'booking_resource', 'class_session', 'trial_assignment',
          'teacher_availability', 'teacher_subject', 'subject_room_preference',
          'class_package_purchase', 'package_catalog']
checks = []


def check(label, ok, detail=''):
    if not ok:
        raise SystemExit(f'FAILED: {label} {detail}')
    checks.append(label)


def extract_query(name, parameters):
    """The string literal bound to `name` in the Haskell source."""
    src = SOURCE.read_text()
    match = re.search(rf'^{name} =\n((?:  .*\n)+)', src, re.MULTILINE)
    if not match:
        raise SystemExit(f'{name} not found; update the extractor with the source')
    literal = re.sub(r'\\\s*\n\s*\\', '', match.group(1)).strip()
    if not (literal.startswith('"') and literal.endswith('"')) or '"' in literal[1:-1]:
        raise SystemExit(f'Unexpected {name} shape; update the extractor with the source')
    query = literal[1:-1]
    if query.count('?') != parameters:
        raise SystemExit(f'{name} takes {query.count("?")} parameters, expected {parameters}')
    return query


def production_table(name):
    schema = PRODUCTION_SCHEMA.read_text()
    match = re.search(rf'^CREATE TABLE public\.{name} \(\n(.*?)^\);', schema, re.MULTILINE | re.DOTALL)
    if not match:
        raise SystemExit(f'{name} not found in the production schema fixture')
    body = match.group(1).replace('    id bigint NOT NULL', '    id bigserial PRIMARY KEY', 1)
    return f'CREATE TABLE {name} (\n{body});\n'


def allocation_table():
    match = re.search(r'^CREATE TABLE IF NOT EXISTS service_booking_resource_allocation \(.*?^\);',
                      ALLOCATION.read_text(), re.MULTILINE | re.DOTALL)
    if not match or 'EXCLUDE USING gist' not in match.group(0):
        raise SystemExit('service_booking_resource_allocation definition not found')
    return match.group(0) + '\n'


def transaction_body(path):
    """A migration file without its own BEGIN/COMMIT, to run inside ours."""
    text = path.read_text()
    if text.count('BEGIN;\n') != 1 or text.count('COMMIT;') != 1:
        raise SystemExit(f'{path.name} must hold exactly one transaction')
    return text.replace('BEGIN;\n', '', 1).rsplit('COMMIT;', 1)[0]


def psql(url, script, expect_failure=None):
    env = dict(os.environ, PGOPTIONS=f'-c search_path={SCHEMA},public -c timezone=UTC')
    result = subprocess.run(['psql', url, '-X', '-q', '-A', '-t', '-F', '|', '-v', 'ON_ERROR_STOP=1'],
                            input=script, text=True, capture_output=True, env=env)
    if expect_failure is None:
        if result.returncode:
            raise SystemExit(f'psql failed:\n{result.stderr}\n--- script ---\n{script[:1500]}')
        return result.stdout
    if result.returncode == 0 or expect_failure not in result.stderr:
        raise SystemExit(f'Expected failure "{expect_failure}", got rc={result.returncode}: {result.stderr[:600]}')
    return result.stderr


# Teacher 10 (teaches subject 1), teacher 11; students 20, 21; rooms 1 (Sala A),
# 2 (Sala B), 3 (inactive), 4 (equipment). All instants are in December 2030.
T = '2030-12-02 '
FIXTURE = f"""
INSERT INTO resource (id, name, slug, resource_type, capacity, active) VALUES
  (1,'Sala A','sala-a','Room',NULL,true), (2,'Sala B','sala-b','Room',NULL,true),
  (3,'Sala C','sala-c','Room',NULL,false), (4,'Consola','consola','Equipment',NULL,true);
INSERT INTO teacher_subject (teacher_id, subject_id) VALUES (10, 1);
INSERT INTO subject_room_preference (subject_id, room_id, priority) VALUES (1, 1, 2), (1, 2, 1), (1, 3, 1);
INSERT INTO package_catalog (id, subject_id, name, hours_qty, price_cents, expires_days, refund_policy, active)
  VALUES (1, 1, 'Paquete', 8, 10000, 30, 'none', true), (2, 2, 'Otro', 8, 10000, 3650, 'none', true);
INSERT INTO class_package_purchase (student_id, package_id, price_cents, purchased_at, status) VALUES
  (20, 1, 10000, '2030-11-20 00:00+00', 'Open'), (20, 1, 10000, '2030-11-25 00:00+00', 'Closed'),
  (20, 2, 10000, '2030-11-20 00:00+00', 'Open');
INSERT INTO teacher_availability (teacher_id, subject_id, room_id, start_at, end_at) VALUES
  (10, 1, 1, '{T}13:00+00', '{T}18:00+00'), (10, 1, 2, '{T}13:00+00', '{T}16:00+00'),
  (10, 2, 1, '{T}18:00+00', '{T}23:00+00');
-- A lesson that ended long ago and overlaps another: history, never projected.
INSERT INTO class_session (student_id, teacher_id, subject_id, start_at, end_at, room_id) VALUES
  (20, 10, 1, '2020-01-01 10:00+00', '2020-01-01 11:00+00', 1),
  (21, 10, 1, '2020-01-01 10:30+00', '2020-01-01 11:30+00', 1);
INSERT INTO class_session (id, student_id, teacher_id, subject_id, start_at, end_at, room_id) VALUES
  (100, 21, 10, 1, '{T}14:00+00', '{T}15:00+00', 1);
INSERT INTO trial_assignment (id, request_id, teacher_id, start_at, end_at, room_id) VALUES
  (200, 1, 11, '{T}15:00+00', '{T}16:00+00', 2);
INSERT INTO booking (id, title, starts_at, ends_at, status, created_at) VALUES
  (300, 'Studio', '{T}16:00+00', '{T}17:00+00', 'Confirmed', now()),
  (301, 'Cancelled', '{T}13:00+00', '{T}14:00+00', 'Cancelled', now()),
  (302, 'Legacy without allocation', '{T}17:30+00', '{T}18:00+00', 'Confirmed', now());
INSERT INTO booking_resource (booking_id, resource_id, role) VALUES (300, 1, 'room'), (301, 1, 'room'), (302, 1, 'room');
INSERT INTO service_booking_resource_allocation (booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at) VALUES
  (300, 1, '{T}16:00+00', '{T}17:00+00', 'reserved', '{T}17:00+00'),
  (301, 1, '{T}13:00+00', '{T}14:00+00', 'released', '{T}14:00+00');
SELECT setval(pg_get_serial_sequence('class_session','id'), 1000);
SELECT setval(pg_get_serial_sequence('trial_assignment','id'), 1000);
"""


def lesson(teacher, student, room, start, end):
    return (f"INSERT INTO class_session (student_id, teacher_id, subject_id, start_at, end_at, room_id) "
            f"VALUES ({student}, {teacher}, 1, '{start}', '{end}', {room});")


def micros(url, stamp):
    return int(psql(url, f"SELECT (extract(epoch FROM '{stamp}'::timestamptz) * 1000000)::bigint;").strip())


def main():
    if len(sys.argv) not in (2, 3):
        raise SystemExit(__doc__)
    url = sys.argv[1]
    oracle = sys.argv[2] if len(sys.argv) == 3 else None
    query = extract_query('slotSnapshotSql', 6)
    migration = transaction_body(MIGRATION)
    psql(url, f'CREATE SCHEMA {SCHEMA};')
    try:
        run(url, oracle, query, migration)
    finally:
        psql(url, f'DROP SCHEMA IF EXISTS {SCHEMA} CASCADE;')
    print(f'Lesson slot suggestion SQL checked: {len(checks)} checks on PostgreSQL')
    if oracle is None:
        print('NOT CHECKED: correspondence between the Haskell predicate and the exclusion '
              'constraint (pass the tdf-slot-suggestions-oracle binary as the second argument)')


def run(url, oracle, query, migration):
    psql(url, 'CREATE EXTENSION IF NOT EXISTS btree_gist;\n'
         + ''.join(production_table(name) for name in TABLES) + allocation_table() + FIXTURE)

    # --- migration -----------------------------------------------------------
    # Negative control first: a future overlap that already exists must abort it.
    psql(url, 'BEGIN;\n' + lesson(10, 20, 2, T + '14:30+00', T + '15:30+00') + migration + 'COMMIT;',
         expect_failure='school_schedule_occupancy_no_overlap')
    check('migration aborts on a pre-existing future teacher overlap', True)
    psql(url, 'BEGIN;\n' + lesson(11, 20, 1, T + '16:30+00', T + '17:30+00') + migration + 'COMMIT;',
         expect_failure='school_schedule_occupancy_no_overlap')
    check('migration aborts on a pre-existing lesson over a studio allocation', True)
    psql(url, 'BEGIN;\n' + lesson(11, 20, 2, T + '19:00+00', T + '19:00+00') + migration + 'COMMIT;',
         expect_failure='school_schedule_occupancy_interval')
    check('migration aborts on a pre-existing empty interval', True)
    check('an aborted migration leaves nothing behind',
          psql(url, "SELECT to_regclass('school_schedule_occupancy') IS NULL;").strip() == 't')

    psql(url, 'BEGIN;\n' + migration + 'COMMIT;')
    projected = psql(url, 'SELECT source_kind, subject_kind, subject_id FROM school_schedule_occupancy ORDER BY 1, 2, 3;')
    check('backfill projects future lessons, trials and active allocations only',
          projected.split() == ['class_session|resource|1', 'class_session|student|21', 'class_session|teacher|10',
                                'resource_allocation|resource|1', 'trial_assignment|resource|2',
                                'trial_assignment|teacher|11'], projected)
    psql(url, 'BEGIN;\n' + migration + 'COMMIT;')
    check('migration is repeatable',
          psql(url, 'SELECT count(*) FROM school_schedule_occupancy;').strip() == '6')

    # --- the exclusion constraint is the authority ---------------------------
    overlap = 'school_schedule_occupancy_no_overlap'
    psql(url, lesson(10, 20, 2, T + '14:30+00', T + '15:30+00'), expect_failure=overlap)
    check('teacher cannot hold two overlapping lessons', True)
    psql(url, lesson(11, 21, 2, T + '13:30+00', T + '14:30+00'), expect_failure=overlap)
    check('student cannot hold two overlapping lessons', True)
    psql(url, lesson(11, 20, 1, T + '14:59+00', T + '15:30+00'), expect_failure=overlap)
    check('room cannot hold two overlapping lessons', True)
    psql(url, lesson(11, 20, 1, T + '15:30+00', T + '16:00+00'), expect_failure=overlap)
    check('a trial lesson blocks its teacher for lessons', True)
    psql(url, lesson(10, 20, 2, T + '15:30+00', T + '16:30+00'), expect_failure=overlap)
    check('a trial lesson blocks its room for lessons', True)
    psql(url, lesson(10, 20, 1, T + '16:30+00', T + '17:30+00'), expect_failure=overlap)
    check('a studio allocation blocks its room for lessons', True)
    psql(url, "INSERT INTO service_booking_resource_allocation (booking_id, resource_id, starts_at, ends_at, "
              f"allocation_status, hold_expires_at) VALUES (302, 1, '{T}14:30+00', '{T}15:30+00', 'holding', '{T}15:30+00');",
         expect_failure=overlap)
    check('a lesson blocks its room for studio allocations', True)
    # Half-open: the instant a commitment ends is free. Each probe is rolled back.
    psql(url, 'BEGIN;\n' + lesson(10, 21, 1, T + '15:00+00', T + '16:00+00')
         + lesson(10, 21, 1, T + '13:00+00', T + '14:00+00') + 'ROLLBACK;')
    check('touching intervals are accepted for teacher, student and room', True)
    psql(url, 'BEGIN;\n'
         "UPDATE service_booking_resource_allocation SET allocation_status = 'released' WHERE booking_id = 300;\n"
         + lesson(11, 20, 1, T + '16:00+00', T + '17:00+00')
         + "UPDATE service_booking_resource_allocation SET allocation_status = 'reserved' WHERE booking_id = 300;\n"
         + 'COMMIT;', expect_failure=overlap)
    check('a released allocation frees the room and cannot be reactivated over a lesson', True)
    psql(url, 'BEGIN;\nDELETE FROM class_session WHERE id = 100;\n'
         + lesson(10, 21, 1, T + '14:15+00', T + '14:45+00') + 'ROLLBACK;')
    check('deleting a lesson frees teacher, student and room', True)
    psql(url, 'BEGIN;\n' + lesson(11, 20, 2, T + '20:00+00', T + '21:00+00')
         + f"UPDATE class_session SET start_at = '{T}14:30+00', end_at = '{T}15:30+00', room_id = 1 "
           f"WHERE start_at = '{T}20:00+00';\nCOMMIT;", expect_failure=overlap)
    check('moving a lesson onto an occupied room is rejected', True)
    psql(url, 'BEGIN;\nUPDATE class_session SET attended = true WHERE id = 100;\n'
              "UPDATE class_session SET notes = 'x' WHERE start_at < '2021-01-01';\nROLLBACK;")
    check('edits that do not move a lesson never touch the calendar', True)

    # --- the snapshot query ---------------------------------------------------
    lo, hi = "'2030-12-01 00:00+00'", "'2030-12-04 00:00+00'"

    def snapshot(teacher, student, subject, room, read_only=True):
        bound = query
        for value in (teacher, student, subject, room, lo, hi):
            bound = bound.replace('?', str(value), 1)
        head = 'BEGIN TRANSACTION READ ONLY;\n' if read_only else 'BEGIN;\n'
        return json.loads(psql(url, head + bound + ';\nROLLBACK;'))

    us = lambda stamp: micros(url, stamp)
    doc = snapshot(10, 20, 1, 'NULL')
    check('the snapshot query runs in a read-only transaction', True)
    check('rooms are the active Room resources allowed for the subject, with priority',
          doc['rooms'] == [{'id': 1, 'name': 'Sala A', 'priority': 2}, {'id': 2, 'name': 'Sala B', 'priority': 1}], doc['rooms'])
    check('teacher busy is the teacher occupancy',
          doc['teacherBusy'] == [[us(T + '14:00+00'), us(T + '15:00+00')]], doc['teacherBusy'])
    check('student 20 has no commitments', doc['studentBusy'] == [])
    check('resource busy holds lessons, trials, active allocations and legacy bookings, not cancelled ones',
          sorted(doc['resourceBusy']) == sorted([
              [1, us(T + '14:00+00'), us(T + '15:00+00')], [2, us(T + '15:00+00'), us(T + '16:00+00')],
              [1, us(T + '16:00+00'), us(T + '17:00+00')], [1, us(T + '16:00+00'), us(T + '17:00+00')],
              [1, us(T + '17:30+00'), us(T + '18:00+00')]]), doc['resourceBusy'])
    check('availability is the teacher windows of the subject',
          sorted(doc['availability']) == [[1, us(T + '13:00+00'), us(T + '18:00+00')],
                                          [2, us(T + '13:00+00'), us(T + '16:00+00')]], doc['availability'])
    check('package validity is the latest expiry among open packages of the subject',
          doc['packageValidUntil'] == us('2030-12-20 00:00+00') and doc['teacherTeachesSubject'] is True, doc)
    other = snapshot(11, 21, 1, 2)
    check('a requested room restricts the rooms; no package and no subject link are reported',
          other['rooms'] == [{'id': 2, 'name': 'Sala B', 'priority': 1}] and other['packageValidUntil'] is None
          and other['teacherTeachesSubject'] is False and other['studentBusy'] == [[us(T + '14:00+00'), us(T + '15:00+00')]], other)
    check('a subject without room preferences may use every active room',
          [room['id'] for room in snapshot(10, 20, 2, 'NULL')['rooms']] == [1, 2])
    check('an inactive or non-room resource is never offered',
          snapshot(10, 20, 1, 3)['rooms'] == [] and snapshot(10, 20, 2, 4)['rooms'] == [])

    # --- Haskell predicate against the exclusion constraint -------------------
    if oracle is not None:
        correspondence(url, oracle, doc, us)

    # --- rollback --------------------------------------------------------------
    psql(url, ROLLBACK.read_text())
    check('rollback removes the projection and leaves the lessons',
          psql(url, "SELECT to_regclass('school_schedule_occupancy') IS NULL, count(*) FROM class_session;").strip() == 't|3')
    psql(url, lesson(10, 20, 2, T + '14:30+00', T + '15:30+00'))
    check('negative control: without the migration the same double booking is accepted', True)


def correspondence(url, oracle, doc, us):
    """Ask the compiled core which candidates are feasible; insert every candidate."""
    instance = {'now': us('2030-12-01 20:00+00'), 'from': None, 'days': 1, 'durationMinutes': 30,
                'limit': 20, 'preferred': [], 'snapshot': doc}
    out = subprocess.run([oracle], input=json.dumps(instance) + '\n', text=True, capture_output=True, check=True)
    answer = json.loads(out.stdout)
    if 'error' in answer:
        raise SystemExit('oracle refused the instance: ' + answer['error'])
    feasible = {(item['start'], item['roomId']) for item in answer['feasible']}
    candidates = [tuple(pair) for pair in answer['candidates']]
    check('the oracle enumerates candidates and finds some feasible and some not',
          0 < len(feasible) < len(candidates), f'{len(feasible)} of {len(candidates)}')
    script = "CREATE TEMP TABLE probe_result (start_us bigint, room bigint, state text);\n"
    for start, room in candidates:
        script += ("DO $$ BEGIN\n"
                   "  INSERT INTO class_session (student_id, teacher_id, subject_id, start_at, end_at, room_id)\n"
                   f"  VALUES (20, 10, 1, to_timestamp({start} / 1000000.0), to_timestamp({start} / 1000000.0) + interval '30 minutes', {room});\n"
                   "  RAISE EXCEPTION USING ERRCODE = 'P0999';\n"
                   "EXCEPTION WHEN SQLSTATE 'P0999' THEN\n"
                   f"  INSERT INTO probe_result VALUES ({start}, {room}, 'accepted');\n"
                   "WHEN exclusion_violation THEN\n"
                   f"  INSERT INTO probe_result VALUES ({start}, {room}, 'excluded');\n"
                   "END $$;\n")
    script += 'SELECT start_us, room, state FROM probe_result ORDER BY 1, 2;\n'
    rows = [line.split('|') for line in psql(url, 'BEGIN;\n' + script + 'ROLLBACK;').split()]
    accepted = {(int(start), int(room)) for start, room, state in rows if state == 'accepted'}
    excluded = {(int(start), int(room)) for start, room, state in rows if state == 'excluded'}
    check('every candidate was probed', len(rows) == len(candidates) and accepted | excluded == set(candidates))
    check('soundness: every feasible candidate is accepted by the exclusion constraint',
          feasible <= accepted, str(sorted(feasible - accepted)[:5]))
    check('negative control: the constraint does reject candidates, none of them feasible',
          len(excluded) > 0 and not (excluded & feasible))
    # What the constraint accepts but the core refuses must be refused for a
    # reason the database does not enforce (availability window or legacy booking).
    busy = [(start, end) for resource, start, end in doc['resourceBusy'] if resource == 1]
    windows = {room: [(start, end) for r, start, end in doc['availability'] if r == room] for room in (1, 2)}
    half_hour = 30 * 60 * 1000000
    unexplained = [
        (start, room) for start, room in accepted - feasible
        if any(lo <= start and start + half_hour <= hi for lo, hi in windows[room])
        and not (room == 1 and any(start < end and begin < start + half_hour for begin, end in busy))
    ]
    check('everything the constraint accepts and the core refuses lies outside availability or on a legacy booking',
          unexplained == [], str(unexplained[:5]))
    check('the top suggestion can be inserted',
          (answer['suggestions'][0]['start'], answer['suggestions'][0]['roomId']) in accepted)


if __name__ == '__main__':
    main()
