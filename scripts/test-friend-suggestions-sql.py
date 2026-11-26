#!/usr/bin/env python3
"""COMMUNITY-FRIEND-SUGGESTION-001: run the friend-suggestion SQL from
tdf-hq/src/TDF/Social/SuggestionStore.hs and
tdf-hq/src/TDF/Auth/EmailVerification.hs and the migration
tdf-hq/sql/2026-11-21_social_friend_suggestions.sql against PostgreSQL.

The statements are extracted from the Haskell source, never copied, so the test
cannot drift from the implementation. The fixture reproduces only the columns
the statements read; social_v2_profile_eligible is a stand-in with the migrated
signature. Everything runs in a fresh schema inside one transaction that is
always rolled back, except for the two-session check in its own disposable schema.

Usage: test-friend-suggestions-sql.py DATABASE_URL   (a disposable database)
"""
import re
import select
import subprocess
import sys
from pathlib import Path
from uuid import uuid4

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tdf-hq/src/TDF/Social/SuggestionStore.hs'
VERIFICATION_SOURCE = ROOT / 'tdf-hq/src/TDF/Auth/EmailVerification.hs'
MIGRATION = ROOT / 'tdf-hq/sql/2026-11-21_social_friend_suggestions.sql'
ROLLBACK = ROOT / 'tdf-hq/sql/2026-11-21_social_friend_suggestions_rollback.sql'


def extract(name, parameters, source=None, depth=0):
    """The text bound to `name`: string literals and other bindings joined by <>."""
    source = source or SOURCE.read_text()
    match = re.search(rf'^{name} =(?: |\n)((?:  .*\n)+|.*\n)', source, re.MULTILINE)
    if not match or depth > 3:
        raise SystemExit(f'{name} not found; update the extractor with the source')
    body = re.sub(r'\\\s*\n\s*\\', '', match.group(1))
    parts, position = [], 0
    for token in re.finditer(r'\s*(?:"((?:[^"\\]|\\.)*)"|([a-z]\w*)|(<>))', body):
        if token.start() != position:
            break
        position = token.end()
        if token.group(1) is not None:
            parts.append(token.group(1))
        elif token.group(2):
            parts.append(extract(token.group(2), None, source, depth + 1))
    if body[position:].strip() or not parts or any('\\' in part for part in parts):
        raise SystemExit(f'Unexpected {name} shape; update the extractor with the source')
    query = ''.join(parts)
    if parameters is not None and query.count('?') != parameters:
        raise SystemExit(f'{name} takes {query.count("?")} parameters, expected {parameters}')
    return query


def bind(query, *values):
    for value in values:
        query = query.replace('?', str(value), 1)
    return query


def transactionless(path):
    """A migration file without its own BEGIN/COMMIT, to run inside the test transaction."""
    lines = [line for line in path.read_text().splitlines() if line.strip() not in ('BEGIN;', 'COMMIT;')]
    return '\n'.join(lines) + '\n'


FIXTURE = """
CREATE SCHEMA friend_suggestions_test;
SET LOCAL search_path = friend_suggestions_test;
CREATE TABLE party (id BIGINT PRIMARY KEY, is_org BOOLEAN NOT NULL DEFAULT false,
  created_at TIMESTAMPTZ NOT NULL);
CREATE TABLE user_credential (id BIGINT PRIMARY KEY, party_id BIGINT NOT NULL REFERENCES party(id),
  active BOOLEAN NOT NULL);
CREATE TABLE auth_provider_identity (issuer TEXT NOT NULL, subject TEXT NOT NULL,
  credential_id BIGINT NOT NULL REFERENCES user_credential(id), PRIMARY KEY (issuer, subject));
CREATE TABLE social_v2_preference (party_id BIGINT PRIMARY KEY REFERENCES party(id),
  personalized BOOLEAN NOT NULL DEFAULT true, closed BOOLEAN NOT NULL DEFAULT false);
CREATE TABLE social_v2_pair (party_a BIGINT NOT NULL, party_b BIGINT NOT NULL,
  block_a BOOLEAN NOT NULL DEFAULT false, block_b BOOLEAN NOT NULL DEFAULT false,
  follow_a BOOLEAN NOT NULL DEFAULT false, follow_b BOOLEAN NOT NULL DEFAULT false,
  PRIMARY KEY (party_a, party_b), CHECK (party_a < party_b));
CREATE TABLE party_follow (follower_party_id BIGINT NOT NULL, following_party_id BIGINT NOT NULL,
  UNIQUE (follower_party_id, following_party_id));
CREATE TABLE refused (actor BIGINT NOT NULL, target BIGINT NOT NULL);
-- Stand-in for tdf-hq/sql/2026-09-15_social_v2_profile_reads.sql: same signature and
-- result columns; refuses the pairs listed in the table above.
CREATE FUNCTION social_v2_profile_eligible(actor bigint, requested bigint[])
RETURNS TABLE(profile_id bigint, ordinal bigint) LANGUAGE sql STABLE AS $$
  SELECT r.id, r.ordinal FROM unnest(requested) WITH ORDINALITY AS r(id, ordinal)
  WHERE NOT EXISTS (SELECT 1 FROM refused x WHERE x.actor = $1 AND x.target = r.id) $$;
-- 1 viewer, 2 friend, 3 and 11 candidates, 9 blocks the viewer, 10 opted out: all old,
-- verified, live people. 4 is two days old. 5 has no provider identity. 6 is an
-- organisation. 7 closed its profile. 8 has an inactive credential. 12 has no credential.
-- 13 has no provider identity either; it becomes verified by a completed password reset.
INSERT INTO party (id, is_org, created_at) VALUES
  (1,false,'2026-01-01'),(2,false,'2026-01-01'),(3,false,'2026-01-01'),(4,false,'2026-11-19'),
  (5,false,'2026-01-01'),(6,true,'2026-01-01'),(7,false,'2026-01-01'),(8,false,'2026-01-01'),
  (9,false,'2026-01-01'),(10,false,'2026-01-01'),(11,false,'2026-11-14'),(12,false,'2026-01-01'),
  (13,false,'2026-01-01');
INSERT INTO user_credential (id, party_id, active) VALUES
  (1,1,true),(2,2,true),(3,3,true),(4,4,true),(5,5,true),(6,6,true),(7,7,true),(8,8,false),
  (9,9,true),(10,10,true),(11,11,true),(13,13,true);
INSERT INTO auth_provider_identity (issuer, subject, credential_id) VALUES
  ('g','1',1),('g','2',2),('g','3',3),('g','4',4),('g','6',6),('g','7',7),('g','8',8),
  ('g','9',9),('g','10',10),('g','11',11);
INSERT INTO social_v2_preference (party_id, personalized, closed) VALUES
  (7,true,true),(10,false,false);
INSERT INTO social_v2_pair (party_a, party_b, block_a, block_b) VALUES (1,9,false,true);
INSERT INTO social_v2_pair (party_a,party_b,follow_a,follow_b) VALUES
  (1,2,true,false),(2,3,true,true),(2,11,true,false),(2,4,true,false),(3,4,false,true),(3,9,false,true);
-- Reciprocal legacy rows and a legacy-only path must never contribute score.
INSERT INTO party_follow VALUES (1,2),(2,3),(2,11),(2,4),(4,3),(9,3),(2,2),(2,1),(1,10);
"""

RUN_START = "'2026-11-21 08:00:00+00'"
EXPECTED_ELIGIBLE = [1, 2, 3, 9, 10, 11]  # 11 is exactly seven days old at the run start
EXPECTED_ELIGIBLE_AFTER_RESET = EXPECTED_ELIGIBLE + [13]
SUPPORT = "'{1,2,3,11}'"
SCENARIOS = (
    # name, statements applied after the stored result, served (None means fallback)
    ('baseline', '', [3, 11]),
    ('legacy mirror cannot suppress a candidate', 'INSERT INTO party_follow VALUES (1,3),(3,1);', [3, 11]),
    ('viewer now follows a candidate', 'INSERT INTO social_v2_pair (party_a,party_b,follow_a) VALUES (1,3,true);', [11]),
    ('a candidate now follows the viewer', 'INSERT INTO social_v2_pair (party_a,party_b,follow_b) VALUES (1,11,true);', [3]),
    ('all candidates refused', 'INSERT INTO refused VALUES (1,3),(1,11);', []),
    ('policy refuses a candidate', 'INSERT INTO refused VALUES (1,3);', [11]),
    ('policy refuses for another viewer only', 'INSERT INTO refused VALUES (2,3);', [3, 11]),
    ('evidence closed its profile',
     'INSERT INTO social_v2_preference (party_id, closed) VALUES (2, true);', None),
    ('evidence blocked the viewer',
     'INSERT INTO social_v2_pair VALUES (1,11,false,true);', None),
    ('viewer blocked evidence', 'UPDATE social_v2_pair SET block_a=true,follow_a=false WHERE party_a=1 AND party_b=2;', None),
    ('evidence was deactivated', 'UPDATE user_credential SET active = false WHERE party_id = 3;', None),
    ('evidence lost its verified identity',
     'DELETE FROM auth_provider_identity WHERE credential_id = 2;', None),
    ('evidence became an organisation', 'UPDATE party SET is_org = true WHERE id = 11;', None),
    ('viewer opted out',
     'INSERT INTO social_v2_preference (party_id, personalized) VALUES (1, false);', None),
    ('viewer was deactivated', 'UPDATE user_credential SET active = false WHERE party_id = 1;', None),
    ('run is older than the maximum age',
     "UPDATE social_suggestion_run SET started_at = clock_timestamp() - interval '50 hours', "
     "completed_at = clock_timestamp() - interval '49 hours';", None),
    ('run is just inside the maximum age',
     "UPDATE social_suggestion_run SET started_at = clock_timestamp() - interval '47 hours', "
     "completed_at = clock_timestamp() - interval '47 hours';", [3, 11]),
)


def check_replacement(url, read):
    """Hold a production query open while a second connection replaces its run."""
    schema = 'suggestion_overlap_' + uuid4().hex
    psql = ['psql', '-XqAt', '-v', 'ON_ERROR_STOP=1', url]
    path = f'SET search_path = {schema};\n'

    def execute(sql):
        return subprocess.run(psql, input=sql, text=True, stdout=subprocess.PIPE,
                              check=True, timeout=30).stdout

    def store(run, support, candidates):
        statements = [bind(extract('insertRunSql', 4), 'clock_timestamp()', 0.85, 100, 1),
                      bind(extract('deleteEarlierRunsSql', 1), run),
                      bind(extract('insertViewerSql', 3), 1, run, support)]
        for pos, candidate in enumerate(candidates, 1):
            statements.append(bind(extract('insertScoreSql', 4), 1, candidate, pos, 0.1))
        return ';\n'.join(statements) + ';\n'

    # A unique committed fixture makes both real connections see the same data.
    execute('BEGIN;\n' + FIXTURE.replace('friend_suggestions_test', schema)
            + transactionless(MIGRATION) + store(1, SUPPORT, [3, 11]) + 'COMMIT;')
    try:
        with subprocess.Popen(psql, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True) as reader:
            try:
                reader.stdin.write(path + 'BEGIN ISOLATION LEVEL READ COMMITTED;\n'
                                   + f'DECLARE suggestions CURSOR FOR {read};\nFETCH 1 FROM suggestions;\n')
                reader.stdin.flush()
                if not select.select([reader.stdout], [], [], 30)[0]:
                    raise TimeoutError('Timed out opening the suggestion cursor')
                if reader.stdout.readline().strip() != '3':
                    raise AssertionError('Expected the first candidate of the original run')

                # The replacement has a different candidate and includes a closed account in support.
                execute('BEGIN;\n' + path + store(2, "'{1,7,10}'", [10]) + 'COMMIT;')
                remaining, _ = reader.communicate(
                    f'FETCH ALL FROM suggestions;\nSELECT count(*) FROM ({read}) fresh;\nCOMMIT;\n',
                    timeout=30)
                if reader.returncode or remaining.splitlines() != ['11', '0']:
                    raise AssertionError(f'Expected the original tail, then fallback on the next read: {remaining!r}')
            finally:
                if reader.poll() is None:
                    reader.kill()
    finally:
        execute(f'SET client_min_messages = warning; DROP SCHEMA {schema} CASCADE;')


def main():
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    installed = extract('suggestionSchemaInstalledSql', 0)
    eligible = extract('eligibleAccountsSql', 2)
    opted_out = extract('optedOutAccountsSql', 0)
    blocks = extract('blockedPairsSql', 0)
    follows = extract('followRowsSql', 0)
    insert_run = extract('insertRunSql', 4)
    delete_earlier = extract('deleteEarlierRunsSql', 1)
    insert_viewer = extract('insertViewerSql', 3)
    insert_score = extract('insertScoreSql', 4)
    served = extract('servedSuggestionsSql', 3)
    probe = f"SELECT ({installed[len('SELECT '):]})::int;\n"
    verification = VERIFICATION_SOURCE.read_text()
    writable = extract('emailVerificationWritableSql', 0, verification)
    record = extract('recordVerifiedEmailSql', 1, verification)
    writable_probe = f"SELECT ({writable[len('SELECT '):]})::int;\n"

    script = "\\set ON_ERROR_STOP 1\nBEGIN;\n" + FIXTURE
    script += f"\\echo BEFORE\n{probe}\\echo UNWRITABLE\n{writable_probe}"
    script += transactionless(MIGRATION) + transactionless(MIGRATION)  # idempotent
    script += f"\\echo INSTALLED\n{probe}"
    script += f"\\echo ELIGIBLE\n{bind(eligible, RUN_START, 7)} ORDER BY 1;\n"
    script += f"\\echo OPTEDOUT\n{opted_out} ORDER BY 1;\n"
    # A completed password reset verifies party 13; repeating it and naming a party
    # that does not exist change nothing. Rolled back so the later scenarios are unaffected.
    script += f"SAVEPOINT reset;\n\\echo WRITABLE\n{writable_probe}"
    script += f"{bind(record, 13)};\n{bind(record, 13)};\n{bind(record, 999)};\n"
    script += "\\echo VERIFIED\nSELECT party_id FROM party_email_verification ORDER BY 1;\n"
    script += f"\\echo ELIGIBLERESET\n{bind(eligible, RUN_START, 7)} ORDER BY 1;\n"
    script += "ROLLBACK TO SAVEPOINT reset;\n"
    script += f"\\echo BLOCKS\n{blocks} ORDER BY 1, 2;\n"
    script += f"\\echo FOLLOWS\n{follows} ORDER BY 1, 2;\n"
    # What a run stores for viewer 1: support {1,2,3,11}, candidates 3 then 11.
    script += f"\\echo RUN\n{bind(insert_run, 'clock_timestamp()', 0.85, 100, 1)};\n"
    script += bind(delete_earlier, 1) + ";\n"
    script += bind(insert_viewer, 1, 1, SUPPORT) + ";\n"
    script += bind(insert_score, 1, 3, 1, 0.3) + ";\n" + bind(insert_score, 1, 11, 2, 0.2) + ";\n"
    for index, (_, statements, _) in enumerate(SCENARIOS):
        script += f"SAVEPOINT scenario;\n{statements}\n"
        script += f"\\echo SERVED {index}\n{bind(served, 1, 48, 20)};\n"
        script += "ROLLBACK TO SAVEPOINT scenario;\n"
    script += f"\\echo NOROW\n{bind(served, 2, 48, 20)};\n"
    script += f"\\echo LIMIT\n{bind(served, 1, 48, 1)};\n"
    # A later run replaces everything stored by the earlier one.
    script += f"\\echo RUN2\n{bind(insert_run, 'clock_timestamp()', 0.85, 100, 0)};\n"
    script += bind(delete_earlier, 2) + ";\n"
    script += "\\echo LEFT\nSELECT (SELECT count(*) FROM social_suggestion_run), " \
              "(SELECT count(*) FROM social_suggestion_viewer), (SELECT count(*) FROM social_suggestion_score);\n"
    # Deleting a party removes what was stored about it.
    script += transactionless(ROLLBACK)
    script += f"\\echo ROLLEDBACK\n{probe}ROLLBACK;\n"
    completed = subprocess.run(['psql', sys.argv[1], '-X', '-q', '-A', '-t', '-F', ',', '-P', 'null=NULL'],
                               input=script, text=True, capture_output=True)
    if completed.returncode != 0:
        raise SystemExit(f'psql failed:\n{completed.stderr}')
    observed, section = {}, None
    for line in completed.stdout.splitlines():
        if re.fullmatch(r'[A-Z0-9]+(?: \d+)?', line) and not line.isdigit() and line != 'NULL':
            section = line
            observed[section] = []
        elif line.strip():
            observed[section].append(tuple(None if value == 'NULL' else int(value) for value in line.split(',')))
    expected = {
        'BEFORE': [(0,)], 'INSTALLED': [(1,)], 'ROLLEDBACK': [(0,)],
        'ELIGIBLE': [(party,) for party in EXPECTED_ELIGIBLE],
        'OPTEDOUT': [(10,)], 'BLOCKS': [(1, 9)], 'UNWRITABLE': [(0,)], 'WRITABLE': [(1,)],
        'VERIFIED': [(13,)],
        'ELIGIBLERESET': [(party,) for party in sorted(EXPECTED_ELIGIBLE_AFTER_RESET)],
        'FOLLOWS': [(1, 2), (2, 3), (2, 4), (2, 11), (3, 2), (4, 3), (9, 3)],
        'RUN': [(1,)], 'RUN2': [(2,)], 'NOROW': [], 'LIMIT': [(3,)], 'LEFT': [(1, 0, 0)],
    }
    for index, (_, _, candidates) in enumerate(SCENARIOS):
        # No rows signal fallback. A single SQL NULL represents a valid empty list.
        expected[f'SERVED {index}'] = [] if candidates is None else [(c,) for c in candidates or [None]]
    if observed != expected:
        differing = {key: (expected.get(key), observed.get(key))
                     for key in sorted(set(expected) | set(observed)) if expected.get(key) != observed.get(key)}
        names = {f'SERVED {index}': name for index, (name, _, _) in enumerate(SCENARIOS)}
        raise SystemExit('Friend suggestion SQL differs (expected, observed): '
                         + '; '.join(f'{key} [{names.get(key, "")}]: {value}' for key, value in differing.items()))
    check_replacement(sys.argv[1], bind(served, 1, 48, 20))
    print(f'Friend suggestion SQL checked: migration applied twice and rolled back, '
          f'{len(EXPECTED_ELIGIBLE)} of 13 accounts eligible (7 after a password reset), {len(SCENARIOS)} read scenarios and concurrent run replacement')


if __name__ == '__main__':
    main()
