#!/usr/bin/env python3
"""Execute the shipped SQL projection against a disposable migrated PostgreSQL DB.
No production data, credentials, or HTTP fixtures are used. Fixtures roll back.
"""
import json
import os
from pathlib import Path
import re
import subprocess
from urllib.parse import urlparse

root = Path(__file__).resolve().parents[1]
url = os.environ['TDF_DISCOVERY_TEST_DATABASE_URL']
parsed = urlparse(url)
assert (parsed.hostname in ('127.0.0.1', 'localhost') or (os.environ.get('CI') == 'true' and parsed.hostname == 'postgres')) and parsed.path.endswith('_test'), 'Disposable loopback _test database required'
source = (root / 'tdf-hq/src/TDF/Server/Directory.hs').read_text()

def expression(name):
    start = name + (' sponsoredOnly' if name == 'searchSql' else '') + ' =\n'
    block = source.split(start, 1)[1].split('\n\n', 1)[0]
    block = block.replace('(if sponsoredOnly then "TRUE " else "FALSE ")', '"FALSE "')
    tokens = re.findall(r'"(?:[^"\\]|\\.)*"|\b(?:searchDocumentsSql|directoryVisibleEventDocumentClause|directoryDerivedListingDedupClause|directorySourceProfileSql)\b', block)
    return ''.join(json.loads(token) if token.startswith('"') else expression(token) for token in tokens)

sql = expression('searchSql')

fixture = """
BEGIN;
INSERT INTO party(id,display_name,is_org,created_at) VALUES
(990001,'Lúna',false,now()),(990002,'Luna Extended',false,now()),(990003,'Hidden Luna',false,now()),(990004,'Fixture Banda',true,now()),(990005,'Fixture Proyecto',true,now());
INSERT INTO artist_profile(artist_party_id,slug,bio,city,created_at) VALUES
(990001,'luna-fixture','Ambient music project','Quito',now()),
(990002,'luna-extended-fixture','Music','Quito',now()),
(990003,'hidden-luna-fixture','Private directory identity','Quito',now());
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status)
VALUES ('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',990003,'artist','Hidden Luna','private-luna-fixture','draft','private','allowed'),
('bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',990002,'artist','Luna Extended','canonical-luna-fixture','published','public','allowed');
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status) VALUES
('cccccccc-cccc-4ccc-8ccc-cccccccccccc',990004,'band','Fixture Banda','fixture-banda','published','public','allowed'),
('dddddddd-dddd-4ddd-8ddd-dddddddddddd',990005,'project','Fixture Proyecto','fixture-proyecto','published','public','allowed');
SELECT directory_refresh_profile_search(id) FROM directory_profile WHERE subject_party_id BETWEEN 990002 AND 990005;
"""

def query(q='', entity=None, kind=None, cursor=None, limit=20):
    params = [q, entity, None, None, None, None, None, None, None, None, None, None, None, None, kind, 'suppressed', cursor, limit]
    chunks = sql.split('?')
    assert len(chunks) == len(params) + 1
    def quote(value):
        if value is None: return 'NULL'
        if isinstance(value, int): return str(value)
        return "'" + value.replace("'", "''") + "'"
    rendered = chunks[0] + ''.join(quote(value) + chunk for value, chunk in zip(params, chunks[1:]))
    result = subprocess.run(['psql', url, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], input=fixture + rendered + ';\nROLLBACK;', text=True, capture_output=True)
    if result.returncode:
        raise AssertionError(result.stderr)
    return [json.loads(line) for line in result.stdout.splitlines() if line.startswith('{')]

for value in ['luna', 'LÚNA', '  lúNa  ', 'lun']:
    rows = query(value, 'artist')
    assert any(row['title'] == 'Lúna' for row in rows), rows
    if value != 'lun': assert rows[0]['title'] == 'Lúna', rows
    assert not any(row['id'] == 'artist-990003' for row in rows), rows
    assert next(row for row in rows if row['title'] == 'Lúna')['canonicalPath'] == '/artista/990001'
print('PASS: case/accent/whitespace/partial matching; exact before prefix; canonical identity; private fallback denied')
assert not query("'; DROP TABLE party; --", 'artist')
assert not query('zzzxqqv999', 'artist')
assert not query('Luna', 'event')
assert not query('Luna', 'profile', 'band')
assert query('Luna', 'profile', 'artist')
first = query('Luna', 'artist', limit=1)
second = query('Luna', 'artist', cursor=first[0]['cursor'], limit=1)
assert len(first) == len(second) == 1 and first[0]['id'] != second[0]['id']
print('PASS: parameter text, empty result, entity and profile-kind filters, bounded cursor pagination')

assert len([row for row in query('Luna') if row['title'] == 'Luna Extended']) == 1
assert query('Fixture Banda', 'artist', 'band')[0]['profileKind'] == 'band'
assert query('Fixture Proyecto', 'artist', 'project')[0]['profileKind'] == 'project'
print('PASS: canonical deduplication, published bands and musical projects')
