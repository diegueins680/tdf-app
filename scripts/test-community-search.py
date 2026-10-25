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
    start = name + (' sponsoredOnly' if name == 'searchSql' else '') + ' ='
    block = source.split(start, 1)[1].split('\n\n', 1)[0]
    block = block.replace('(if sponsoredOnly then "TRUE " else "FALSE ")', '"FALSE "')
    tokens = re.findall(r'"(?:[^"\\]|\\.)*"|\b(?:searchDocumentsSql|directoryVisibleEventDocumentClause|directoryDerivedListingDedupClause|directorySourceProfileSql|directoryFilterClause|directoryEntityTypeClause)\b', block)
    return ''.join(json.loads(token) if token.startswith('"') else expression(token) for token in tokens)

sql = expression('searchSql')

fixture = """
BEGIN;
INSERT INTO party(id,display_name,is_org,created_at) VALUES
(990001,'Lúna',false,now()),(990002,'Luna Extended',false,now()),(990003,'Hidden Luna',false,now()),(990004,'Fixture Banda',true,now()),(990005,'Fixture Proyecto',true,now()),(990006,'Ana',false,now()),(990007,'Linked Hidden Luna',false,now());
INSERT INTO artist_profile(artist_party_id,slug,bio,city,created_at) VALUES
(990001,'luna-fixture','Ambient music project','Quito',now()),
(990002,'luna-extended-fixture','Music','Quito',now()),
(990003,'hidden-luna-fixture','Private directory identity','Quito',now()),
(990007,'linked-hidden-luna-fixture','Private legacy link identity','Quito',now());
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status)
VALUES ('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',990003,'artist','Hidden Luna','private-luna-fixture','draft','private','allowed'),
('bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',990002,'artist','Luna Extended','canonical-luna-fixture','published','public','allowed');
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status) VALUES
('cccccccc-cccc-4ccc-8ccc-cccccccccccc',990004,'band','Fixture Banda','fixture-banda','published','public','allowed'),
('dddddddd-dddd-4ddd-8ddd-dddddddddddd',990005,'project','Fixture Proyecto','fixture-proyecto','published','public','allowed');
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,profile_status,visibility,moderation_status) VALUES ('eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee',990006,'artist','Ana','ana-fixture','productor','published','public','allowed');
INSERT INTO directory_legacy_link(profile_id,legacy_kind,legacy_id,source_table)
SELECT 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa','artist_profile',id::text,'artist_profile'
FROM artist_profile WHERE artist_party_id=990007;
SELECT directory_refresh_profile_search(id) FROM directory_profile WHERE subject_party_id BETWEEN 990002 AND 990006;
"""

def query(q='', entity=None, kind=None, cursor=None, limit=20, facets=False, changes=''):
    params = [q, entity, None, None, None, None, None, None, None, None, None, None, None, None, kind, 'suppressed', cursor, limit]
    if facets: params = params[:-2]
    chunks = (expression('facetsSql') if facets else sql).split('?')
    assert len(chunks) == len(params) + 1
    def quote(value):
        if value is None: return 'NULL'
        if isinstance(value, int): return str(value)
        return "'" + value.replace("'", "''") + "'"
    rendered = chunks[0] + ''.join(quote(value) + chunk for value, chunk in zip(params, chunks[1:]))
    result = subprocess.run(['psql', url, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], input=fixture + changes + rendered + ';\nROLLBACK;', text=True, capture_output=True)
    if result.returncode:
        raise AssertionError(result.stderr)
    return [json.loads(line) for line in result.stdout.splitlines() if line.startswith('{')]

for value in ['luna', 'LÚNA', '  lúNa  ', 'lun']:
    rows = query(value, 'artist')
    assert any(row['title'] == 'Lúna' for row in rows), rows
    if value != 'lun': assert rows[0]['title'] == 'Lúna', rows
    assert not any(row['id'] == 'artist-990003' for row in rows), rows
    assert not any(row['id'] == 'artist-990007' for row in rows), rows
    assert next(row for row in rows if row['title'] == 'Lúna')['canonicalPath'] == '/artista/990001'
print('PASS: case/accent/whitespace/partial matching; exact before prefix; canonical identity; private subject and legacy-link fallback denied')
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

facets = query('Luna', 'artist', facets=True)[0]
assert facets['total'] == len(query('Luna', 'artist')), facets
assert facets['entityTypes']['artist'] >= facets['total'], facets
print('PASS: shared artist facet totals and category counts')

assert any(row['title'] == 'Ana' for row in query('productro', 'artist')), 'Directory biography typo tolerance regressed'
print('PASS: existing non-title directory typo matching is preserved')

# Arbitrary newly created artists use persisted media, without a seeded slug map.
for image in ['https://cdn.example.test/first-artist.jpg', 'https://cdn.example.test/replacement.jpg', None]:
    value = 'NULL' if image is None else "'" + image + "'"
    changes = "UPDATE artist_profile SET hero_image_url='https://cdn.example.test/original.jpg' WHERE artist_party_id=990001;\n"
    changes += f"UPDATE artist_profile SET hero_image_url={value} WHERE artist_party_id=990001;\n"
    row = next(row for row in query('Luna', 'profile', changes=changes) if row['id'] == 'artist-990001')
    assert row['imageUrl'] == image, row
    assert row['canonicalPath'] == '/artista/990001', row

# A linked public directory identity follows canonical media updates too.
link = """
INSERT INTO directory_legacy_link(profile_id,legacy_kind,legacy_id,source_table)
SELECT 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb','artist_profile',id::text,'artist_profile'
FROM artist_profile WHERE artist_party_id=990002;
"""
for image in ['https://cdn.example.test/linked-artist.jpg', None]:
    value = 'NULL' if image is None else "'" + image + "'"
    changes = link + f"UPDATE artist_profile SET hero_image_url={value} WHERE artist_party_id IN (990002,990003,990007);\n"
    rows = query('Luna', 'profile', changes=changes)
    row = next(row for row in rows if row['title'] == 'Luna Extended')
    assert row['imageUrl'] == image, row
    assert not any(row['id'] in ('artist-990003', 'artist-990007') for row in rows), rows
print('PASS: arbitrary artist images, replacement/removal, linked preview refresh and private-image suppression')
