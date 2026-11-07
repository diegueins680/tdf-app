#!/usr/bin/env python3
"""ART-PUBLIC-VISIBILITY-001 / PAY-TIP-001: run the artist visibility SQL
from tdf-hq/src/TDF/Profiles/ArtistVisibility.hs against PostgreSQL.

The query text is extracted from the Haskell source, never copied, so the test
cannot drift from the implementation. The fixture reproduces only the columns
the query reads, with their migrated types (tdf-hq/sql/2026-08-14_music_directory_core.sql).
It runs in a fresh schema inside one transaction that is always rolled back.

Usage: test-artist-visibility-sql.py DATABASE_URL   (a disposable database)
"""
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tdf-hq/src/TDF/Profiles/ArtistVisibility.hs'


def extract_query():
    src = SOURCE.read_text()
    body = src[src.index('visibilitySql whereClause ='):src.index('-- | Visibility of one artist profile row')]
    head = body[body.index('"'):body.index('<> whereClause')]
    head = re.sub(r'\\\s*\n\s*\\', '', head).strip()
    if not (head.startswith('"') and head.endswith('"')):
        raise SystemExit('Unexpected visibilitySql shape; update the extractor with the source')
    tail_line = body[body.index('<> whereClause <>') + len('<> whereClause <>'):].strip().split('\n')[0].strip()
    if not (tail_line.startswith('"') and tail_line.endswith('"')):
        raise SystemExit('Unexpected visibilitySql tail; update the extractor with the source')
    return head[1:-1], tail_line[1:-1]


FIXTURE = """
CREATE SCHEMA artist_visibility_test;
SET LOCAL search_path = artist_visibility_test;
CREATE TABLE party (id BIGSERIAL PRIMARY KEY);
CREATE TABLE artist_profile (id BIGSERIAL PRIMARY KEY, artist_party_id BIGINT NOT NULL UNIQUE REFERENCES party(id));
CREATE TABLE directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_status TEXT NOT NULL, visibility TEXT NOT NULL, moderation_status TEXT NOT NULL,
  canonical_profile_id UUID REFERENCES directory_profile(id));
CREATE TABLE directory_legacy_link (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  legacy_kind TEXT NOT NULL, legacy_id TEXT NOT NULL);
INSERT INTO party (id) SELECT g FROM generate_series(1, 20) g;
-- Profile ids differ from party ids on purpose.
INSERT INTO artist_profile (id, artist_party_id) VALUES (101,1),(102,2),(103,3),(104,4),(105,5),(106,6),(107,7),(108,8);
INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (2,'published','public','allowed');
INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (3,'draft','public','allowed');
WITH p AS (INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (14,'published','public','allowed') RETURNING id)
INSERT INTO directory_legacy_link (profile_id,legacy_kind,legacy_id) SELECT id,'artist_profile','104' FROM p;
INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (5,'published','private','allowed');
WITH p AS (INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (15,'published','public','allowed') RETURNING id)
INSERT INTO directory_legacy_link (profile_id,legacy_kind,legacy_id) SELECT id,'artist_profile','105' FROM p;
WITH c AS (INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (16,'published','public','allowed') RETURNING id)
INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status,canonical_profile_id) SELECT 6,'published','public','allowed',id FROM c;
WITH p AS (INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (7,'suspended','public','allowed') RETURNING id)
INSERT INTO directory_legacy_link (profile_id,legacy_kind,legacy_id) SELECT id,'artist_profile','107' FROM p;
WITH p AS (INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (18,'draft','private','allowed') RETURNING id)
INSERT INTO directory_legacy_link (profile_id,legacy_kind,legacy_id) SELECT id,'band','108' FROM p;
INSERT INTO directory_profile (subject_party_id,profile_status,visibility,moderation_status) VALUES (9,'published','public','blocked');
"""

# party -> (all identities, public identities); listed iff total == 0 or public > 0
EXPECTED = {
    1: (0, 0),  # no identity: legacy profile listed
    2: (1, 1),  # public identity by subject
    3: (1, 0),  # draft identity hides the legacy profile
    4: (1, 1),  # public identity by legacy link
    5: (2, 1),  # private by subject, public by link
    6: (1, 0),  # merged identity is not public
    7: (1, 0),  # suspended identity reached by subject and link counts once
    8: (0, 0),  # a link of another legacy kind is ignored
}


def main():
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    query, tail = extract_query()
    all_rows = f"{query} {tail} ORDER BY 1"
    single = f"{query} WHERE artist.artist_party_id = 7 AND artist.id = 107 {tail}"
    script = "\\set ON_ERROR_STOP 1\nBEGIN;\n" + FIXTURE + f"\\echo ALL\n{all_rows};\n\\echo SINGLE\n{single};\nROLLBACK;\n"
    out = subprocess.run(['psql', sys.argv[1], '-X', '-q', '-A', '-t', '-F', ','],
                         input=script, text=True, capture_output=True, check=True).stdout
    section, observed, single_rows = None, {}, []
    for line in out.splitlines():
        if line in ('ALL', 'SINGLE'):
            section = line
            continue
        if not line.strip():
            continue
        party, total, public = (int(v) for v in line.split(','))
        if section == 'ALL':
            observed[party] = (total, public)
        else:
            single_rows.append((party, total, public))
    if observed != EXPECTED:
        raise SystemExit(f'Visibility counts differ: expected {EXPECTED}, observed {observed}')
    if single_rows != [(7, 1, 0)]:
        raise SystemExit(f'Single-artist query differs: {single_rows}')
    listed = sorted(p for p, (t, pub) in observed.items() if t == 0 or pub > 0)
    if listed != [1, 2, 4, 5, 8]:
        raise SystemExit(f'Listed parties differ: {listed}')
    print('Artist visibility SQL checked: 8 scenarios, listed parties', listed)


if __name__ == '__main__':
    main()
