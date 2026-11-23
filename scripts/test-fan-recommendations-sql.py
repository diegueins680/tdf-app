#!/usr/bin/env python3
"""COMMUNITY-FAN-RECOMMENDATION-001: run the recommendation SQL from
tdf-hq/src/TDF/Fans/ArtistRecommendations.hs against PostgreSQL.

The query text is extracted from the Haskell source, never copied, so the test
cannot drift from the implementation. The fixture reproduces only the columns
the queries read (fan_follow, tdf-hq/sql/init_schema.sql). It runs in a fresh
schema inside one transaction that is always rolled back.

Usage: test-fan-recommendations-sql.py DATABASE_URL   (a disposable database)
"""
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tdf-hq/src/TDF/Fans/ArtistRecommendations.hs'


def extract(name, parameters):
    """The string literal bound to `name`, checked to take `parameters` placeholders."""
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


FIXTURE = """
CREATE SCHEMA fan_recommendations_test;
SET LOCAL search_path = fan_recommendations_test;
CREATE TABLE fan_follow (
  id BIGSERIAL PRIMARY KEY, fan_party_id BIGINT NOT NULL, artist_party_id BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (fan_party_id, artist_party_id));
-- Fan 100 follows artists 1 and 7. Fans 101 and 102 follow 1 and 2; fans 103 and
-- 104 follow 1 and 3; fan 101 also follows 7; fan 105 follows only 4.
INSERT INTO fan_follow (fan_party_id, artist_party_id) VALUES
  (100,1),(100,7),(101,1),(101,2),(101,7),(102,1),(102,2),(103,1),(103,3),(104,1),(104,3),(105,4);
-- Stand-in for the follow policy of tdf-hq/sql/2026-09-15_social_v2_profile_reads.sql with the
-- same signature and result columns: here artist 3 has blocked fan 100, nobody else is refused.
CREATE FUNCTION social_v2_profile_eligible(actor bigint, requested bigint[])
RETURNS TABLE(profile_id bigint, ordinal bigint) LANGUAGE sql STABLE AS $$
  SELECT r.id, r.ordinal FROM unnest(requested) WITH ORDINALITY AS r(id, ordinal)
  WHERE NOT (actor = 100 AND r.id = 3) $$;
"""

FAN, LONE_FAN, STRANGER = 100, 105, 999
EXPECTED_FOLLOWED = {FAN: [(1,), (7,)], LONE_FAN: [(4,)], STRANGER: []}
# (followed artist, candidate, fans following both)
EXPECTED_CO_FOLLOWS = {FAN: [(1, 2, 2), (1, 3, 2), (7, 2, 1)], LONE_FAN: [], STRANGER: []}
EXPECTED_FOLLOWERS = [(1, 5), (2, 2), (3, 2), (4, 1), (7, 2)]
# The array literal the Haskell loader builds for candidates 2, 3 and 4.
CANDIDATES = "'{2,3,4}'"
EXPECTED_FOLLOWABLE = {FAN: [(2,), (4,)], LONE_FAN: [(2,), (3,), (4,)]}


def main():
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    followed = extract('followedArtistsSql', 1)
    co_follows = extract('coFollowSql', 3)
    followers = extract('followerCountsSql', 0)
    policy = extract('followPolicyInstalledSql', 0)
    followable = extract('followableArtistsSql', 2)
    script = "\\set ON_ERROR_STOP 1\nBEGIN;\n" + FIXTURE
    for fan in (FAN, LONE_FAN, STRANGER):
        script += f"\\echo FOLLOWED {fan}\n{followed.replace('?', str(fan))} ORDER BY 1;\n"
        script += f"\\echo CO {fan}\n{co_follows.replace('?', str(fan))} ORDER BY 1, 2;\n"
    script += f"\\echo FOLLOWERS\n{followers} ORDER BY 1;\n"
    for fan in (FAN, LONE_FAN):
        bound = followable.replace('?', str(fan), 1).replace('?', CANDIDATES, 1)
        script += f"\\echo FOLLOWABLE {fan}\n{bound} ORDER BY 1;\n"
    # The probe names the function unqualified, so it resolves through the test schema.
    script += f"\\echo POLICY\nSELECT ({policy[len('SELECT '):]})::int;\n"
    script += "DROP FUNCTION social_v2_profile_eligible(bigint, bigint[]);\n"
    script += f"\\echo NOPOLICY\nSELECT ({policy[len('SELECT '):]})::int;\nROLLBACK;\n"
    out = subprocess.run(['psql', sys.argv[1], '-X', '-q', '-A', '-t', '-F', ','],
                         input=script, text=True, capture_output=True, check=True).stdout
    observed, section = {}, None
    for line in out.splitlines():
        if re.fullmatch(r'(FOLLOWED|CO|FOLLOWABLE) \d+|FOLLOWERS|POLICY|NOPOLICY', line):
            section = line
            observed[section] = []
        elif line.strip():
            observed[section].append(tuple(int(value) for value in line.split(',')))
    expected = {'FOLLOWERS': EXPECTED_FOLLOWERS, 'POLICY': [(1,)], 'NOPOLICY': [(0,)]}
    for fan, rows in EXPECTED_FOLLOWABLE.items():
        expected[f'FOLLOWABLE {fan}'] = rows
    for fan in (FAN, LONE_FAN, STRANGER):
        expected[f'FOLLOWED {fan}'] = EXPECTED_FOLLOWED[fan]
        expected[f'CO {fan}'] = EXPECTED_CO_FOLLOWS[fan]
    if observed != expected:
        raise SystemExit(f'Recommendation SQL differs: expected {expected}, observed {observed}')
    # No row may offer an artist the fan already follows, or pair an artist with itself.
    mine = {artist for (artist,) in EXPECTED_FOLLOWED[FAN]}
    if any(candidate in mine or seed == candidate or seed not in mine for seed, candidate, _ in observed[f'CO {FAN}']):
        raise SystemExit('Co-follow rows leak a followed artist')
    print('Fan recommendation SQL checked: 3 fans,', len(observed[f'CO {FAN}']), 'shared-fan rows, follow policy applied and absent')


if __name__ == '__main__':
    main()
