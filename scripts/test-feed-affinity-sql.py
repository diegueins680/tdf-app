#!/usr/bin/env python3
"""COMMUNITY-FEED-AFFINITY-001: run the evidence SQL from
tdf-hq/src/TDF/Fans/FeedAffinity.hs against PostgreSQL.

The query text is extracted from the Haskell source, never copied, so the test
cannot drift from the implementation. The fixture reproduces only the columns
the queries read (interaction_target, interaction_reaction, interaction_comment
of tdf-hq/sql/2026-09-28_universal_interactions.sql and social_v2_preference of
tdf-hq/sql/2026-09-14_social_v2_foundation.sql). It runs in a fresh schema
inside one transaction that is always rolled back.

Usage: test-feed-affinity-sql.py DATABASE_URL   (a disposable database)
"""
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tdf-hq/src/TDF/Fans/FeedAffinity.hs'


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


VIEWER, OTHER, STRANGER = 100, 101, 102
CUTOFF = "'2026-08-23 12:00:00+00'::timestamptz"
RECENT, OLD = "'2026-11-01 00:00:00+00'", "'2026-05-01 00:00:00+00'"

FIXTURE = f"""
CREATE SCHEMA feed_affinity_test;
SET LOCAL search_path = feed_affinity_test;
CREATE TABLE interaction_target (
  id INT PRIMARY KEY, entity_kind TEXT NOT NULL, entity_key TEXT NOT NULL,
  UNIQUE (entity_kind, entity_key));
CREATE TABLE interaction_reaction (
  target_id INT NOT NULL REFERENCES interaction_target(id), actor_id BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL, PRIMARY KEY (target_id, actor_id));
CREATE TABLE interaction_comment (
  id SERIAL PRIMARY KEY, target_id INT NOT NULL REFERENCES interaction_target(id),
  author_id BIGINT, state TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL);
CREATE TABLE social_v2_preference (party_id BIGINT PRIMARY KEY, personalized BOOLEAN NOT NULL DEFAULT true);
INSERT INTO interaction_target VALUES
  (1,'club_post','10'),(2,'club_memory','5'),(3,'event_moment','7'),(4,'club_post','11');
-- The viewer reacts to post 10 recently, to memory 5 before the window and to an
-- event moment, which is not a club item. Another fan reacts to post 11.
INSERT INTO interaction_reaction VALUES
  (1,{VIEWER},{RECENT}),(2,{VIEWER},{OLD}),(3,{VIEWER},{RECENT}),(4,{OTHER},{RECENT});
-- Two visible comments on post 10 and one on memory 5 count. A deleted and a hidden
-- comment, one before the window, one on the event moment and one by another fan do not.
INSERT INTO interaction_comment (target_id, author_id, state, created_at) VALUES
  (1,{VIEWER},'visible',{RECENT}),(1,{VIEWER},'visible',{RECENT}),(1,{VIEWER},'deleted',{RECENT}),
  (2,{VIEWER},'hidden',{RECENT}),(2,{VIEWER},'visible',{RECENT}),(4,{VIEWER},'visible',{OLD}),
  (3,{VIEWER},'visible',{RECENT}),(1,{OTHER},'visible',{RECENT});
INSERT INTO social_v2_preference VALUES ({VIEWER}, false), ({OTHER}, true);
"""

EXPECTED = {
    f'REACTIONS {VIEWER}': [('club_post', '10')],
    f'REACTIONS {OTHER}': [('club_post', '11')],
    f'REACTIONS {STRANGER}': [],
    f'REPLIES {VIEWER}': [('club_memory', '5'), ('club_post', '10'), ('club_post', '10')],
    f'REPLIES {OTHER}': [('club_post', '10')],
    f'REPLIES {STRANGER}': [],
    f'PREFERENCE {VIEWER}': [('f',)],
    f'PREFERENCE {OTHER}': [('t',)],
    f'PREFERENCE {STRANGER}': [],
}


def main():
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    reactions = extract('canonicalReactionEvidenceSql', 2)
    replies = extract('canonicalReplyEvidenceSql', 2)
    preference = extract('personalizationPreferenceSql', 1)
    script = "\\set ON_ERROR_STOP 1\nBEGIN;\n" + FIXTURE
    for party in (VIEWER, OTHER, STRANGER):
        for label, query in (('REACTIONS', reactions), ('REPLIES', replies)):
            bound = query.replace('?', str(party), 1).replace('?', CUTOFF, 1)
            script += f"\\echo {label} {party}\n{bound} ORDER BY 1, 2;\n"
        script += f"\\echo PREFERENCE {party}\n{preference.replace('?', str(party))};\n"
    script += "ROLLBACK;\n"
    out = subprocess.run(['psql', sys.argv[1], '-X', '-q', '-A', '-t', '-F', ','],
                         input=script, text=True, capture_output=True, check=True).stdout
    observed, section = {}, None
    for line in out.splitlines():
        if re.fullmatch(r'(REACTIONS|REPLIES|PREFERENCE) \d+', line):
            section = line
            observed[section] = []
        elif line.strip():
            observed[section].append(tuple(line.split(',')))
    if observed != EXPECTED:
        raise SystemExit(f'Feed affinity SQL differs: expected {EXPECTED}, observed {observed}')
    print('Feed affinity SQL checked: 3 viewers; window, kind, comment state and actor filters applied')


if __name__ == '__main__':
    main()
