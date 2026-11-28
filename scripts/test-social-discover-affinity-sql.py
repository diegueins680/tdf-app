#!/usr/bin/env python3
"""SOCIAL-DISCOVER-AFFINITY-001: run social_v2_discover(actor, page_size, use_affinity)
of tdf-hq/sql/2026-11-27_social_discover_affinity.sql on PostgreSQL and compare it
with the unmodified social_v2_discover(actor, page_size) and with an oracle of the
rule in formal/system/social-discover-affinity.md.

The database gets the real social foundation and read-model migrations and the
new migration (applied twice). The other tables are reduced to the columns the
functions read. Each generated world is checked under both interaction
authorities (legacy tables, canonical interactions).

The oracle is checked against the reference vectors of
tdf-hq/test/TDF/Social/DiscoverAffinitySpec.hs, which the Haskell reference
order reproduces in the Hspec suite.

Usage: test-social-discover-affinity-sql.py DATABASE_URL   (a disposable, empty database)
"""
import ast
import json
import random
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SQL = ROOT / 'tdf-hq/sql'
MIGRATION = SQL / '2026-11-27_social_discover_affinity.sql'
ROLLBACK = SQL / '2026-11-27_social_discover_affinity_rollback.sql'
READ_MODELS = SQL / '2026-09-14_social_v2_read_models.sql'
FOUNDATION = SQL / '2026-09-14_social_v2_foundation.sql'
SPEC = ROOT / 'tdf-hq/test/TDF/Social/DiscoverAffinitySpec.hs'

WINDOW_DAYS, REPLY_WEIGHT, CAP = 90, 2, 20
ACTOR, QUIET, PARTIES = 1, 2, 44
PAGES = (1, 2, 3, 7, 10, 50)
WORLDS = 12


def psql(url, script, tuples=False):
    args = ['psql', url, '-X', '-q', '-v', 'ON_ERROR_STOP=1']
    if tuples:
        args += ['-A', '-t']
    done = subprocess.run(args, input=script, text=True, capture_output=True)
    if done.returncode != 0:
        raise SystemExit(f'psql failed:\n{done.stderr[-4000:]}')
    return done.stdout


def function_body(path, signature):
    text = path.read_text()
    start = text.index(f'CREATE OR REPLACE FUNCTION social_v2_discover({signature})')
    return text[start:text.index('END $$;', start)]


def between(body, first, last):
    return body[body.index(first):body.index(last, body.index(first))]


def check_shared_text():
    """The candidate set, the base order key and the response of the new function are
    the text of the old one."""
    old = function_body(READ_MODELS, 'actor bigint, page_size integer')
    new = function_body(MIGRATION, 'actor bigint, page_size integer, use_affinity boolean')
    candidates_old = between(old, 'WITH public_sample AS MATERIALIZED', '), ranked AS (')
    candidates_new = between(new, 'WITH public_sample AS MATERIALIZED', '), interaction AS (')
    if candidates_old != candidates_new:
        raise SystemExit('Candidate set of the affinity function differs from social_v2_discover(bigint,integer)')
    key = re.search(r'ORDER BY (shared_interest DESC,\s*md5\(.*?\),id)\s+LIMIT page_size', old, re.S)
    if not key or re.sub(r'\s+', ' ', key.group(1)) not in re.sub(r'\s+', ' ', new):
        raise SystemExit('Base order key of the affinity function differs')
    tail_old, tail_new = old[old.index(') SELECT jsonb_build_object('):], new[new.index(') SELECT jsonb_build_object('):]
    if tail_old != tail_new:
        raise SystemExit('Response of the affinity function differs')
    for needle in (f"interval '{WINDOW_DAYS} days'", f'least(sum(i.weight),{CAP})', 'page_size/2',
                   f'SELECT {REPLY_WEIGHT},'):
        if needle not in new:
            raise SystemExit(f'Constant {needle!r} not found in the migration')


def oracle(page_size, base, units):
    """formal/system/social-discover-affinity.md, section Order. `base` is the previous
    order of the candidate ids, `units` maps an id to its interaction units."""
    with_evidence = sorted((c for c in base if units.get(c, 0) > 0), key=lambda c: (-units[c], c))
    promoted = with_evidence[:page_size // 2]
    return (promoted + [c for c in base if c not in promoted])[:page_size]


def check_reference_vectors():
    source = SPEC.read_text()
    block = re.search(r'^referenceVectors =\n((?:  .*\n)+)', source, re.M)
    if not block:
        raise SystemExit('referenceVectors not found; update the extractor with the spec')
    vectors = ast.literal_eval(block.group(1).strip())
    if len(vectors) < 5:
        raise SystemExit('Too few reference vectors')
    for page_size, base, expected in vectors:
        got = oracle(page_size, [c for c, _ in base], dict(base))
        if got != expected:
            raise SystemExit(f'Oracle differs from reference vector {(page_size, base)}: {got} != {expected}')
    return len(vectors)


SCHEMA = """
CREATE TABLE party(id bigint PRIMARY KEY, display_name text NOT NULL, is_org boolean NOT NULL DEFAULT false);
CREATE TABLE user_credential(id bigint PRIMARY KEY, party_id bigint REFERENCES party(id), active boolean NOT NULL DEFAULT true);
CREATE TABLE fan_club(id bigint PRIMARY KEY, artist_party_id bigint REFERENCES party(id));
CREATE TABLE fan_club_post(id bigint PRIMARY KEY, club_id bigint REFERENCES fan_club(id),
  fan_party_id bigint REFERENCES party(id), parent_id bigint, title text, content text NOT NULL,
  is_hidden boolean NOT NULL DEFAULT false, created_at timestamptz NOT NULL);
CREATE TABLE fan_follow(fan_party_id bigint, artist_party_id bigint, PRIMARY KEY(fan_party_id, artist_party_id));
CREATE TABLE fan_club_officer(club_id bigint, fan_party_id bigint);
CREATE TABLE fan_profile_genre_membership(fan_party_id bigint, genre_id text);
CREATE TABLE artist_profile_genre_membership(artist_party_id bigint, genre_id text);
CREATE TABLE fan_club_post_reaction(post_id bigint NOT NULL, reactor_party_id bigint NOT NULL,
  created_at timestamptz NOT NULL, UNIQUE(post_id, reactor_party_id));
CREATE TABLE interaction_runtime(singleton boolean PRIMARY KEY DEFAULT true, activated_once boolean NOT NULL DEFAULT false);
INSERT INTO interaction_runtime VALUES (true, false);
CREATE TABLE interaction_target(id bigint PRIMARY KEY, entity_kind text NOT NULL, entity_key text NOT NULL,
  UNIQUE(entity_kind, entity_key));
CREATE TABLE interaction_reaction(target_id bigint NOT NULL REFERENCES interaction_target(id), actor_id bigint NOT NULL,
  created_at timestamptz NOT NULL, PRIMARY KEY(target_id, actor_id));
CREATE TABLE interaction_comment(id bigserial PRIMARY KEY, target_id bigint NOT NULL REFERENCES interaction_target(id),
  author_id bigint, state text NOT NULL, created_at timestamptz NOT NULL);
"""

WORLD_TABLES = ('interaction_comment, interaction_reaction, interaction_target, fan_club_post_reaction, '
                'fan_club_post, fan_club_officer, fan_follow, fan_club, fan_profile_genre_membership, '
                'artist_profile_genre_membership, social_v2_pair, social_v2_preference, user_credential, party')


def stamp(days_ago):
    return f"now() - interval '{days_ago} days'"


class World:
    """One generated database state. Everything the oracle needs is kept here, so the
    expected units never come from the SQL under test."""

    def __init__(self, seed):
        rng = random.Random(seed)
        self.rng = rng
        ids = range(1, PARTIES + 1)
        chance = lambda p: rng.random() < p
        self.org = {p: p > QUIET and chance(0.08) for p in ids}
        self.active = {p: p <= QUIET or chance(0.9) for p in ids}
        self.pref = {p: {'discoverable': chance(0.85), 'closed': p > QUIET and chance(0.07)}
                     for p in ids if p <= QUIET or chance(0.9)}
        # State of the pair (actor, p) from the actor's side, for both actors.
        self.pair = {}
        for actor in (ACTOR, QUIET):
            for p in ids:
                if p in (ACTOR, QUIET) or not chance(0.3):
                    continue
                kind = rng.choice(['block_mine', 'block_theirs', 'mute', 'dismiss', 'follow', 'request',
                                   'connected', 'incoming'])
                self.pair[(actor, p)] = kind
        self.genres = {p: rng.sample(['rock', 'jazz', 'folk', 'pop'], rng.randint(0, 2)) for p in ids}
        artists = [p for p in ids if p > QUIET and chance(0.3)] + [ACTOR]
        self.clubs = {index + 1: artist for index, artist in enumerate(artists)}
        self.follows = {artist for artist in artists if artist != ACTOR and chance(0.6)}
        self.officer = {club for club in self.clubs if chance(0.1)}
        self.posts, self.reactions, self.replies = {}, [], []
        next_post = 1
        for club in self.clubs:
            for _ in range(rng.randint(0, 9)):
                self.posts[next_post] = {'club': club, 'author': rng.choice(list(ids)), 'parent': None,
                                         'hidden': chance(0.12)}
                next_post += 1
        top_level = list(self.posts)
        for post in top_level:
            # Reactions of the actor, of the quiet actor (none) and of third parties.
            if chance(0.45):
                self.reactions.append((post, ACTOR, rng.choice([1, 30, 89, 91, 200])))
            for third in rng.sample(range(QUIET + 1, PARTIES + 1), rng.randint(0, 3)):
                self.reactions.append((post, third, rng.choice([1, 30])))
            for _ in range(rng.choice([0, 0, 0, 1, 2, 6])):
                self.replies.append({'id': next_post, 'post': post, 'author': ACTOR,
                                     'days': rng.choice([2, 40, 89, 91, 300]),
                                     'state': rng.choice(['visible', 'visible', 'visible', 'hidden', 'deleted'])})
                next_post += 1
            if chance(0.3):
                self.replies.append({'id': next_post, 'post': post, 'author': rng.randint(QUIET + 1, PARTIES),
                                     'days': 3, 'state': 'visible'})
                next_post += 1

    def pair_row(self, actor, other, kind):
        a, b = min(actor, other), max(actor, other)
        mine, theirs = ('a', 'b') if actor == a else ('b', 'a')
        columns = {'block_mine': [f'block_{mine}'], 'block_theirs': [f'block_{theirs}'], 'mute': [f'mute_{mine}'],
                   'dismiss': [f'dismiss_{mine}'], 'follow': [f'follow_{mine}'], 'request': [f'consent_{mine}'],
                   'incoming': [f'consent_{theirs}'], 'connected': ['consent_a', 'consent_b']}[kind]
        return (f"INSERT INTO social_v2_pair(party_a,party_b,{','.join(columns)}) "
                f"VALUES({a},{b},{','.join(['true'] * len(columns))});")

    def load(self, canonical):
        out = [f'TRUNCATE {WORLD_TABLES}, social_v2_command CASCADE;',
               f'UPDATE interaction_runtime SET activated_once={str(canonical).lower()};']
        for p in range(1, PARTIES + 1):
            out.append(f"INSERT INTO party VALUES({p},'Synthetic {p}',{str(self.org[p]).lower()});")
            out.append(f"INSERT INTO user_credential VALUES({p},{p},{str(self.active[p]).lower()});")
            for genre in self.genres[p]:
                out.append(f"INSERT INTO fan_profile_genre_membership VALUES({p},'{genre}');")
                out.append(f"INSERT INTO artist_profile_genre_membership VALUES({p},'{genre}');")
        for p, pref in self.pref.items():
            out.append("INSERT INTO social_v2_preference(party_id,discoverable,closed) "
                       f"VALUES({p},{str(pref['discoverable']).lower()},{str(pref['closed']).lower()});")
        merged = {}
        for (actor, other), kind in self.pair.items():
            merged.setdefault((min(actor, other), max(actor, other)), (actor, other, kind))
        out += [self.pair_row(*row) for row in merged.values()]
        for club, artist in self.clubs.items():
            out.append(f'INSERT INTO fan_club VALUES({club},{artist});')
        out += [f'INSERT INTO fan_follow VALUES({ACTOR},{artist});' for artist in self.follows]
        out += [f'INSERT INTO fan_club_officer VALUES({club},{ACTOR});' for club in self.officer]
        for post, row in self.posts.items():
            out.append(f"INSERT INTO fan_club_post VALUES({post},{row['club']},{row['author']},NULL,NULL,'p',"
                       f"{str(row['hidden']).lower()},{stamp(5)});")
            out.append(f"INSERT INTO interaction_target VALUES({post},'club_post','{post}');")
        # Targets of another kind whose keys are the post ids, and a malformed key: the
        # actor's interactions with them are not evidence.
        for post in self.posts:
            out.append(f"INSERT INTO interaction_target VALUES({800000 + post},'club_memory','{post}');")
            out.append(f'INSERT INTO interaction_reaction VALUES({800000 + post},{ACTOR},{stamp(1)});')
            out.append("INSERT INTO interaction_comment(target_id,author_id,state,created_at) "
                       f"VALUES({800000 + post},{ACTOR},'visible',{stamp(1)});")
        out.append("INSERT INTO interaction_target VALUES(900002,'club_post','not-a-number');")
        out.append(f'INSERT INTO interaction_reaction VALUES(900002,{ACTOR},{stamp(1)});')
        for post, reactor, days in self.reactions:
            table = ('interaction_reaction' if canonical else 'fan_club_post_reaction')
            out.append(f'INSERT INTO {table} VALUES({post},{reactor},{stamp(days)});')
        # The actor reacts to every reply written by somebody else. A reply is not a
        # top-level post, so this is never evidence for its author.
        for reply in self.replies:
            if reply['author'] == ACTOR:
                continue
            out.append(f"INSERT INTO fan_club_post VALUES({700000 + reply['id']},{self.posts[reply['post']]['club']},"
                       f"{reply['author']},{reply['post']},NULL,'r',false,{stamp(3)});")
            if canonical:
                out.append(f"INSERT INTO interaction_target VALUES({700000 + reply['id']},'club_post','{700000 + reply['id']}');")
                out.append(f"INSERT INTO interaction_reaction VALUES({700000 + reply['id']},{ACTOR},{stamp(1)});")
            else:
                out.append(f"INSERT INTO fan_club_post_reaction VALUES({700000 + reply['id']},{ACTOR},{stamp(1)});")
        for reply in self.replies:
            if canonical:
                out.append('INSERT INTO interaction_comment(target_id,author_id,state,created_at) '
                           f"VALUES({reply['post']},{reply['author']},'{reply['state']}',{stamp(reply['days'])});")
            elif reply['state'] != 'deleted':
                # The legacy table has no deleted state: a deleted reply is no row.
                out.append(f"INSERT INTO fan_club_post VALUES({reply['id']},{self.posts[reply['post']]['club']},"
                           f"{reply['author']},{reply['post']},NULL,'r',{str(reply['state'] == 'hidden').lower()},"
                           f"{stamp(reply['days'])});")
        return '\n'.join(out) + '\n'

    def readable(self, post):
        """The club rule of social_v2_feed, for ACTOR."""
        row = self.posts[post]
        artist = self.clubs[row['club']]
        member = artist == ACTOR or artist in self.follows or row['club'] in self.officer
        return (member and not self.org[artist] and self.active[artist]
                and not self.pref.get(artist, {}).get('closed', False)
                and self.pair.get((ACTOR, artist)) not in ('block_mine', 'block_theirs', 'mute'))

    def units(self):
        """Units of ACTOR towards every author, before intersecting with the candidates."""
        total = {}
        usable = {post for post, row in self.posts.items() if not row['hidden'] and self.readable(post)}
        for post, reactor, days in self.reactions:
            if reactor == ACTOR and days <= WINDOW_DAYS - 1 and post in usable:
                total[self.posts[post]['author']] = total.get(self.posts[post]['author'], 0) + 1
        for reply in self.replies:
            if (reply['author'] == ACTOR and reply['state'] == 'visible' and reply['days'] <= WINDOW_DAYS - 1
                    and reply['post'] in usable):
                author = self.posts[reply['post']]['author']
                total[author] = total.get(author, 0) + REPLY_WEIGHT
        return {author: min(value, CAP) for author, value in total.items()}


def queries(labels_and_sql):
    return ''.join(f"\\echo @@{label}\n{sql};\n" for label, sql in labels_and_sql)


def parse(output):
    result, label = {}, None
    for line in output.splitlines():
        if line.startswith('@@'):
            label = line[2:]
        elif line.strip() and label is not None:
            result[label] = line
    return result


def ids_of(text):
    return [item['partyId'] for item in json.loads(text)['items']]


def check_world(url, seed, canonical):
    world = World(seed)
    authority = 'canonical' if canonical else 'legacy'
    name = f'world {seed} ({authority})'
    asked = [(f'old {a} {n}', f'SELECT social_v2_discover({a},{n})::text') for a in (ACTOR, QUIET) for n in PAGES]
    asked += [(f'new {a} {n} {flag}', f'SELECT social_v2_discover({a},{n},{flag})::text')
              for a in (ACTOR, QUIET) for n in PAGES for flag in ('true', 'false', 'NULL')]
    got = parse(psql(url, world.load(canonical) + queries(asked), tuples=True))
    base = ids_of(got[f'old {ACTOR} 50'])
    if len(base) >= 50:
        raise SystemExit(f'{name}: the fixture must keep the whole order on one page')
    units = {c: u for c, u in world.units().items() if c in base}
    stats = {'promoted': 0, 'with_units': len(units)}
    for n in PAGES:
        old = got[f'old {ACTOR} {n}']
        # P1: use_affinity false or NULL is the old response, byte for byte.
        for flag in ('false', 'NULL'):
            if got[f'new {ACTOR} {n} {flag}'] != old:
                raise SystemExit(f'{name}: page {n} with use_affinity {flag} differs from the old function')
        # P1: an actor without interactions gets the old response.
        for flag in ('true', 'false'):
            if got[f'new {QUIET} {n} {flag}'] != got[f'old {QUIET} {n}']:
                raise SystemExit(f'{name}: actor without evidence differs from the old function at page {n}')
        expected = oracle(n, base, units)
        observed = ids_of(got[f'new {ACTOR} {n} true'])
        if observed != expected:
            raise SystemExit(f'{name}: page {n}: expected {expected}, observed {observed}; units {units}')
        stats['promoted'] = max(stats['promoted'], sum(1 for c in observed[:n // 2] if units.get(c, 0) > 0))
    # P2: same set, and each item (name, reason, relationship) is the old item.
    new_items = {item['partyId']: item for item in json.loads(got[f'new {ACTOR} 50 true'])['items']}
    old_items = {item['partyId']: item for item in json.loads(got[f'old {ACTOR} 50'])['items']}
    if new_items != old_items:
        raise SystemExit(f'{name}: the affinity function returns other candidates or other items')
    reference = got[f'new {ACTOR} 10 true']
    # P3: third parties' behaviour changes nothing.
    noise = []
    for post in world.posts:
        if canonical:
            noise.append(f'INSERT INTO interaction_reaction VALUES({post},{QUIET},now()) ON CONFLICT DO NOTHING;')
            noise.append(f"INSERT INTO interaction_comment(target_id,author_id,state,created_at) VALUES({post},{QUIET},'visible',now());")
        else:
            noise.append(f'INSERT INTO fan_club_post_reaction VALUES({post},{QUIET},now()) ON CONFLICT DO NOTHING;')
            noise.append(f"INSERT INTO fan_club_post VALUES({500000 + post},{world.posts[post]['club']},{QUIET},{post},NULL,'r',false,now());")
    after = parse(psql(url, '\n'.join(noise) + '\n' + queries(
        [('noise', f'SELECT social_v2_discover({ACTOR},10,true)::text')]), tuples=True))
    if after['noise'] != reference:
        raise SystemExit(f"{name}: another party's reactions or replies changed the order")
    # P3: hiding every post removes all evidence. P1: so does the actor's opt-out.
    after = parse(psql(url, 'BEGIN;\nUPDATE fan_club_post SET is_hidden=true WHERE parent_id IS NULL;\n' + queries(
        [('hidden', f'SELECT social_v2_discover({ACTOR},10,true)::text'),
         ('hidden old', f'SELECT social_v2_discover({ACTOR},10)::text')]) + 'ROLLBACK;\n' + 'BEGIN;\n' +
        f'INSERT INTO social_v2_preference(party_id,personalized) VALUES({ACTOR},false) '
        'ON CONFLICT(party_id) DO UPDATE SET personalized=false;\n' + queries(
        [('optout', f'SELECT social_v2_discover({ACTOR},10,true)::text'),
         ('optout old', f'SELECT social_v2_discover({ACTOR},10)::text')]) + 'ROLLBACK;\n', tuples=True))
    if after['hidden'] != after['hidden old']:
        raise SystemExit(f'{name}: hidden posts still count as evidence')
    if after['optout'] != after['optout old']:
        raise SystemExit(f'{name}: an opted-out actor does not get the old response')
    if json.loads(after['optout'])['personalized'] is not False:
        raise SystemExit(f'{name}: the opt-out fixture did not opt out')
    return stats


def check_guards(url):
    got = parse(psql(url, queries([
        ('invalid', f'SELECT social_v2_discover({ACTOR},0,true)::text'),
        ('invalid old', f'SELECT social_v2_discover({ACTOR},0)::text'),
        ('large', f'SELECT social_v2_discover({ACTOR},51,true)::text'),
        ('unknown', 'SELECT social_v2_discover(999999,10,true)::text'),
        ('unknown old', 'SELECT social_v2_discover(999999,10)::text'),
        ('volatility', "SELECT string_agg(provolatile::text,'' ORDER BY pronargs) FROM pg_proc WHERE proname='social_v2_discover'"),
    ]) + 'BEGIN;\nUPDATE social_v2_runtime SET enabled=false;\n' + queries([
        ('disabled', f'SELECT social_v2_discover({ACTOR},10,true)::text'),
        ('disabled old', f'SELECT social_v2_discover({ACTOR},10)::text')]) + 'ROLLBACK;\n', tuples=True))
    for label in ('invalid', 'unknown', 'disabled'):
        if got[label] != got[f'{label} old'] or 'error' not in got[label]:
            raise SystemExit(f'Guard {label} differs from the old function: {got[label]}')
    if got['large'] != got['invalid'] or got['volatility'] != 'ss':
        raise SystemExit(f'Guards or volatility differ: {got}')


def main():
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    url = sys.argv[1]
    check_shared_text()
    vectors = check_reference_vectors()
    migration = MIGRATION.read_text()
    psql(url, SCHEMA + FOUNDATION.read_text() + READ_MODELS.read_text() + migration + migration
         + 'UPDATE social_v2_runtime SET enabled=true;\n')
    worlds = promoted = with_units = 0
    for seed in range(1, WORLDS + 1):
        for canonical in (False, True):
            stats = check_world(url, 20261127 + seed, canonical)
            worlds += 1
            promoted += stats['promoted']
            with_units += stats['with_units']
    if promoted < WORLDS or with_units < 2 * WORLDS:
        raise SystemExit(f'Fixtures too weak: {promoted} promoted, {with_units} candidates with units')
    check_guards(url)
    # The rollback leaves the old function, and the old function alone.
    left = psql(url, ROLLBACK.read_text() + "SELECT string_agg(pronargs::text,',') FROM pg_proc WHERE proname='social_v2_discover';\n",
                tuples=True).strip()
    if left != '2':
        raise SystemExit(f'Rollback left functions with argument counts {left!r}')
    print(f'Discover affinity SQL checked: {vectors} reference vectors; {worlds} worlds, page sizes {PAGES}; '
          f'{with_units} candidates with units, {promoted} promoted on the largest pages; '
          'old-function equality with affinity off, without evidence, opted out and with every post hidden; '
          'third-party interactions ignored; guards; rollback')


if __name__ == '__main__':
    main()
