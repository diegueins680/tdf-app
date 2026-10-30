#!/usr/bin/env python3
"""Finite SQL admission checks, not handler/provider or publication verification.

Run against an explicitly disposable database. Temporary relation projections
shadow the offer's four tables only within a rolled-back connection. The real
handler suite separately executes with the complete production schema.
"""
import hashlib
import itertools
import json
import os
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parent.parent
COMMERCE = ROOT / 'tdf-hq/src/TDF/Server/MusicReleaseCommerce.hs'
ASSETS = ROOT / 'tdf-hq/src/TDF/Server/MusicReleaseAssets.hs'


def offer_query():
    lines = [line.strip() for line in COMMERCE.read_text().splitlines()
             if 'SELECT public.release_version_id,rule.price_minor' in line]
    if len(lines) != 1:
        raise ValueError('Expected exactly one purchase offer SQL expression')
    expression = re.fullmatch(
        r'\(("[^\n]+") <> downloadableAssetPredicate <> ("[^\n]+")\)', lines[0])
    predicate = re.search(r'downloadableAssetPredicate =\n  ("[^\n]+")', ASSETS.read_text())
    if not expression or not predicate:
        raise ValueError('Unrecognized purchase offer or shared download predicate')
    fragment = json.loads(predicate[1])
    query = json.loads(expression[1]) + fragment + json.loads(expression[2])
    parameters = ['1', "'00000000-0000-0000-0000-000000000001'::uuid", 'NULL', 'NULL', 'NULL']
    if query.count('?') != len(parameters):
        raise ValueError('Purchase offer parameter shape changed')
    for value in parameters:
        query = query.replace('?', value, 1)
    return query, fragment


def matrix(query, cases, url):
    sql = ["""BEGIN;
CREATE TEMP TABLE party(id bigint,display_name text,primary_email text);
CREATE TEMP TABLE music_public_release(release_version_id uuid,title text);
CREATE TEMP TABLE music_asset(id uuid,release_version_id uuid,processing_state text,asset_role text,immutable boolean,storage_class text);
CREATE TEMP TABLE music_availability_rule(id uuid,release_version_id uuid,price_minor bigint,currency text,downloadable_asset_id uuid,purchasable boolean,download_policy text,starts_at timestamptz,ends_at timestamptz,territory_mode text,territories text[]);
INSERT INTO party VALUES(1,'Synthetic buyer','buyer@fixture.invalid');
INSERT INTO music_public_release VALUES('00000000-0000-0000-0000-000000000002','Synthetic release');
INSERT INTO music_asset VALUES('00000000-0000-0000-0000-000000000003','00000000-0000-0000-0000-000000000002','ready','cover_original',true,'standard');
INSERT INTO music_availability_rule VALUES('00000000-0000-0000-0000-000000000001','00000000-0000-0000-0000-000000000002',250,'USD','00000000-0000-0000-0000-000000000003',true,'purchase',NULL,NULL,'include',ARRAY['Worldwide']);
"""]
    for state, role, immutable, storage, same in cases:
        version = '00000000-0000-0000-0000-000000000002' if same else '00000000-0000-0000-0000-000000000004'
        sql.append(f"UPDATE music_asset SET processing_state='{state}',asset_role='{role}',immutable={str(immutable).lower()},storage_class='{storage}',release_version_id='{version}';\nSELECT count(*) FROM ({query}) offer;")
    sql.append('ROLLBACK;')
    result = subprocess.run(['psql', url, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'],
                            input='\n'.join(sql), text=True, capture_output=True, check=True)
    values = [int(line) for line in result.stdout.splitlines()]
    if len(values) != len(cases):
        raise ValueError('Incomplete purchase asset matrix')
    return values


def main():
    url = os.environ['TDF_MUSIC_PURCHASE_DATABASE_URL']
    subprocess.run(['node', '--input-type=module', '-e',
                    "import {disposablePostgresUrl} from './scripts/lib/disposable-postgres-url.mjs'; "
                    "disposablePostgresUrl(process.argv[1], {ci:process.env.CI==='true'});", url],
                   cwd=ROOT, check=True)
    before = {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
              for path in [COMMERCE, ASSETS, Path(__file__).resolve()]}
    cases = list(itertools.product(
        ['quarantined', 'uploaded', 'inspecting', 'valid', 'invalid', 'processing', 'ready', 'failed', 'deleted'],
        ['master_audio', 'cover_original', 'stream_audio'], [False, True],
        ['quarantine', 'standard', 'archive'], [False, True]))
    # Independent policy oracle; no implementation fragment is evaluated here.
    expected = [int(same and (state == 'ready' or
                state == 'valid' and role == 'master_audio' and immutable and storage != 'quarantine'))
                for state, role, immutable, storage, same in cases]
    query, predicate = offer_query()
    if matrix(query, cases, url) != expected:
        raise ValueError('Purchase offer disagrees with the download admission policy')
    version_guard = ' AND asset.release_version_id=public.release_version_id'
    if query.count(predicate) != 1 or query.count(version_guard) != 1:
        raise ValueError('Missing or ambiguous mutation targets')
    mutations = {'drop-download-policy': query.replace(predicate, 'TRUE', 1),
                 'drop-release-version': query.replace(version_guard, '', 1)}
    controls = {}
    for name, mutant in mutations.items():
        actual = matrix(mutant, cases, url)
        differences = [i for i, (left, right) in enumerate(zip(actual, expected)) if left != right]
        if not differences:
            raise ValueError('Undetected purchase guard mutation: ' + name)
        first = differences[0]
        controls[name] = {'mismatches': len(differences), 'firstCase': cases[first],
                          'expected': expected[first], 'actual': actual[first]}
    after = {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in before}
    if before != after:
        raise ValueError('Sources changed during SQL verification')
    version = subprocess.check_output(['psql', url, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1',
                                       '-c', 'SHOW server_version'], text=True).strip()
    print(json.dumps({'status': 'TESTED', 'scope': 'finite extracted SQL relation projection',
                      'commit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                      'workingTreeClean': not bool(subprocess.check_output(
                          ['git', 'status', '--porcelain', '--untracked-files=no'], cwd=ROOT, text=True).strip()),
                      'sourceHashes': before, 'postgresServerVersion': version,
                      'cases': len(cases), 'accepted': sum(expected), 'mismatches': 0,
                      'negativeControls': controls}, indent=2))


if __name__ == '__main__':
    main()
