#!/usr/bin/env python3
"""Synthetic cold-cluster -> migrations -> real isolated application integration.

Linux root, preloaded immutable images and at least 2GiB available memory only.
No production database, volume, credentials, provider or public port is used.
"""
import copy
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('synthetic_physical', ROOT/'scripts/test-physical-postgres-docker.py')
fixture = importlib.util.module_from_spec(spec); spec.loader.exec_module(fixture)
p, r, require = fixture.p, fixture.r, fixture.require
canary = p.load('combined_application', 'isolated-application-canary.py')
recovered = p.load('combined_recovered_content', 'recovered-application-content.py')
bundle = recovered.bundle


def sql(target, query, database='tdf_hq'):
    # This fixture owns only newly initialized synthetic data. Never expose
    # production-clone diagnostics through the shared recovery helper.
    require(target.source == '0'*64 and database in ('postgres', 'tdf_hq'))
    target.inspect()
    result = subprocess.run(target.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database]),
                            input=query, text=True, capture_output=True, timeout=180)
    require(len(result.stdout) <= 4*1024*1024 and len(result.stderr) <= 4*1024*1024)
    if result.returncode != 0:
        print('Synthetic migration diagnostic: '+result.stderr[-4096:], file=sys.stderr)
    require(result.returncode == 0)
    return result.stdout.strip()


def synthetic_application_diagnostics(application):
    """Only this fixture's new synthetic database may expose startup diagnostics."""
    require(application.database.source == '0'*64)
    state = application.inspect()['State']
    result = subprocess.run(r.DOCKER+['logs', '--tail', '40', application.target],
                            env=canary.HOST_ENV, text=True, capture_output=True, timeout=10)
    require(result.returncode == 0 and len(result.stdout)+len(result.stderr) <= 65536)
    return {'scope': 'new synthetic fixture only', 'exitCode': state['ExitCode'],
            'oomKilled': state['OOMKilled'], 'running': state['Running'],
            'startupLog': (result.stdout+result.stderr)[-16384:]}


def main():
    require(sys.platform == 'linux' and os.geteuid() == 0)
    image = os.environ.get('TDF_PHYSICAL_TEST_IMAGE', '')
    app_image = os.environ.get('TDF_CANARY_TEST_IMAGE', '')
    revision = os.environ.get('TDF_CANARY_TEST_REVISION', '')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}', image))
    require(re.fullmatch(r'diegueins680/tdf-hq@sha256:[a-f0-9]{64}', app_image))
    require(re.fullmatch(r'[a-f0-9]{40}', revision))
    memory = next(int(line.split()[1])*1024 for line in Path('/proc/meminfo').read_text().splitlines()
                  if line.startswith('MemAvailable:'))
    require(memory >= 2*1024**3)
    for reference in (image, app_image):
        rows = json.loads(r.execute(r.DOCKER+['image', 'inspect', reference]))
        require(len(rows) == 1 and reference in rows[0]['RepoDigests'])
    image_id = json.loads(r.execute(r.DOCKER+['image', 'inspect', image]))[0]['Id']
    batch = subprocess.check_output(['node', 'scripts/render-production-migration-batch.mjs'], cwd=ROOT,
        env={'PATH': os.environ['PATH'], 'SOURCE_COMMIT': revision}, text=True, timeout=30)
    batch_hash = hashlib.sha256(batch.encode()).hexdigest()
    expected_count = len(json.loads((ROOT/'scripts/production-migrations.json').read_text())['migrations'])
    nonce, directory = fixture.new_directory()
    seed = p.PhysicalClone('0'*64, image, image_id, nonce, directory, '1')
    seed.data.mkdir(mode=0o700); os.chown(seed.data, 999, 999)
    seed.config.mkdir(mode=0o755); seed.config.chmod(0o755)
    for name, content in p.CONFIG_FILES.items():
        (seed.config/name).write_text(content); (seed.config/name).chmod(0o444)
    # Test fixture only: initialize new empty synthetic files. The production
    # PhysicalClone.start path never invokes initdb or bypasses cold admission.
    seed.prepared_manifest = {}
    with seed.reserved():
        seed.creation_attempted = True
        target = r.execute(seed.create_command()).strip()
        seed.admit(json.loads(r.execute(r.DOCKER+['inspect', target]))[0])
        r.execute(r.DOCKER+['start', target])
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'initdb',
                           '-D', p.DATA, '--auth=trust', '--no-locale', '--encoding=UTF8'], timeout=60)
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'pg_ctl', '-D', p.DATA,
                  '-l', '/tmp/postgres.log', '-o', '-c config_file='+p.CONFIG+'/postgresql.conf',
                  '-w', '-t', '30', 'start'])
        sql(seed, 'CREATE DATABASE tdf_hq;', 'postgres')
        require(sql(seed, "SHOW server_encoding;") == 'UTF8')
        for name in ('production-schema-20260814.sql', 'catalog-production-source-fixture.sql'):
            sql(seed, (ROOT/'scripts/__tests__/fixtures'/name).read_text())
        system_id = sql(seed, 'SELECT system_identifier FROM pg_control_system();', 'postgres')
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'pg_ctl', '-D', p.DATA,
                           '-m', 'fast', '-w', '-t', '30', 'stop'])
        p.control_identity(r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV,
                                             'pg_controldata', '-D', p.DATA]), system_id)
        archive = directory/'synthetic-application-cold.tar'
        manifest = p.files.capture(str(seed.data), str(archive))
    require(seed.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))

    # One complete file bundle must supply the actual database and application
    # content. Synthetic edge/unit/config sentinels represent the other roots;
    # this fixture does not establish encryption or off-host custody.
    roots = {'database': str(seed.data)}
    for name in bundle.ROLES:
        if name == 'database': continue
        source = seed.directory/('bundle-source-'+name); source.mkdir(mode=0o700)
        roots[name] = str(source)
    assets = Path(roots['production'])/'assets'; assets.mkdir(mode=0o700)
    uploads = Path(roots['legacy-uploads'])
    sentinels = {}
    for name, source in (('assets', assets), ('uploads', uploads)):
        os.chown(source, 1000, 1000)
        sentinel = source/'synthetic-recovery-sentinel'
        sentinel.write_bytes(bytes(range(256))*7); sentinel.chmod(0o600); os.chown(sentinel, 1000, 1000)
        sentinels[name] = hashlib.sha256(sentinel.read_bytes()).hexdigest()
    (Path(roots['production'])/'synthetic-secret').write_bytes(b'SYNTHETIC SECRET ONLY')
    for name in ('edge-data','edge-config','host-units'):
        (Path(roots[name])/'synthetic-config').write_bytes(name.encode())

    def restored():
        nonce, directory = fixture.new_directory()
        return p.PhysicalClone('0'*64, image, image_id, nonce, directory, system_id)

    cleanup_injected = False
    def migrate_and_run(clone, cleanup_failure=False):
        nonlocal cleanup_injected
        binding = {'sourceRevision': revision, 'mobileRevision': '0'*40,
                   'runtimeSha256': hashlib.sha256(b'synthetic disconnected fixture').hexdigest(),
                   'migrationManifestSha256': hashlib.sha256((ROOT/'scripts/production-migrations.json').read_bytes()).hexdigest(),
                   'releaseNonce': clone.nonce, 'databaseSystemIdentifier': system_id}
        plain = clone.directory/'synthetic-complete-bundle.tar'
        receipt = bundle.capture(roots, str(clone.directory/'captured-components'), str(plain), binding)
        preparation = recovered.prepare(clone, plain, receipt, binding, legacy_uploads=True)
        require(preparation['fileReplay']['status'] == 'component-files-restored')
        require((clone.directory/'recovered-bundle/production/synthetic-secret').read_bytes() == b'SYNTHETIC SECRET ONLY')
        content = preparation['contentManifests']
        clone.start()
        require(sql(clone, "SHOW server_encoding;") == 'UTF8')
        sql(clone, batch)
        ledger_query = 'SELECT json_agg(row_to_json(t) ORDER BY migration_id)::text FROM tdf_schema_migration t;'
        first_ledger = sql(clone, ledger_query)
        sql(clone, batch)
        require(sql(clone, ledger_query) == first_ledger)
        require(sql(clone, 'SELECT count(*) FROM tdf_schema_migration;') == str(expected_count))
        for name in ('assets', 'uploads'):
            invalid = copy.deepcopy(content)
            next(row for row in invalid[name]['entries'] if row['kind'] == 'file')['sha256'] = '0'*64
            denied = canary.Canary(r, clone, clone.directory, app_image, revision, restored_content=invalid)
            with clone.with_application(denied):
                try: denied.run()
                except ValueError:
                    require(denied.image_id is not None and denied.target is None and not denied.creation_attempted)
                else: raise ValueError('Restored content mismatch was accepted')
        application = canary.Canary(r, clone, clone.directory, app_image, revision, restored_content=content)
        with clone.with_application(application):
            try:
                evidence = application.run()
            except Exception:
                try:
                    print(json.dumps({'syntheticApplicationFailure': synthetic_application_diagnostics(application)}),
                          file=sys.stderr)
                except Exception:
                    print('Synthetic application diagnostics unavailable', file=sys.stderr)
                raise
            require(evidence['content']['mode'] == 'restored-copy-verified')
            for name, destination in (('assets', '/data/assets'), ('uploads', '/app/uploads')):
                actual = application.execute(['exec', application.target, 'sha256sum',
                                               destination+'/synthetic-recovery-sentinel']).split()[0]
                require(actual == sentinels[name])
            packaged_hash = application.execute(['exec', application.target, 'sha256sum',
                                                '/app/production-migrations.sql']).split()[0]
            require(packaged_hash == batch_hash)
            if cleanup_failure:
                # A completed real run remains alive. Simulate only the removal
                # failure, then verify real application/DB and marker retention.
                cleanup_injected = True
                application.cleanup = lambda: (_ for _ in ()).throw(ValueError('Synthetic application cleanup failure'))
        return evidence

    clone = restored()
    with clone.reserved():
        evidence = migrate_and_run(clone)
        require(clone.active_application is None)
        require(r.execute(r.DOCKER+['ps', '--all', '--quiet', '--filter', 'label='+canary.LABEL]).strip() == '')
        clone.inspect()  # DB survives verified application removal until scope exit.
    require(clone.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))

    failed = restored()
    try:
        with failed.reserved(): migrate_and_run(failed, cleanup_failure=True)
    except ValueError:
        require(cleanup_injected)
        require(failed.active_application is not None and failed.reservation_pid is None)
        application = failed.active_application
        require(application.target is not None and failed.target is not None)
        application.inspect(); failed.inspect()
        require(json.loads((p.HOST_ROOT/r.PENDING_NAME).read_text()) == {'nonce': failed.nonce, 'image': image})
    else:
        raise ValueError('Application cleanup failure was accepted')
    # Exact fixture-only recovery: all create/start requests completed and the
    # injected cleanup function made no Docker request. Never clear uncertainty
    # this way for a production operation with unknown request completion.
    with r.rehearsal_lock(p.HOST_ROOT):
        canary.Canary.cleanup(application)
        require(application.target is None and not application.creation_attempted and not application.paused)
        failed.active_application = None
        failed.cleanup()
        require(failed.target is None and not failed.creation_attempted)
        r.release_creation(p.HOST_ROOT, failed.nonce, image)
    require(not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))
    print(json.dumps({'status': 'passed', 'scope': 'synthetic physical PG17 and isolated application',
        'application': evidence, 'migrations': expected_count, 'migrationBatchSha256': batch_hash,
        'migrationBatchMatchesImage': True, 'migrationReplayStable': True,
        'databaseAssetsAndUploadsFromOneVerifiedBundle': True,
        'encryptionAndOffHostCustodyVerified': False,
        'restoredAssetAndPrivateUploadSentinelsMatch': True,
        'restoredContentMismatchRejectedBeforeApplicationCreation': True,
        'applicationRemovedBeforeDatabase': True, 'failedApplicationCleanupRetainedBothAndReservation': True,
        'ownedContainersRemoved': True, 'productionDataAccessed': False}))


if __name__ == '__main__':
    main()
