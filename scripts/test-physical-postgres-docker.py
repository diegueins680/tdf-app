#!/usr/bin/env python3
"""Opt-in PG17 Docker integration, using exclusively new synthetic file trees.

Run as Linux root with TDF_PHYSICAL_TEST_IMAGE set to an already loaded immutable
pgvector image. Never reads or stops a production database/container/volume.
"""
import importlib.util
import json
import os
from pathlib import Path
import re
import sys
import uuid
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('physical', Path(__file__).resolve().parent.parent/
                                            'ops/hetzner/physical-postgres-recovery.py')
p = importlib.util.module_from_spec(spec); spec.loader.exec_module(p)
r = p.restore


def require(value):
    if not value: raise ValueError('Synthetic physical recovery test failed')


def new_directory():
    nonce = uuid.uuid4().hex
    directory = p.HOST_ROOT/('rehearsal-'+nonce)
    with p.files.directory(str(p.HOST_ROOT), private=True): directory.mkdir(mode=0o700)
    return nonce, directory


def main():
    require(sys.platform == 'linux' and os.geteuid() == 0)
    image = os.environ.get('TDF_PHYSICAL_TEST_IMAGE', '')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}', image))
    images = json.loads(r.execute(r.DOCKER+['image', 'inspect', image]))
    require(len(images) == 1 and image in images[0]['RepoDigests'])
    image_id = images[0]['Id']
    nonce, directory = new_directory()
    seed = p.PhysicalClone('0'*64, image, image_id, nonce, directory, '1')
    seed.data.mkdir(mode=0o700); os.chown(seed.data, 999, 999)
    seed.config.mkdir(mode=0o755); seed.config.chmod(0o755)
    for name, content in p.CONFIG_FILES.items():
        (seed.config/name).write_text(content); (seed.config/name).chmod(0o444)
    # Fixture-only setup bypasses cold-copy prepare: this brand-new empty tree is
    # deliberately initialized here, never by the production recovery module.
    seed.prepared_manifest = {}
    with seed.reserved():
        seed.creation_attempted = True
        target = r.execute(seed.create_command()).strip()
        seed.admit(json.loads(r.execute(r.DOCKER+['inspect', target]))[0])
        r.execute(r.DOCKER+['start', target])
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'initdb',
                           '-D', p.DATA, '--auth=trust', '--no-locale'], timeout=60)
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'pg_ctl', '-D', p.DATA,
                  '-l', '/tmp/postgres.log', '-o', '-c config_file='+p.CONFIG+'/postgresql.conf',
                  '-w', '-t', '30', 'start'])
        sql = seed.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'postgres'])
        r.execute(sql, input="CREATE DATABASE tdf_hq; CREATE ROLE recovery_fixture NOLOGIN; "
                  "ALTER SYSTEM SET archive_command='synthetic-command-never-executed';")
        fixture_sql = seed.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq'])
        r.execute(fixture_sql, input="CREATE TABLE recovery_sentinel(id integer PRIMARY KEY, value bytea NOT NULL); "
                  "INSERT INTO recovery_sentinel VALUES(7,decode('0001feff','hex')); "
                  "ALTER TABLE recovery_sentinel OWNER TO recovery_fixture;")
        system_id = r.execute(sql, input='SELECT system_identifier FROM pg_control_system();').strip()
        r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'pg_ctl', '-D', p.DATA,
                           '-m', 'fast', '-w', '-t', '30', 'stop'])
        control = r.execute(r.DOCKER+['exec', target, 'env', '-i', *p.ENV, 'pg_controldata', '-D', p.DATA])
        p.control_identity(control, system_id)
        archive = directory/'synthetic-cold.tar'
        manifest = p.files.capture(str(seed.data), str(archive))
    require(seed.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))
    nonce2, directory2 = new_directory()
    clone = p.PhysicalClone('0'*64, image, image_id, nonce2, directory2, system_id)
    p.files.restore(str(archive), manifest, str(clone.data))
    clone.prepare(manifest)
    require('synthetic-command-never-executed' in (seed.data/'postgresql.auto.conf').read_text())
    with clone.reserved():
        result = clone.start()
        values = r.execute(clone.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq']),
                input="SELECT id::text || ':' || encode(value,'hex') || ':' || "
                "(SELECT tableowner FROM pg_tables WHERE tablename='recovery_sentinel') FROM recovery_sentinel;")
        require(values.strip() == '7:0001feff:recovery_fixture')
        options = r.execute(clone.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'postgres']),
                input="SELECT current_setting('config_file') || ':' || current_setting('archive_mode') || ':' || "
                "(SELECT setting FROM pg_file_settings WHERE name='archive_command' AND applied AND error IS NULL);")
        require(options.strip() == '/recovery-config/postgresql.conf:off:')
        # A container stop is not a clean-cluster receipt. Kill only this owned
        # synthetic clone, then deliberately remove its PID hint: pg_control
        # must still reject the actual crashed cluster on the next recovery.
        r.execute(r.DOCKER+['kill', '--signal=KILL', clone.target])
        clone.inspect()
        with p.files.directory(str(clone.data)) as fd:
            dirty_manifest = p.files.walk(fd)
        try: p.admit_cold_manifest(dirty_manifest)
        except ValueError: pass
        else: raise ValueError('Crash PID marker was not rejected')
        (clone.data/'postmaster.pid').unlink()  # synthetic adversarial fixture only
        crashed_archive = directory2/'synthetic-crashed.tar'
        crashed_manifest = p.files.capture(str(clone.data), str(crashed_archive))
    require(clone.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))

    def restored_clone(archive_path, copied_manifest):
        nonce, target_directory = new_directory()
        value = p.PhysicalClone('0'*64, image, image_id, nonce, target_directory, system_id)
        p.files.restore(str(archive_path), copied_manifest, str(value.data))
        value.prepare(copied_manifest)
        return value

    crashed = restored_clone(crashed_archive, crashed_manifest)
    with crashed.reserved():
        try: crashed.start()
        except ValueError: pass
        else: raise ValueError('Unclean pg_control was accepted')
        require(crashed.target is not None and not crashed.start_attempted)
    require(crashed.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))

    lost = restored_clone(archive, manifest)
    original_execute = r.execute
    lost_responses = []
    def lose_create(command, **kwargs):
        value = original_execute(command, **kwargs)
        if command[:len(r.DOCKER)+1] == r.DOCKER+['create']:
            lost_responses.append(value.strip())
            raise TimeoutError('Synthetic lost create response')
        return value
    with lost.reserved():
        with patch.object(r, 'execute', side_effect=lose_create):
            try: lost.start()
            except TimeoutError: pass
            else: raise ValueError('Lost response did not interrupt startup')
        require(len(lost_responses) == 1 and lost.target is None and lost.creation_attempted)
    require(lost.target is None and not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))

    failed_cleanup = restored_clone(archive, manifest)
    try:
        with patch.object(failed_cleanup, 'cleanup', side_effect=ValueError('Synthetic cleanup failure')):
            with failed_cleanup.reserved(): failed_cleanup.start()
    except ValueError as error:
        require(str(error) == 'Synthetic cleanup failure')
    else: raise ValueError('Cleanup failure was incorrectly reported successful')
    require(os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))
    expected = {'nonce': failed_cleanup.nonce, 'image': image}
    require(json.loads((p.HOST_ROOT/r.PENDING_NAME).read_text()) == expected)
    # This fixture knows create/start completed and cleanup was never attempted.
    # Independently admit the retained target under the same lock before cleanup.
    with r.rehearsal_lock(p.HOST_ROOT):
        failed_cleanup.inspect(); failed_cleanup.cleanup()
        require(failed_cleanup.target is None and not failed_cleanup.creation_attempted)
        r.release_creation(p.HOST_ROOT, failed_cleanup.nonce, image)
    require(not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))
    print(json.dumps({'status': 'passed', 'scope': 'synthetic PG17 cold cluster',
        'copiedBytes': manifest['bytes'], 'exactFixtureAndOwnership': True,
        'originalAutoConfPreserved': True, 'cloneConfigReplacedAfterByteVerification': True,
        'initializedRecoveredDatabase': result['initializedDatabase'],
        'uncleanControlRejectedBeforePostgresStart': True, 'lostCreateResponseRecoveredForCleanup': True,
        'failedCleanupRetainedReservation': True,
        'ownedContainersRemoved': True, 'productionDataAccessed': False}))


if __name__ == '__main__': main()
