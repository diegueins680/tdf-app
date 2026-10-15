#!/usr/bin/env python3
"""Qualify a canonical build image on isolated synthetic Linux resources.

This is a release-image HTTP boundary check, never production release permission.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

spec = importlib.util.spec_from_file_location('fixed_image_shutdown', Path(__file__).with_name('qualify-current-image-shutdown.py'))
base = importlib.util.module_from_spec(spec)
spec.loader.exec_module(base)
require = base.require
ROOT = base.ROOT


def listener_refused(result):
    return result.returncode == 1 and b'/dev/tcp/127.0.0.1/8080: Connection refused' in result.stderr


class CandidateFixture(base.Fixture):
    def __init__(self, output, image, revision, binary):
        require(re.fullmatch(r'diegueins680/tdf-hq@sha256:[a-f0-9]{64}', image) is not None)
        require(re.fullmatch(r'[a-f0-9]{40}', revision) is not None)
        require(binary.is_file() and not binary.is_symlink() and 0 < binary.stat().st_size < 256*1024*1024)
        require(subprocess.check_output(['git', '-C', str(ROOT), 'rev-parse', 'HEAD'], text=True).strip() == revision)
        self.image, self.revision = image, revision
        self.binary = binary
        self.expected_binary = hashlib.sha256(binary.read_bytes()).hexdigest()
        self.migration_count = len(json.loads((ROOT/'scripts/production-migrations.json').read_text())['migrations'])
        super().__init__(output)
        self.report.update({'mode': 'candidate-image', 'binarySha256': self.expected_binary,
            'activeRequestDrainVerified': False, 'releaseAuthorized': False,
            'scope': 'Quiescent TERM/INT and one witnessed missing-course read per signal; not universal request, write, HTTP2 or worker drainage.',
            'exclusions': ['No worker fencing, provider-effect completion, offhost recovery, load-balancer cutover or production release authorization.']})
        # The renderer's source marker must bind the same immutable checkout.
        # run() intentionally clears caller env, so set SOURCE_COMMIT only here.
        env = {**self.environment, 'SOURCE_COMMIT': revision}
        rendered = subprocess.run(['node', str(ROOT/'scripts/render-production-migration-batch.mjs')],
            cwd=ROOT, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=60)
        require(rendered.returncode == 0 and 0 < len(rendered.stdout) < 32*1024*1024)
        self.expected_sql = rendered.stdout

    def validate_migration_bundle(self, sql):
        require(sql == self.expected_sql)

    def source_snapshot(self):
        source = super().source_snapshot()
        require(hashlib.sha256(self.binary.read_bytes()).hexdigest() == self.expected_binary)
        source['testedBinarySha256'] = self.expected_binary
        source['candidateRunnerSha256'] = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
        source['buildWorkflowSha256'] = hashlib.sha256((ROOT/'.github/workflows/build.yml').read_bytes()).hexdigest()
        return source

    def prepare(self):
        super().prepare()
        binary_hash = self.docker(['exec', self.app, 'sha256sum', '/app/tdf-hq-exe']).stdout.decode().split()
        require(len(binary_hash) == 2 and binary_hash[0] == self.expected_binary)
        require(self.docker(['exec', self.app, 'readlink', '/proc/1/exe']).stdout.strip() == b'/app/tdf-hq-exe')
        self.report['pid1ExecutableVerified'] = True

    def process(self, args):
        return subprocess.Popen(['docker', '--host=unix:///var/run/docker.sock', *args],
            env=self.environment, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    @staticmethod
    def finish(process):
        if process.poll() is None:
            process.kill()
        process.communicate(timeout=5)

    def drain_request(self, signal):
        # The missing synthetic slug takes the actual public course lookup path
        # and must return404. No course is created and no provider is contacted.
        slug = 'synthetic-shutdown-'+self.nonce
        lock_name = 'tdf-shutdown-lock-'+self.nonce[:12]
        locker = self.process(['exec', '-i', '-e', 'PGAPPNAME='+lock_name, self.database,
            'psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-U', 'postgres', '-d', 'tdf_hq'])
        reader = stopper = None
        try:
            locker.stdin.write(b'BEGIN; LOCK TABLE course IN ACCESS EXCLUSIVE MODE;\n')
            locker.stdin.flush()
            deadline = time.monotonic()+5
            locked = b'f'
            while time.monotonic() < deadline:
                locked = self.sql(("SELECT EXISTS (SELECT 1 FROM pg_locks l JOIN pg_stat_activity a ON a.pid=l.pid "
                    "WHERE a.application_name='"+lock_name+"' AND l.relation='course'::regclass "
                    "AND l.mode='AccessExclusiveLock' AND l.granted);").encode()).strip()
                if locked == b't': break
                time.sleep(.05)
            require(locked == b't')
            # Run the HTTP client in the still-running database namespace so
            # stopping application PID1 cannot kill the client before EOF.
            reader = self.process(['exec', self.database, 'bash', '-c', base.HTTP,
                'probe', '/public/courses/'+slug])
            waiting = b'f'
            deadline = time.monotonic()+5
            while time.monotonic() < deadline:
                waiting = self.sql(("SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE wait_event_type='Lock' "
                    "AND query LIKE '%"+slug+"%');").encode()).strip()
                if waiting == b't': break
                time.sleep(.05)
            require(waiting == b't')
            started = time.monotonic()
            stopper = self.process(['stop', '--signal='+signal, '--time=10', self.app])
            # Witness actual listener closure while the accepted read remains
            # blocked; process launch or a fixed sleep is not signal delivery.
            closed = False
            deadline = time.monotonic()+3
            while time.monotonic() < deadline:
                probe = self.docker(['exec', self.database, 'timeout', '1', 'bash', '-c',
                    'exec 3<>/dev/tcp/127.0.0.1/8080'], timeout=2, allow_failure=True)
                closed = listener_refused(probe) and time.monotonic() < deadline
                if closed: break
                time.sleep(.05)
            require(closed and self.inspect(self.app)['State']['Running'])
            locker.stdin.write(b'COMMIT;\n\\q\n'); locker.stdin.flush(); locker.stdin.close(); locker.stdin = None
            _, errors = locker.communicate(timeout=5)
            require(locker.returncode == 0 and not errors)
            response, errors = reader.communicate(timeout=8)
            require(reader.returncode == 0 and not errors and len(response) <= 65536)
            head, separator, body = response.partition(b'\r\n\r\n')
            require(separator and head.split(b'\r\n')[0].split()[1] == b'404')
            _, errors = stopper.communicate(timeout=15)
            require(stopper.returncode == 0 and not errors)
            state = self.inspect(self.app)['State']
            elapsed = time.monotonic()-started
            require(not state['Running'] and not state['OOMKilled'] and state['ExitCode'] == 0 and elapsed < 7)
            observation = {'signal': signal, 'kind': 'witnessed-blocked-public-course-read',
                'responseStatus': 404, 'listenerClosedBeforeRelease': True, 'responseBodySha256': hashlib.sha256(body).hexdigest(),
                'elapsedSeconds': round(elapsed, 3), 'exitCode': state['ExitCode']}
            self.report['observations'].append(observation)
            self.docker(['start', self.app])
            observation['recoveredVersion'] = self.healthy()
            require(self.sql(b'SELECT value FROM synthetic_shutdown_evidence;').strip() == b'committed-before-shutdown')
            observation['committedSyntheticDataPreserved'] = True
        finally:
            for process in (locker, reader, stopper):
                if process is not None:
                    self.finish(process)

    def observe(self, signal):
        super().observe(signal)
        observation = self.report['observations'][-1]
        require(observation['exitCode'] == 0 and observation['elapsedSeconds'] < 7)
        self.drain_request(signal)

    def exercise(self):
        super().exercise()
        require(self.report.get('ownedResourcesRemoved') is True and len(self.report['observations']) == 4)
        held = [row for row in self.report['observations'] if row.get('kind') == 'witnessed-blocked-public-course-read']
        require(len(held) == 2 and {row['signal'] for row in held} == {'SIGTERM', 'SIGINT'})
        self.report['status'] = 'candidate-image-http-shutdown-qualified'
        self.report['activeRequestDrainVerified'] = True
        (self.output/'result.json').write_text(json.dumps(self.report, indent=2)+'\n')


def main(arguments):
    require(len(arguments) == 4)
    image, revision, binary, output = arguments
    CandidateFixture(Path(output).absolute(), image, revision, Path(binary).absolute()).exercise()


if __name__ == '__main__':
    main(sys.argv[1:])
