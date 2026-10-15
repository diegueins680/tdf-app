#!/usr/bin/env python3
"""Observe one immutable shipped image on nonce-owned synthetic Linux resources.

This is a diagnostic, never a writer fence, production backup or release approval.
No host mounts, published ports, provider routes or inherited application env.
"""
import hashlib
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import re
import subprocess
import stat
import sys
import time
import uuid

ROOT = Path(__file__).resolve().parent.parent
IMAGE = 'diegueins680/tdf-hq@sha256:a5e6f5e8dd1d7a267c7b00ef6d4263bf11d02dd23842fe2e5f6599c41c48545c'
REVISION = '7ab344400d0878838b9b12742ca4cc5cd7f38f94'
POSTGRES = 'pgvector/pgvector@sha256:3e8b3adfd27b5707128f60956f62a793c3c9326ea8cfaf0eab7adccb5d700b21'
LABEL = 'net.tdf.synthetic-current-image-shutdown'
HTTP = "exec 3<>/dev/tcp/127.0.0.1/8080; printf 'GET %s HTTP/1.1\\r\\nHost: localhost\\r\\nConnection: close\\r\\n\\r\\n' \"$1\" >&3; cat <&3"
APP_ENV = {
    'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
    'LANG': 'C.UTF-8', 'APP_ENV': 'production', 'APP_PORT': '8080',
    'DB_HOST': '127.0.0.1', 'DB_PORT': '5432', 'DB_USER': 'postgres',
    'DB_PASS': 'synthetic-isolated-only', 'DB_NAME': 'tdf_hq', 'DB_SSLMODE': 'disable',
    'RUN_MIGRATIONS': 'false', 'AUTO_APPLY_PRODUCTION_MIGRATIONS': 'false',
    'RESET_DB': 'false', 'SEED_DB': 'false', 'ALLOW_ALL_ORIGINS': 'false',
    'CORS_DISABLE_DEFAULTS': 'true', 'ALLOWED_ORIGINS': 'https://www.tdfrecords.net',
    'HQ_ASSETS_DIR': '/data/assets', 'TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT': '/data/assets/.internal-feedback',
    'EVENT_DISCOVERY_ENABLED': 'false', 'EVENT_DISCOVERY_AUTO_PUBLISH': 'false',
    'EVENT_LOGISTICS_RECHECK_ENABLED': 'false', 'ARTIST_ENRICHMENT_ENABLED': 'false',
    'REPUTATION_AGGREGATION_WORKER_ENABLED': 'false', 'TICKET_CONFIRMATION_WORKER_ENABLED': 'false',
}


def require(condition):
    if not condition:
        raise ValueError('Synthetic image qualification boundary rejected')


class Fixture:
    def __init__(self, output):
        require(sys.platform == 'linux')
        info = output.lstat()
        require(stat.S_ISDIR(info.st_mode) and info.st_uid == os.geteuid()
                and stat.S_IMODE(info.st_mode) == 0o700 and not any(output.iterdir()))
        self.output, self.nonce = output, uuid.uuid4().hex
        self.containers, self.volumes = [], []
        self.environment = {'PATH': os.environ['PATH'], 'LANG': 'C.UTF-8'}
        self.report = {'schemaVersion': 1, 'image': IMAGE, 'imageRevision': REVISION,
                       'postgresImage': POSTGRES, 'fixtureNonce': self.nonce,
                       'status': 'running', 'observations': [], 'productionWritten': False,
                       'releaseAuthorized': False, 'activeRequestDrainVerified': False,
                       'exclusions': ['No external providers, live writers, full worker drain, offhost recovery or production configuration qualification.']}

    def run(self, args, *, data=None, timeout=30, allow_failure=False):
        result = subprocess.run(args, input=data, env=self.environment, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, timeout=timeout)
        require(len(result.stdout) <= 32*1024*1024 and len(result.stderr) <= 4*1024*1024)
        if not allow_failure:
            if result.returncode != 0:
                self.report['failure'] = {'operation': args[:4], 'exitCode': result.returncode,
                                          'syntheticStderr': result.stderr.decode(errors='replace')[:2048]}
            require(result.returncode == 0)
        return result

    def docker(self, args, **kwargs):
        return self.run(['docker', '--host=unix:///var/run/docker.sock', *args], **kwargs)

    def inspect(self, name):
        values = json.loads(self.docker(['inspect', name]).stdout)
        require(len(values) == 1 and values[0]['Config']['Labels'].get(LABEL) == self.nonce)
        return values[0]

    def create(self, suffix, args):
        name = 'tdf-image-qualification-'+self.nonce+'-'+suffix
        self.containers.append(name)  # Remember intent before a possibly lost create reply.
        result = self.docker(['create', '--pull=never', '--name', name, '--label', LABEL+'='+self.nonce,
                              '--read-only', '--cap-drop=ALL', '--security-opt=no-new-privileges:true',
                              '--pids-limit=128', '--memory=768m', '--memory-swap=768m', *args])
        require(re.fullmatch(b'[a-f0-9]{64}\n', result.stdout))
        self.inspect(name)
        return name

    def sql(self, text):
        return self.docker(['exec', '-i', self.database, 'psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1',
                            '-U', 'postgres', '-d', 'tdf_hq'], data=text, timeout=180).stdout

    def probe(self, path):
        require(path in ('/health', '/version'))
        result = self.docker(['exec', self.app, 'bash', '-c', HTTP, 'probe', path],
                             timeout=8, allow_failure=True)
        if result.returncode:
            return None
        head, sep, body = result.stdout.partition(b'\r\n\r\n')
        require(sep and len(body) <= 65536)
        status = head.split(b'\r\n')[0].split()
        require(len(status) >= 2 and status[1].isdigit())
        # Warp normally emits Content-Length for these fixed JSON responses.
        headers = dict(line.split(b':', 1) for line in head.split(b'\r\n')[1:] if b':' in line)
        require(not any(key.lower() == b'transfer-encoding' for key in headers))
        value = json.loads(body)
        require(isinstance(value, dict))
        allowed = {'status', 'db', 'message'} if path == '/health' else {'name', 'version', 'commit', 'buildTime'}
        require(set(value) <= allowed and all(isinstance(v, str) and len(v) <= 256 for v in value.values()))
        return {'status': int(status[1]), 'body': value}

    def healthy(self):
        deadline = time.monotonic()+90
        while time.monotonic() < deadline:
            require(self.inspect(self.app)['State']['Running'])
            health = self.probe('/health')
            if health and health['status'] == 200 and health['body'].get('db') == 'ok':
                version = self.probe('/version')
                require(version and version['status'] == 200 and version['body'].get('commit') == REVISION)
                return version
            time.sleep(.25)
        require(False)

    def prepare(self):
        # Resolve both immutable historical fixtures before any Docker request.
        schemas = {}
        for file in ['production-schema-20260814.sql', 'catalog-production-source-fixture.sql']:
            data = self.run(['git', '-C', str(ROOT), 'show', REVISION+':scripts/__tests__/fixtures/'+file]).stdout
            require(data and len(data) < 32*1024*1024)
            schemas[file] = data
        self.report['fixtureSourceRevision'] = REVISION
        self.report['fixtureSourceHashes'] = {name: hashlib.sha256(data).hexdigest() for name, data in schemas.items()}
        info = json.loads(self.docker(['image', 'inspect', IMAGE]).stdout)[0]
        require(IMAGE in info['RepoDigests'] and info['Architecture'] == 'amd64' and info['Os'] == 'linux')
        require(not info['Config'].get('Entrypoint'))
        env = dict(line.split('=', 1) for line in info['Config']['Env'])
        require(env.get('SOURCE_COMMIT') == env.get('GIT_SHA') == REVISION)
        require(not any(re.search('PASSWORD|TOKEN|SECRET|API_KEY|DB_PASS', key) for key in env))
        self.report['imageId'] = info['Id']
        pg = json.loads(self.docker(['image', 'inspect', POSTGRES]).stdout)[0]
        require(POSTGRES in pg['RepoDigests'])
        self.database = self.create('database', ['--network=none', '--user=999:999',
            '--tmpfs=/var/lib/postgresql/data:rw,nosuid,nodev,size=1073741824,uid=999,gid=999',
            '--tmpfs=/var/run/postgresql:rw,nosuid,nodev,size=16777216,uid=999,gid=999',
            '--tmpfs=/tmp:rw,nosuid,nodev,size=16777216', '--entrypoint=/usr/bin/env', POSTGRES,
            '-i', 'PATH=/usr/lib/postgresql/17/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
            'LANG=C.UTF-8', 'PGDATA=/var/lib/postgresql/data', 'POSTGRES_USER=postgres',
            'POSTGRES_PASSWORD=synthetic-isolated-only', 'POSTGRES_DB=tdf_hq',
            '/usr/local/bin/docker-entrypoint.sh', 'postgres', '-c', 'listen_addresses=127.0.0.1'])
        self.docker(['start', self.database])
        for _ in range(120):
            result = self.docker(['exec', self.database, 'pg_isready', '-U', 'postgres', '-d', 'tdf_hq'], allow_failure=True)
            if result.returncode == 0:
                break
            time.sleep(.25)
        if result.returncode != 0:
            state = self.inspect(self.database)['State']
            logs = self.docker(['logs', '--tail=40', self.database], allow_failure=True)
            self.report['databaseStartup'] = {'running': state['Running'], 'exitCode': state['ExitCode'],
                'oomKilled': state['OOMKilled'], 'syntheticLogs': (logs.stdout+logs.stderr).decode(errors='replace')[:8192]}
        require(result.returncode == 0)
        for schema in schemas.values():
            self.sql(schema)
        mounts = []
        for suffix, destination in [('uploads', '/app/uploads'), ('assets', '/data/assets')]:
            volume = 'tdf-image-qualification-'+self.nonce+'-'+suffix
            self.volumes.append(volume)
            self.docker(['volume', 'create', '--label', LABEL+'='+self.nonce, volume])
            mounts += ['--mount', 'type=volume,src='+volume+',dst='+destination]
        init = self.create('init', ['--network=none', '--user=0:0', '--cap-add=CHOWN', '--cap-add=FOWNER', *mounts,
            IMAGE, 'env', '-i', 'PATH=/usr/bin:/bin', 'sh', '-c',
            'chown 1000:1000 /app/uploads /data/assets; chmod 700 /app/uploads /data/assets'])
        self.docker(['start', '-a', init])
        require(self.inspect(init)['State']['ExitCode'] == 0)
        self.app = self.create('application', ['--network=container:'+self.database, '--user=1000:1000',
            '--workdir=/app', '--tmpfs=/tmp:rw,nosuid,nodev,size=16777216', *mounts,
            IMAGE, 'env', '-i', *[key+'='+value for key, value in APP_ENV.items()], '/app/production-entrypoint.sh'])
        # Read only the immutable packaged SQL before this application can start.
        migration = self.docker(['cp', self.app+':/app/production-migrations.sql', '-']).stdout
        import io, tarfile
        with tarfile.open(fileobj=io.BytesIO(migration)) as archive:
            members = archive.getmembers()
            require(len(members) == 1 and members[0].isfile() and members[0].size < 32*1024*1024)
            sql = archive.extractfile(members[0]).read()
        self.report['packagedMigrationsSha256'] = hashlib.sha256(sql).hexdigest()
        self.sql(sql)
        self.report['migrationCount'] = int(self.sql(b'SELECT count(*) FROM tdf_schema_migration;').strip())
        require(self.report['migrationCount'] == 188)
        self.sql(b'CREATE TABLE synthetic_shutdown_evidence(value text PRIMARY KEY); INSERT INTO synthetic_shutdown_evidence VALUES (\'committed-before-shutdown\');')
        self.docker(['start', self.app])
        self.report['version'] = self.healthy()

    def observe(self, signal):
        require(signal in ('SIGTERM', 'SIGINT'))
        started = time.monotonic()
        self.docker(['stop', '--signal='+signal, '--time=10', self.app], timeout=25)
        state = self.inspect(self.app)['State']
        require(not state['Running'] and not state['OOMKilled'])
        observation = {'signal': signal, 'elapsedSeconds': round(time.monotonic()-started, 3),
                       'exitCode': state['ExitCode'], 'oomKilled': state['OOMKilled'],
                       'quiescentStopAvoidedSigkill': state['ExitCode'] != 137}
        self.report['observations'].append(observation)
        self.docker(['start', self.app])
        observation['recoveredVersion'] = self.healthy()
        observation['committedSyntheticDataPreserved'] = self.sql(b'SELECT value FROM synthetic_shutdown_evidence;').strip() == b'committed-before-shutdown'
        require(observation['committedSyntheticDataPreserved'])

    def cleanup(self):
        for name in reversed(self.containers):
            result = self.docker(['inspect', name], allow_failure=True)
            if result.returncode:
                # Missing objects are the only admissible failed inspect response.
                require(b'No such object:' in result.stderr)
                continue
            self.inspect(name)
            self.docker(['rm', '--force', name])
        for name in reversed(self.volumes):
            rows = json.loads(self.docker(['volume', 'inspect', name]).stdout)
            require(len(rows) == 1 and rows[0]['Labels'].get(LABEL) == self.nonce)
            self.docker(['volume', 'rm', name])
        self.report['ownedResourcesRemoved'] = True

    def source_snapshot(self):
        refs = self.run(['git', '-C', str(ROOT), 'rev-parse', 'HEAD', 'HEAD^{tree}']).stdout.decode().splitlines()
        require(len(refs) == 2 and all(re.fullmatch('[a-f0-9]{40}', value) for value in refs))
        require(not self.run(['git', '-C', str(ROOT), 'status', '--porcelain']).stdout.strip())
        return {'revision': refs[0], 'tree': refs[1], 'clean': True,
                'runnerSha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                'workflowSha256': hashlib.sha256((ROOT/'.github/workflows/interaction-verification.yml').read_bytes()).hexdigest()}

    def exercise(self):
        try:
            self.report['startedAt'] = datetime.now(timezone.utc).isoformat()
            self.report['diagnosticSource'] = self.source_snapshot()
            self.report['stage'] = 'prepare'
            self.prepare()
            for signal in ('SIGTERM', 'SIGINT'):
                self.report['stage'] = 'observe-'+signal
                self.observe(signal)
            require(self.source_snapshot() == self.report['diagnosticSource'])
            self.report['completedAt'] = datetime.now(timezone.utc).isoformat()
            self.report['status'] = 'observed-not-release-qualified'
        except BaseException as error:
            self.report['status'] = 'failed'
            self.report['failureType'] = type(error).__name__
            self.report['syntheticContainerFailures'] = []
            for name in self.containers:
                try:
                    row = self.inspect(name)
                    logs = self.docker(['logs', '--tail=50', name], allow_failure=True)
                    self.report['syntheticContainerFailures'].append({
                        'fixtureRole': name.rsplit('-', 1)[-1], 'running': row['State']['Running'],
                        'exitCode': row['State']['ExitCode'], 'oomKilled': row['State']['OOMKilled'],
                        'syntheticLogs': (logs.stdout+logs.stderr).decode(errors='replace')[:8192]})
                except Exception:
                    self.report['syntheticContainerFailures'].append({'inspectionUnavailable': True})
            raise
        finally:
            try:
                self.cleanup()
            finally:
                (self.output/'result.json').write_text(json.dumps(self.report, indent=2)+'\n')


def main(arguments):
    require(len(arguments) == 1)
    # absolute() preserves the final symlink for Fixture.lstat admission.
    Fixture(Path(arguments[0]).absolute()).exercise()


if __name__ == '__main__':
    main(sys.argv[1:])
