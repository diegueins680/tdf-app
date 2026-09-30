"""Read-only access to the existing TDF Hetzner deployment; never log secrets."""
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys

# Dedicated TDF deployment connection established during the 2026-09-28 cutover.
# Overrides permit key relocation/host rotation without changing verification.
DEFAULT_HOST = 'root@178.105.93.101'
DEFAULT_KEY = Path.home() / '.ssh/tdf_hetzner_deploy_20260928'

REMOTE = r'''
import json, pathlib, stat, subprocess, sys, urllib.request

def capture(args, **kwargs):
    result = subprocess.run(args, capture_output=True, text=True, timeout=180, **kwargs)
    if result.returncode:
        raise RuntimeError('Read-only production command failed')
    return result.stdout

def inspect(name):
    return json.loads(capture(['docker', 'inspect', name]))[0]

def binding(container, service):
    labels = container['Config'].get('Labels') or {}
    if (labels.get('com.docker.compose.project') != 'tdf-production'
        or labels.get('com.docker.compose.service') != service
        or labels.get('com.docker.compose.project.working_dir') != '/opt/tdf/production'
        or not container['State']['Running']):
        raise RuntimeError('Unexpected production container identity')

def raw_env(path):
    return dict(line.split('=', 1) for line in path.read_text().splitlines()
                if '=' in line and not line.lstrip().startswith('#'))

def main(mode):
    api = inspect('tdf-production-api-1')
    db = inspect('tdf-production-db-1')
    binding(api, 'api')
    binding(db, 'db')
    env = dict(entry.split('=', 1) for entry in api['Config']['Env'])
    dbnet = db['NetworkSettings']['Networks'].get('tdf-production_database', {})
    apinet = api['NetworkSettings']['Networks'].get('tdf-production_database', {})
    if (env.get('DB_HOST') != 'db' or env.get('DB_NAME') != 'tdf_hq'
        or env.get('DB_PORT', '5432') != '5432'
        or not apinet.get('NetworkID') or apinet.get('NetworkID') != dbnet.get('NetworkID')
        or 'db' not in (dbnet.get('Aliases') or [])):
        raise RuntimeError('API is not bound to the expected production database')
    configured = raw_env(pathlib.Path('/opt/tdf/production/.env')).get('TDF_IMAGE', '')
    if '@sha256:' not in configured or configured.rsplit('@', 1)[1] != api['Image']:
        raise RuntimeError('Running API does not match configured immutable image')
    if mode == 'credentials':
        path = pathlib.Path('/opt/tdf/production/api.env')
        if stat.S_IMODE(path.stat().st_mode) & 0o077:
            raise RuntimeError('Production credential file permissions are unsafe')
        config = raw_env(path)
        keys = ('SMTP_USERNAME', 'SMTP_PASSWORD')
        if any(not config.get(k) or config[k] != env.get(k) for k in keys):
            raise RuntimeError('Protected mail configuration does not match the running API')
        print(json.dumps({k: config[k] for k in keys}))
    elif mode == 'metadata':
        ip = apinet['IPAddress']
        def get(path):
            with urllib.request.urlopen('http://' + ip + ':8080/' + path, timeout=15) as response:
                return json.load(response)
        print(json.dumps({'provider': 'hetzner', 'project': 'tdf-production',
                          'apiContainer': api['Id'], 'databaseContainer': db['Id'],
                          'apiImage': api['Image'], 'configuredImage': configured,
                          'databaseImage': db['Image'], 'database': 'tdf_hq',
                          'health': get('health'), 'version': get('version')}))
    elif mode == 'inventory':
        sql = sys.stdin.read()
        print(capture(['docker', 'exec', '-i',
                       '-e', 'PGOPTIONS=-c default_transaction_read_only=on',
                       'tdf-production-db-1', 'psql', '-X', '-v', 'ON_ERROR_STOP=1',
                       '-qAt', '-U', 'postgres', '-d', 'tdf_hq'], input=sql), end='')
    else:
        raise RuntimeError('Unsupported read-only operation')

try:
    main(sys.argv[1])
except Exception:
    # Never print subprocess output, SQL values, env contents or exceptions.
    sys.stderr.write('Verified production read-only access failed\n')
    sys.exit(1)
'''


def connection_args(environ=None):
    env = os.environ if environ is None else environ
    host = env.get('TDF_PRODUCTION_SSH_HOST', DEFAULT_HOST)
    key = str(Path(env.get('TDF_PRODUCTION_SSH_KEY', str(DEFAULT_KEY))).expanduser())
    if not re.fullmatch(r'[a-z_][a-z0-9_-]*@[a-zA-Z0-9][a-zA-Z0-9.-]*', host):
        raise ValueError('Invalid production SSH account/host')
    if not Path(key).is_absolute():
        raise ValueError('Production SSH key path must be absolute')
    return ['ssh', '-i', key, '-o', 'IdentitiesOnly=yes', '-o', 'BatchMode=yes',
            '-o', 'StrictHostKeyChecking=yes', '-o', 'ConnectTimeout=10', host]


def remote(mode, sql=None):
    if mode not in ('metadata', 'credentials', 'inventory'):
        raise ValueError('Unsupported production operation')
    command = 'python3 -c ' + shlex.quote(REMOTE) + ' ' + shlex.quote(mode)
    try:
        result = subprocess.run(connection_args() + [command], input=sql,
                                capture_output=True, text=True, timeout=210)
        if result.returncode:
            raise RuntimeError('Production access unavailable')
        return result.stdout
    except (OSError, subprocess.TimeoutExpired):
        raise RuntimeError('Production access unavailable') from None


def mail_credentials():
    try:
        result = json.loads(remote('credentials'))
        if set(result) != {'SMTP_USERNAME', 'SMTP_PASSWORD'} or not all(
                isinstance(v, str) and v for v in result.values()):
            raise ValueError('Invalid configuration')
        return result
    except (ValueError, TypeError):
        raise RuntimeError('Production mail configuration unavailable') from None


if __name__ == '__main__':
    # Credential retrieval is import-only, never a CLI that prints credentials.
    if sys.argv[1:] not in (['metadata'], ['inventory']):
        raise SystemExit('Usage: production_access.py metadata|inventory')
    try:
        print(remote(sys.argv[1], sys.stdin.read() if sys.argv[1] == 'inventory' else None), end='')
    except Exception:
        raise SystemExit('Verified production read-only access failed')
