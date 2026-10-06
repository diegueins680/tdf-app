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
import hashlib, json, os, pathlib, stat, subprocess, sys, urllib.request

def capture(args, **kwargs):
    # Bind every inspection/exec to this SSH host, never an ambient Docker context.
    if args[0] != 'docker':
        raise RuntimeError('Unexpected production command')
    args = ['docker', '--host', 'unix:///var/run/docker.sock', *args[1:]]
    result = subprocess.run(args, capture_output=True, text=True, timeout=180,
                            env={'PATH': '/usr/local/bin:/usr/bin:/bin', 'LANG': 'C.UTF-8'}, **kwargs)
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
    mounts = [mount for mount in db.get('Mounts', [])
              if mount.get('Destination') == '/var/lib/postgresql/data']
    if (len(mounts) != 1 or mounts[0].get('Type') != 'volume'
        or mounts[0].get('Name') != 'tdf_production_postgres_data'
        or any(mount.get('Destination', '').startswith('/var/lib/postgresql/data/')
               for mount in db.get('Mounts', []))):
        raise RuntimeError('Database is not using the authoritative production volume')
    db_settings = {}
    for entry in db['Config']['Env']:
        key, separator, value = entry.partition('=')
        if not separator or key in db_settings:
            raise RuntimeError('Invalid database configuration')
        db_settings[key] = value
    if db_settings.get('PGDATA') != '/var/lib/postgresql/data':
        raise RuntimeError('Database directory is not canonical')
    # The restricted inventory role cannot inspect data_directory. This fixed
    # boolean query uses existing local administration without granting it any
    # new capability or allowing caller-provided SQL under that role.
    storage = capture(['docker', 'exec', '-i', 'tdf-production-db-1', 'env', '-i',
        'PATH=/usr/local/bin:/usr/bin:/bin', 'PGOPTIONS=-c default_transaction_read_only=on',
        'PGCONNECT_TIMEOUT=10', 'psql', '-X', '-h', '/var/run/postgresql', '-p', '5432',
        '-v', 'ON_ERROR_STOP=1', '-qAt', '-U', 'postgres', '-d', 'tdf_hq', '-c',
        "SELECT current_database()='tdf_hq' AND current_user='postgres' AND inet_server_addr() IS NULL AND current_setting('port')='5432' AND current_setting('transaction_read_only')='on' AND current_setting('data_directory')='/var/lib/postgresql/data';"])
    if storage.strip() != 't':
        raise RuntimeError('Effective database storage is not canonical')
    env = dict(entry.split('=', 1) for entry in api['Config']['Env'])
    dbnet = db['NetworkSettings']['Networks'].get('tdf-production_database', {})
    apinet = api['NetworkSettings']['Networks'].get('tdf-production_database', {})
    if (env.get('DB_HOST') != 'db' or env.get('DB_NAME') != 'tdf_hq'
        or env.get('DB_PORT', '5432') != '5432'
        or not apinet.get('NetworkID') or apinet.get('NetworkID') != dbnet.get('NetworkID')
        or 'db' not in (dbnet.get('Aliases') or [])):
        raise RuntimeError('API is not bound to the expected production database')
    deployment_config = raw_env(pathlib.Path('/opt/tdf/production/.env'))
    configured = deployment_config.get('TDF_IMAGE', '')
    image = json.loads(capture(['docker', 'image', 'inspect', api['Image']]))[0]
    if '@sha256:' not in configured or (api['Config'].get('Image') != configured
            and configured not in (image.get('RepoDigests') or [])):
        raise RuntimeError('Running API does not match configured immutable image')
    configured_database = deployment_config.get('POSTGRES_IMAGE', '')
    database_image = json.loads(capture(['docker', 'image', 'inspect', db['Image']]))[0]
    if '@sha256:' not in configured_database or (db['Config'].get('Image') != configured_database
            and configured_database not in (database_image.get('RepoDigests') or [])):
        raise RuntimeError('Running database does not match configured immutable image')
    if mode == 'credentials':
        path = pathlib.Path('/opt/tdf/production/api.env')
        # Inspect the opened file, refusing symlinks and avoiding a check/read race.
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
        with os.fdopen(fd) as credential_file:
            metadata = os.fstat(credential_file.fileno())
            if (metadata.st_uid != 0 or not stat.S_ISREG(metadata.st_mode)
                    or stat.S_IMODE(metadata.st_mode) & 0o077):
                raise RuntimeError('Production credential file ownership/type/permissions are unsafe')
            config = dict(line.split('=', 1) for line in credential_file.read().splitlines()
                          if '=' in line and not line.lstrip().startswith('#'))
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
                          'databaseImage': db['Image'], 'configuredDatabaseImage': configured_database,
                          'database': 'tdf_hq',
                          'databaseVolume': mounts[0]['Name'],
                          'sshServerAddress': os.environ['SSH_CONNECTION'].split()[2],
                          'health': get('health'), 'version': get('version')}))
    elif mode == 'inventory':
        sql = sys.stdin.read()
        if hashlib.sha256(sql.encode()).hexdigest() != 'f4c536d1d4554386817b1f44e6a7281ff7c0eb2a435234a8bd6a8ef9c06d394d':
            raise RuntimeError('Only the reviewed catalog query is permitted')
        # Clear the container's libpq environment as well as pinning socket/port.
        # PGHOSTADDR/PGSERVICE can otherwise redirect an apparently local query.
        reader = ['docker', 'exec', '-i', 'tdf-production-db-1', 'env', '-i',
                  'PATH=/usr/local/bin:/usr/bin:/bin', 'PGOPTIONS=-c default_transaction_read_only=on',
                  'PGCONNECT_TIMEOUT=10', 'psql', '-X', '-h', '/var/run/postgresql', '-p', '5432', '-v', 'ON_ERROR_STOP=1',
                  '-qAt', '-U', 'tdf_catalog_inventory', '-d', 'tdf_hq']
        connection_guard = "DO $$ BEGIN IF current_database()<>'tdf_hq' OR current_user<>'tdf_catalog_inventory' OR inet_server_addr() IS NOT NULL OR current_setting('port')<>'5432' THEN RAISE EXCEPTION 'Unexpected production database connection'; END IF; END $$;\n"
        coverage = capture(reader + ['-c', connection_guard + "SELECT NOT EXISTS (SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND NOT has_table_privilege(current_user,c.oid,'SELECT'))"]).strip()
        if coverage != 't':
            raise RuntimeError('Catalog reader lacks reviewed access to current tables')
        print(capture(reader, input=connection_guard + sql), end='')
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
