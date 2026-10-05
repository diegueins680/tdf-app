#!/usr/bin/env python3
"""Read-only, allowlisted facts from the canonical Hetzner host. No secret output."""
import json
import re
import subprocess
import sys
import urllib.request

DOCKER = ['env', '-u', 'DOCKER_HOST', '-u', 'DOCKER_CONTEXT', '-u', 'DOCKER_TLS_VERIFY',
          '-u', 'DOCKER_CERT_PATH', 'docker', '--host', 'unix:///var/run/docker.sock']

PROJECT = 'tdf-production'
DIRECTORY = '/opt/tdf/production'
DATABASE = 'tdf_hq'
VOLUME = 'tdf_production_postgres_data'
ORIGINS = {'https://www.tdfrecords.net', 'https://tdfrecords.net'}
BOOLEAN_KEYS = {
    'RUN_MIGRATIONS', 'AUTO_APPLY_PRODUCTION_MIGRATIONS', 'RESET_DB', 'SEED_DB',
    'ALLOW_ALL_ORIGINS', 'CORS_DISABLE_DEFAULTS', 'SESSION_COOKIE_SECURE',
    'SOCIAL_V2_ENABLED', 'EVENT_LOGISTICS_RECHECK_ENABLED',
}


def require(condition):
    if not condition:
        raise ValueError('Unexpected production inspection boundary')


def capture(command, input=None):
    result = subprocess.run(command, input=input, text=True, capture_output=True, timeout=45)
    # Never forward process stderr: Docker/psql errors can include configuration.
    require(result.returncode == 0 and len(result.stdout) <= 4 * 1024 * 1024)
    return result.stdout


def token(value):
    require(isinstance(value, str) and re.fullmatch(r'[A-Za-z0-9_.:-]{1,160}', value))
    return value


def boolean(value):
    require(isinstance(value, str))
    v = value.lower()
    require(v in {'true', 'false', '1', '0', 'yes', 'no', 'on', 'off', ''})
    # Preserve absence/empty distinction; this is configuration, not default resolution.
    return value


def summarize_container(service, container):
    labels = container['Config']['Labels']
    require(labels.get('com.docker.compose.project') == PROJECT)
    require(labels.get('com.docker.compose.service') == service)
    require(labels.get('com.docker.compose.project.working_dir') == DIRECTORY)
    require(labels.get('com.docker.compose.project.config_files') == DIRECTORY + '/compose.yaml')
    require(re.fullmatch(r'[a-f0-9]{64}', container['Id']))
    require(re.fullmatch(r'sha256:[a-f0-9]{64}', container['Image']))
    image = container['Config']['Image']
    require(isinstance(image, str) and re.fullmatch(r'[a-zA-Z0-9./_-]+@sha256:[a-f0-9]{64}', image))
    result = {'containerId': container['Id'], 'imageId': container['Image'], 'image': image,
              'running': container['State']['Running'],
              'health': token(container['State'].get('Health', {}).get('Status', 'not-configured'))}
    require(isinstance(result['running'], bool))
    networks = container['NetworkSettings']['Networks']
    require(set(networks) <= {PROJECT + '_database', PROJECT + '_outbound'})
    result['networks'] = sorted(networks)
    if service == 'db':
        require(set(networks) == {PROJECT + '_database'})
        require(not any(container['NetworkSettings'].get('Ports', {}).values()))
        mounts = [m for m in container['Mounts'] if m['Destination'] == '/var/lib/postgresql/data']
        require(len(mounts) == 1 and mounts[0]['Type'] == 'volume' and mounts[0]['Name'] == VOLUME)
        result['volume'] = VOLUME
    if service == 'api':
        require(set(networks) == {PROJECT + '_database', PROJECT + '_outbound'})
        env = {}
        for entry in container['Config']['Env']:
            key, sep, value = entry.partition('=')
            require(bool(sep) and key not in env)
            env[key] = value
        flags = {k: boolean(v) for k, v in env.items()
                 if k in BOOLEAN_KEYS or k.endswith(('_ENABLED', '_AUTO_PUBLISH'))}
        result['booleanConfiguration'] = flags
        result['missingBooleanConfiguration'] = sorted(BOOLEAN_KEYS - set(flags))
        origins = {x.strip() for x in env.get('ALLOWED_ORIGINS', '').split(',') if x.strip()}
        result['cors'] = {'canonicalOriginsPresent': sorted(origins & ORIGINS),
                          'otherOriginCount': len(origins - ORIGINS)}
        result['productionEnvironment'] = env.get('APP_ENV') == 'production'
    return result


SQL = """BEGIN READ ONLY;
SELECT json_build_object(
 'serverVersion',current_setting('server_version'),
 'readOnly',current_setting('transaction_read_only'),
 'database',current_database(),
 'role',current_user,
 'localConnection',inet_server_addr() IS NULL,
 'migrations',(SELECT coalesce(json_agg(x ORDER BY migration_id),'[]'::json) FROM
   (SELECT migration_id,checksum,source_commit FROM public.tdf_schema_migration) x),
 'revenueFlags',(SELECT coalesce(json_agg(x ORDER BY flag_key),'[]'::json) FROM
   (SELECT flag_key,enabled,environment FROM public.revenue_feature_flag WHERE environment='production') x),
 'providerAccounts',(SELECT coalesce(json_agg(x ORDER BY provider),'[]'::json) FROM
   (SELECT provider,environment,status,contract_status,credential_status,enabled,feature_flag_key
      FROM public.commerce_provider_account WHERE environment='production') x),
 'socialRuntime',(SELECT json_build_object('enabled',enabled,'activatedOnce',activated_once)
      FROM public.social_v2_runtime WHERE singleton),
 'extensions',(SELECT json_agg(x ORDER BY extname) FROM
   (SELECT extname,extversion FROM pg_extension) x));
ROLLBACK;
"""


def summarize_database(data):
    require(data['database'] == DATABASE and data['readOnly'] == 'on' and data['localConnection'] is True
            and data['role'] == 'tdf_catalog_inventory')
    version = data['serverVersion']
    require(isinstance(version, str) and re.fullmatch(r'17\.[0-9]+(?: [A-Za-z0-9().+~ /:-]+)?', version))
    result = {'database': DATABASE, 'role': 'tdf_catalog_inventory', 'readOnly': True,
              'serverVersion': version, 'localConnection': True, 'migrations': [], 'revenueFlags': [],
              'providerAccounts': [], 'extensions': []}
    social = data['socialRuntime']
    require(social is None or (isinstance(social, dict)
            and isinstance(social.get('enabled'), bool)
            and isinstance(social.get('activatedOnce'), bool)))
    # No row is unknown authority, not a false activation-history claim.
    result['socialRuntime'] = None if social is None else {
        key: social[key] for key in ('enabled', 'activatedOnce')}
    seen = set()
    for row in data['migrations']:
        identifier = token(row['migration_id'])
        require(identifier not in seen)
        seen.add(identifier)
        require(re.fullmatch(r'[a-f0-9]{64}', row['checksum']))
        require(re.fullmatch(r'[a-f0-9]{40}', row['source_commit']))
        result['migrations'].append({k: row[k] for k in ('migration_id', 'checksum', 'source_commit')})
    for row in data['revenueFlags']:
        require(row['environment'] == 'production' and isinstance(row['enabled'], bool))
        result['revenueFlags'].append({'flag': token(row['flag_key']), 'enabled': row['enabled']})
    for row in data['providerAccounts']:
        require(row['environment'] == 'production' and isinstance(row['enabled'], bool))
        safe = {k: token(row[k]) for k in ('provider', 'status', 'contract_status', 'credential_status')}
        safe['enabled'] = row['enabled']
        safe['featureFlag'] = None if row['feature_flag_key'] is None else token(row['feature_flag_key'])
        result['providerAccounts'].append(safe)
    for row in data['extensions']:
        result['extensions'].append({'name': token(row['extname']), 'version': token(row['extversion'])})
    return result


def database_command(container_id):
    require(re.fullmatch(r'[a-f0-9]{64}', container_id))
    return DOCKER + ['exec', '-i', container_id, 'env', '-u', 'PGHOSTADDR',
            '-u', 'PGSERVICE', '-u', 'PGSERVICEFILE',
            'PGOPTIONS=-c default_transaction_read_only=on -c statement_timeout=15000',
            'psql', '-X', '-h', '/var/run/postgresql', '-p', '5432', '-v', 'ON_ERROR_STOP=1',
            '-qAt', '-U', 'tdf_catalog_inventory', '-d', DATABASE]


def inspect():
    containers = {}
    for service in ('api', 'db', 'edge'):
        ids = capture(DOCKER + ['ps', '--all', '--quiet', '--no-trunc',
                       '--filter', 'label=com.docker.compose.project=' + PROJECT,
                       '--filter', 'label=com.docker.compose.service=' + service]).split()
        require(len(ids) == 1 and re.fullmatch(r'[a-f0-9]{64}', ids[0]))
        values = json.loads(capture(DOCKER + ['inspect', ids[0]]))
        require(len(values) == 1)
        containers[service] = summarize_container(service, values[0])
    data = json.loads(capture(database_command(containers['db']['containerId']), input=SQL))
    with urllib.request.urlopen('https://api.tdfrecords.net/version', timeout=15) as response:
        require(response.status == 200)
        public = json.loads(response.read(4096))
    require(public['name'] == 'tdf-hq' and re.fullmatch(r'[a-f0-9]{40}', public['commit']))
    require(re.fullmatch(r'[0-9]+(?:\.[0-9]+){2,3}', public['version']))
    return {'schemaVersion': 1, 'project': PROJECT, 'directory': DIRECTORY,
            'containers': containers, 'database': summarize_database(data),
            'publicBackend': {k: public[k] for k in ('name', 'commit', 'version')},
            'limitations': ['Read-only observation, not deployment eligibility or approval.',
                           'Observations are sequential, not an atomic fleet snapshot.',
                           'Absent runtime variables retain unknown effective defaults.',
                           'Database flags are recorded, not activated; no provider transaction was sent.',
                           'Container image and public version are separate observations until image metadata is verified.']}


if __name__ == '__main__':
    try:
        print(json.dumps(inspect(), sort_keys=True))
    except Exception:
        # No raw subprocess errors, env, credentials, SQL data or request payloads.
        print('Hetzner inspection failed a read-only identity or data boundary; no runtime output emitted.', file=sys.stderr)
        sys.exit(1)
