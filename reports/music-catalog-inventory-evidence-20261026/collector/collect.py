"""Private, prepare-only until reviewed: fixed aggregates, never raw business rows."""
import datetime
import hashlib
import json
import re
import signal
import subprocess
import time

C = globals().get('CANONICAL')
SPEC = globals().get('QUERY_SPECS')
CANONICAL_SHA256 = globals().get('CANONICAL_SOURCE_SHA256')
MAX_OUTPUT = 65536
ROLE = 'tdf_catalog_inventory'
PREFIX = "BEGIN READ ONLY;\nSET LOCAL ROLE tdf_catalog_inventory;\nSET LOCAL statement_timeout = '2s';\nSET LOCAL lock_timeout = '500ms';\nSET LOCAL search_path = pg_catalog, public;\n"
ERRORS = {'42501':'permission_denied','42P01':'relation_missing','42703':'schema_mismatch',
          '57014':'statement_timeout','55P03':'lock_timeout','25006':'read_only_violation',
          '42704':'role_or_setting_missing','28P01':'authentication_denied',
          '28000':'authentication_denied','53300':'connection_limit','57P03':'database_unavailable'}


def require(value):
    if not value:
        raise ValueError('Collection boundary rejected')


def utc():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def validate(value, shape):
    if isinstance(shape, dict):
        require(isinstance(value, dict) and set(value) == set(shape))
        for key in shape:
            validate(value[key], shape[key])
    elif isinstance(shape, list):
        require(type(value) is str and value in shape)
    elif type(shape) is bool:
        require(value is shape)
    else:
        require(type(shape) is int and type(value) is int and 0 <= value <= 9223372036854775807)


def validate_specs():
    require(SPEC['schemaVersion'] == 1 and len(SPEC['queries']) == 9)
    all_specs = [SPEC['identity'], SPEC['capabilities'], *SPEC['queries']]
    require(len({q['name'] for q in all_specs}) == len(all_specs))
    for query in all_specs:
        require(re.fullmatch(r'[a-z_]{1,80}', query['name']) is not None)
        require(set(query['requiredRelations']) <= set(SPEC['relations']))
        sql = query['sql']
        require(sql.startswith(PREFIX) and sql.endswith(';\nROLLBACK;\n'))
        require(len(sql) <= 24000)
        # Structural guard only, not a SQL parser or proof. The reviewed SQL is fixed.
        require(re.search(r'\b(?:ALTER|GRANT|REVOKE|DROP|CREATE|COPY|CALL|DO|INSERT|UPDATE|DELETE|TRUNCATE|VACUUM|ANALYZE|COMMIT|EXECUTE)\b', sql, re.I) is None)


def observe_containers():
    result = {}
    for service in ('api', 'db', 'edge'):
        ids = C.capture(C.DOCKER + ['ps','--all','--quiet','--no-trunc',
          '--filter','label=com.docker.compose.project='+C.PROJECT,
          '--filter','label=com.docker.compose.service='+service]).split()
        C.require(len(ids) == 1 and re.fullmatch(r'[a-f0-9]{64}',ids[0]))
        objects = json.loads(C.capture(C.DOCKER + ['inspect',ids[0]]))
        C.require(len(objects) == 1)
        result[service] = C.summarize_container(service, objects[0])
        C.require(result[service]['running'] is True)
    return result


def inventory_command(container_id):
    # Retain the canonical Docker socket, local PostgreSQL socket/port, env -i,
    # database and LOGIN role. No role replacement or elevated fallback exists.
    command = C.database_command(container_id)
    require(command[command.index('-U')+1] == ROLE)
    positions = [i for i, arg in enumerate(command) if arg.startswith('PGOPTIONS=')]
    require(len(positions) == 1)
    command[positions[0]] = ('PGOPTIONS=-c default_transaction_read_only=on '
       '-c statement_timeout=2000 -c lock_timeout=500 '
       '-c idle_in_transaction_session_timeout=3000')
    command += ['-v','VERBOSITY=sqlstate']
    return command


def unobserved(reason, *, sqlstate=None, blockers=None):
    result = {'status':'UNOBSERVED','reason':reason,'counts':None}
    if sqlstate is not None:
        result['sqlstate'] = sqlstate
    if blockers is not None:
        result['relationBlockers'] = blockers
    return result


def run_query(container_id, query):
    started = time.monotonic()
    try:
        process = subprocess.run(inventory_command(container_id), input=query['sql'],
          text=True,capture_output=True,timeout=8,env=C.COMMAND_ENV)
    except subprocess.TimeoutExpired:
        return unobserved('process_timeout')
    except (OSError, ValueError):
        return unobserved('process_start_failed')
    # No raw stderr or partially produced SQL output is exported, even on errors.
    if process.returncode != 0:
        match = re.search(r'(?:ERROR|FATAL):\s+([A-Z0-9]{5})(?:\s|$)', process.stderr)
        code = match.group(1) if match and match.group(1) in ERRORS else None
        return unobserved(ERRORS.get(code,'query_failed'),sqlstate=code)
    if len(process.stdout.encode('utf-8')) > MAX_OUTPUT:
        return unobserved('output_boundary_rejected')
    try:
        payload = json.loads(process.stdout)
        validate(payload,query['shape'])
    except (ValueError, TypeError, KeyError):
        return unobserved('output_boundary_rejected')
    return {'status':'OBSERVED','counts':payload,'durationMs':round((time.monotonic()-started)*1000)}


def blocked_by_capabilities(query, capabilities):
    if capabilities['status'] != 'OBSERVED':
        return unobserved('relation_capabilities_unobserved')
    blocked = {table:capabilities['counts'][table] for table in query['requiredRelations']
               if capabilities['counts'][table] != 'select_available'}
    if blocked:
        # These are fixed table names and fixed classification codes, never raw errors.
        reason = 'permission_denied' if 'permission_denied' in blocked.values() else 'relation_coverage_unobserved'
        return unobserved(reason,blockers=blocked)
    return None


def collect():
    validate_specs()
    started = utc()
    before = observe_containers()
    database_id = before['db']['containerId']
    # Sole privileged exception, unchanged canonical function and fixed SQL only.
    # It returns one canonical-identity boolean, never business data.
    C.verify_storage(database_id)
    identity = run_query(database_id,SPEC['identity'])
    results = {}
    if identity['status'] != 'OBSERVED':
        capabilities = unobserved('inventory_session_identity_unobserved')
        results = {q['name']:unobserved('inventory_session_identity_unobserved') for q in SPEC['queries']}
    else:
        capabilities = run_query(database_id,SPEC['capabilities'])
        for query in SPEC['queries']:
            results[query['name']] = blocked_by_capabilities(query,capabilities) or run_query(database_id,query)
    after = observe_containers()
    require(before == after)
    fingerprint = hashlib.sha256(json.dumps(before,sort_keys=True,separators=(',',':')).encode()).hexdigest()
    observed = sum(value['status'] == 'OBSERVED' for value in results.values())
    return {'schemaVersion':1,'status':'OBSERVED' if observed == len(results) else 'PARTIAL_OR_UNOBSERVED',
       'startedAt':started,'finishedAt':utc(),'project':C.PROJECT,'directory':C.DIRECTORY,
       'database':C.DATABASE,'inventoryRole':ROLE,'canonicalContainerSetSha256':fingerprint,
       'canonicalSourceSha256':CANONICAL_SHA256,
       'privilegedIdentityProbe':{'exception':'unchanged canonical.verify_storage only',
          'role':'postgres','validated':True,'fixedSqlSha256':hashlib.sha256(C.STORAGE_SQL.encode()).hexdigest(),
          'scope':'Fixed canonical database/user/local socket/port/read-only/data_directory boolean; no business SELECT,permission fallback,grants or DDL.'},
       'inventorySessionIdentity':identity,'relationCapabilities':capabilities,
       'aggregateFamiliesObserved':observed,'aggregateFamiliesUnobserved':len(results)-observed,
       'aggregates':results,
       'limits':['Sequential read-only transactions,not an atomic catalog snapshot.',
          'All new queries explicitly use inventory role and2s statement timeout; timeouts/permissions/RLS/schema gaps remain UNOBSERVED,not zeros.',
          'DB references only: no storage HEAD,byte/MIME validity,rights,authorization,search completeness or end-to-end playback claim.',
          'Structural unreferenced rows can be legitimate drafts; no deletion/repair action is authorized.',
          'Only fixed counters/buckets and sanitized operational facts leave the host; business IDs,titles,owners,URLs,storage keys and failure text are never selected for output.',
          'Canonical music tables and legacy artist_release references only; other social/podcast/file-upload sources remain outside this collection.']}


def deadline_expired(_signum, _frame):
    raise TimeoutError('Collector deadline')


if __name__ == '__main__':
    signal.signal(signal.SIGALRM, deadline_expired)
    signal.alarm(120)
    try:
        print(json.dumps(collect(),sort_keys=True,separators=(',',':')))
    except Exception:
        # Do not leak exception text,process stderr,container environment or data.
        print(json.dumps({'schemaVersion':1,'status':'UNOBSERVED',
          'reason':'canonical_identity_or_output_boundary_failed','aggregates':None},sort_keys=True))
        raise SystemExit(1)
    finally:
        signal.alarm(0)
