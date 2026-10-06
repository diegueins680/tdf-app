#!/usr/bin/env python3
"""Generate bidirectional traceability. Structural coverage never implies a test pass."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import yaml

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = ROOT / 'formal/system/traceability.json'
FIELDS = ('id', 'title', 'statement', 'domain', 'actors', 'preconditions', 'inputs',
          'state', 'allowedTransitions', 'forbiddenTransitions', 'effects', 'authorization',
          'privacy', 'concurrency', 'idempotency', 'failure', 'recovery', 'observability',
          'source', 'implementation', 'tests', 'formalModels', 'status', 'confidence',
          'ambiguity', 'conformance')
STATUSES = {'specified and implemented', 'specified but missing', 'implemented but unspecified',
            'contradictory', 'deprecated', 'intentionally out of scope'}
RESULTS = {'PASS', 'FAIL', 'PARTIAL', 'NOT IMPLEMENTED', 'IMPLEMENTED BUT UNSPECIFIED',
           'SPECIFICATION AMBIGUITY', 'NOT APPLICABLE'}


class UniqueMappingLoader(yaml.SafeLoader):
    pass


def unique_mapping(loader, node):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in result:
            raise ValueError(f'Duplicate YAML mapping key: {key}')
        result[key] = loader.construct_object(value_node)
    return result


UniqueMappingLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def load_yaml(path):
    return yaml.load(path.read_text(), Loader=UniqueMappingLoader)


def resolve_reference(value, source, root, seen=(), documents=None):
    if not isinstance(value, dict):
        raise ValueError(f'Expected OpenAPI object in {source}')
    if '$ref' not in value:
        return value, source
    reference = value['$ref']
    if (source, reference) in seen:
        raise ValueError('Cyclic OpenAPI path/operation reference')
    file, separator, pointer = reference.partition('#')
    target = (source.parent/file).resolve() if file else source.resolve()
    if not target.is_relative_to(root.resolve()) or '://' in file or not separator or not pointer.startswith('/'):
        raise ValueError(f'Unsupported or escaping OpenAPI reference: {reference}')
    documents = {} if documents is None else documents
    if target not in documents:
        documents[target] = load_yaml(target)
    resolved = documents[target]
    for segment in pointer[1:].split('/'):
        resolved = resolved[segment.replace('~1', '/').replace('~0', '~')]
    # Path-item siblings cannot be silently discarded. This boundary accepts
    # references alone; overlapping definitions require deliberate reconciliation.
    if len(value) != 1:
        raise ValueError(f'OpenAPI reference siblings need explicit reconciliation: {reference}')
    return resolve_reference(resolved, target, root, (*seen, (source, reference)), documents)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def validate_requirements(requirements, root=ROOT):
    if not isinstance(requirements, list):
        raise ValueError('Requirements must be a list')
    seen = set()
    for row in requirements:
        if not isinstance(row, dict):
            raise ValueError('Requirement must be an object')
        missing = [field for field in FIELDS if field not in row]
        if missing:
            raise ValueError(f'{row.get("id")}: missing fields {missing}')
        lists = {'actors', 'preconditions', 'inputs', 'allowedTransitions', 'forbiddenTransitions',
                 'effects', 'implementation', 'tests', 'formalModels', 'ambiguity'}
        for field in FIELDS:
            value = row[field]
            if field in lists:
                valid = isinstance(value, list) and all(isinstance(item, str) and item.strip() for item in value)
                if field == 'actors':
                    valid = valid and bool(value)
            elif field in {'state', 'conformance'}:
                valid = isinstance(value, dict) and bool(value)
            else:
                valid = isinstance(value, str) and bool(value.strip())
            if not valid:
                raise ValueError(f'{row["id"]}: invalid or empty {field}')
        result = row['conformance']
        if not isinstance(result.get('classification'), str) or not isinstance(result.get('reason'), str) or not result['reason'].strip():
            raise ValueError(f'{row["id"]}: conformance requires classification and reason')
        if result['classification'] == 'SPECIFICATION AMBIGUITY' and not row['ambiguity']:
            raise ValueError(f'{row["id"]}: unresolved obligations require explicit ambiguity')
        if row['id'] in seen or not re.fullmatch(r'[A-Z]+-[A-Z0-9-]+', row['id']):
            raise ValueError(f'Duplicate or invalid requirement ID: {row["id"]}')
        seen.add(row['id'])
        if row['status'] not in STATUSES or row['conformance']['classification'] not in RESULTS:
            raise ValueError(f'{row["id"]}: invalid status')
        if row['status'] == 'specified and implemented' and not row['implementation']:
            raise ValueError(f'{row["id"]}: implemented requirement lacks implementation')
        if row.get('critical', False) and (not row['implementation'] or not row['tests']):
            raise ValueError(f'{row["id"]}: critical requirement lacks implementation/test mapping')
        # Receipts are execution outputs, deliberately not handwritten assertions.
        if row['conformance']['classification'] == 'PASS':
            raise ValueError(f'{row["id"]}: PASS belongs in an exact-revision execution receipt')
        for field in ['implementation', 'tests', 'formalModels']:
            for reference in row[field]:
                path = Path(reference)
                target = root/path
                if (path.is_absolute() or '..' in path.parts or not target.is_file()
                        or not target.resolve().is_relative_to(root.resolve())
                        or any((root/parent).is_symlink() for parent in (path, *path.parents))):
                    raise ValueError(f'{row["id"]}: invalid {field} mapping {reference}')
    return seen



def validate_availability(policy, requirements, root=ROOT):
    ids = {row['id'] for row in requirements}
    if policy.get('schemaVersion') != 1 or not isinstance(policy.get('deferredOperations'), list):
        raise ValueError('Invalid API availability policy')
    if not isinstance(policy.get('authority'), str) or not policy['authority'].strip():
        raise ValueError('Invalid API availability authority')
    seen = set()
    for row in policy['deferredOperations']:
        if not isinstance(row, dict):
            raise ValueError('Invalid deferred API entry')
        identity = row.get('id')
        if (not isinstance(identity, str)
                or not re.fullmatch(r'(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS|TRACE|CONNECT) /[^?\s]*', identity)
                or re.sub(r'\{([^}]+)\}', lambda m: '{*}' if m[1].endswith('*') else '{}', identity) != identity
                or identity in seen):
            raise ValueError('Invalid or duplicate deferred API identity')
        seen.add(identity)
        if not isinstance(row.get('reason'), str) or not row['reason'].strip():
            raise ValueError('Deferred API lacks rationale')
        if row.get('requirement') not in ids:
            raise ValueError('Deferred API lacks requirement ownership')
        sources = row.get('sources')
        if not isinstance(sources, list) or not sources:
            raise ValueError('Deferred API lacks provenance')
        for reference in sources:
            if not isinstance(reference, str):
                raise ValueError('Invalid deferred API provenance')
            path = Path(reference)
            target = root/path
            if (path.is_absolute() or '..' in path.parts or not target.is_file()
                    or not target.resolve().is_relative_to(root.resolve())
                    or any((root/parent).is_symlink() for parent in (path, *path.parents))):
                raise ValueError('Invalid deferred API provenance')
    return policy


def validate_response_status(policy, requirements, root=ROOT):
    entries = policy.get('unavailableOperations')
    validate_availability({**policy, 'deferredOperations': entries}, requirements, root)
    for row in entries:
        if type(row.get('compiledStatus')) is not int or not 200 <= row['compiledStatus'] <= 299 or row.get('unavailableStatus') != 503:
            raise ValueError('Invalid unavailable API status boundary')
    return policy


def generate(root=ROOT):
    inventory = json.loads((root/'formal/system/inventory.json').read_text())
    requirements = json.loads((root/'formal/system/requirements.json').read_text())['requirements']
    validate_requirements(requirements, root)
    availability = validate_availability(json.loads((root/'formal/system/api-availability.json').read_text()), requirements, root)
    response_status = validate_response_status(json.loads((root/'formal/system/api-response-status.json').read_text()), requirements, root)
    reverse = {}
    for row in requirements:
        for field in ['implementation', 'tests', 'formalModels']:
            for reference in row[field]:
                reverse.setdefault(reference, []).append({'requirement': row['id'], 'relationship': field})
    surfaces = [{**surface, 'requirements': reverse.get(surface['path'], [])}
                for surface in inventory['surfaces']]
    models = []
    for source in ['docs/revenue-platform/formal-model.yaml', 'docs/music-directory/formal-model.yaml',
                   'docs/operations-control-center/formal-model.yaml']:
        doc = load_yaml(root/source)
        machines = doc.get('state_machines', {})
        if 'lifecycle' in doc:
            machines = {**machines, doc['feature']: doc['lifecycle']}
        for name, definition in machines.items():
            # Retain absent guards/roles as absent; never invent behavioral policy.
            models.append({'id': f'{source}#{name}', 'source': source,
                           'sourceSha256': sha(root/source), 'definition': definition,
                           'correspondence': 'requires per-transition implementation review'})
    for requirement in requirements:
        state = requirement['state']
        if isinstance(state, dict) and 'states' in state and 'transitions' in state:
            models.append({'id': requirement['id'], 'source': 'formal/system/requirements.json',
                           'sourceSha256': sha(root/'formal/system/requirements.json'),
                           'definition': state,
                           'correspondence': 'Mapped requirement evidence required; enumeration alone is not conformance'})
    api_source = root/'tdf-hq/docs/openapi/api.yaml'
    api = load_yaml(api_source)
    documents = {api_source.resolve(): api}
    operations = []
    for route, item in api['paths'].items():
        item, item_source = resolve_reference(item, api_source, root, documents=documents)
        for method, operation in item.items():
            if method not in {'get', 'post', 'put', 'patch', 'delete', 'head', 'options', 'trace'}:
                continue
            operation, operation_source = resolve_reference(operation, item_source, root, documents=documents)
            operations.append({'id': f'{method.upper()} {route}', 'operationId': operation.get('operationId'),
                               'source': str(operation_source.relative_to(root)),
                               'security': operation.get('security', api.get('security', [])),
                               'responses': [str(code) for code in operation.get('responses', {})],
                               'conformance': 'not-established-by-client-generation'})
    registry = json.loads((root/'tdf-hq/assets/feature-registry.json').read_text())
    capabilities = []
    for feature in registry['features']:
        merged = {**registry['defaults'], **feature}
        for action, rule in merged['permissions'].items():
            capabilities.append({'feature': feature['id'], 'resource': merged.get('webRoute'),
                                 'action': action, 'authentication': merged['requiredAuth'],
                                 'rolesAny': rule.get('rolesAny', merged['requiredRoles']),
                                 'rolesAll': rule.get('rolesAll', []),
                                 'modulesAll': sorted(set(merged['requiredModules'] + rule.get('modulesAll', []))),
                                 'modulesAny': rule.get('modulesAny', []),
                                 'strictAdmin': rule.get('strictAdmin', False),
                                 'scope': rule.get('recordScope', merged.get('recordScope')),
                                 'condition': {'flag': merged['featureFlag'], 'maturity': merged['maturity']},
                                 'mobile': merged['mobilePresentation'],
                                 'authority': 'client discovery policy; backend denial requires independent verification'})
    counts = {status: sum(r['conformance']['classification'] == status for r in requirements)
              for status in sorted(RESULTS)}
    return {'schemaVersion': 1, 'mobileRevision': inventory['mobileRevision'],
            'scope': 'Explicit reviewed obligations plus mechanically discovered boundaries; semantic gaps stay open.',
            'requirements': requirements, 'sourceFingerprints': {p: sha(root/p) for p in sorted(reverse)},
            'reverseTraceability': reverse, 'surfaces': surfaces,
            'unmappedSurfaces': [s['id'] for s in surfaces if not s['requirements']],
            'untestedRequirements': [r['id'] for r in requirements if not r['tests']],
            'unmodeledCriticalRequirements': [r['id'] for r in requirements if r.get('critical') and not r['formalModels']],
            'stateMachines': models, 'apiOperations': operations, 'apiAvailability': availability, 'apiResponseStatus': response_status, 'capabilityMatrix': capabilities,
            'conformanceCounts': counts,
            'limitations': ['Role predicates are preserved, not flattened into unconditional grants.',
                            'OpenAPI operations are not proof of Servant or runtime endpoint coverage.',
                            'Unmapped surfaces are visible debt, never silently counted as conformant.',
                            'An unchanged fingerprint is freshness evidence, not behavioral correctness.']}


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    rendered = json.dumps(generate(), ensure_ascii=False, indent=2) + '\n'
    if args.check:
        if not OUTPUT.exists() or OUTPUT.read_text() != rendered:
            raise SystemExit('Specification traceability is stale; regenerate and review the changed mappings')
    else:
        OUTPUT.write_text(rendered)
    print('Specification traceability ' + ('checked' if args.check else 'generated'))
