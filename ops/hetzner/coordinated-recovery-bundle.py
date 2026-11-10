#!/usr/bin/env python3
"""Bind caller-fenced recovery components into one private, replayable bundle.

No writer fencing, database shutdown, encryption, key custody or deployment is
performed here. The caller admits the exact source roots and their mount topology.
"""
from contextlib import ExitStack
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat

spec = importlib.util.spec_from_file_location('bundle_files', Path(__file__).with_name('recovery-files.py'))
files = importlib.util.module_from_spec(spec); spec.loader.exec_module(files)
ROLES = ('database', 'production', 'edge-data', 'edge-config', 'host-units', 'legacy-uploads')
BINDINGS = {'sourceRevision', 'mobileRevision', 'runtimeSha256', 'migrationManifestSha256',
            'releaseNonce', 'databaseSystemIdentifier'}
MAX_INDEX = 32 * 1024**2


def require(value):
    if not value: raise ValueError('Coordinated recovery bundle boundary rejected')


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'), ensure_ascii=True)+'\n').encode()


def validate_binding(value):
    require(isinstance(value, dict) and set(value) == BINDINGS)
    for name, entry in value.items():
        require(isinstance(entry, str))
        if name == 'databaseSystemIdentifier':
            require(re.fullmatch('[1-9][0-9]{0,19}', entry))
        else:
            length = 40 if name.endswith('Revision') else 32 if name == 'releaseNonce' else 64
            require(re.fullmatch('[a-f0-9]{%d}' % length, entry))


def normalized(path):
    require(isinstance(path, str) and path.startswith('/') and not path.startswith('//') and str(Path(path)) == path
            and '..' not in Path(path).parts)
    return Path(path)


def private_new_directory(path):
    value = normalized(path)
    with files.directory(str(value.parent), private=True) as fd:
        os.mkdir(value.name, 0o700, dir_fd=fd)
        os.fsync(fd)
    return value


def write_index(path, value):
    data = canonical(value); require(len(data) <= MAX_INDEX)
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=parent)
        with os.fdopen(fd, 'wb') as output:
            output.write(data); output.flush(); os.fsync(output.fileno())
        os.fsync(parent)


def archive_digest(path):
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
        with os.fdopen(fd, 'rb') as source:
            before = os.fstat(source.fileno())
            require(stat.S_ISREG(before.st_mode) and before.st_nlink == 1 and before.st_uid == os.geteuid()
                    and stat.S_IMODE(before.st_mode) == 0o600 and 0 < before.st_size <= files.MAX_BYTES)
            result = {'bytes': before.st_size, 'sha256': files.digest(source, before.st_size)}
            require(files.identity(os.fstat(source.fileno())) == files.identity(before))
            return result


def capture(sources, workspace, destination, binding):
    """Caller holds all writer fences for the entire call; all six roots required.

    An absent legacy uploads root must be represented by a caller-created empty
    private source with independently retained absence evidence, never omitted.
    Host units are a caller-admitted staging tree with original metadata/evidence.
    """
    validate_binding(binding)
    require(isinstance(sources, dict) and set(sources) == set(ROLES))
    roots = {name: normalized(path) for name, path in sources.items()}
    work, output = normalized(workspace), normalized(destination)
    all_directories = list(roots.values()) + [work]
    for index, left in enumerate(all_directories):
        for right in all_directories[index+1:]:
            require(left != right and left not in right.parents and right not in left.parents)
    require(all(output != root and root not in output.parents for root in all_directories))
    # Admit every root before creating outputs. Held FDs avoid following a later
    # pathname replacement; the caller separately excludes privileged mutation.
    with ExitStack() as stack:
        descriptors = {name: stack.enter_context(files.directory(str(path))) for name, path in roots.items()}
        identities = [(os.fstat(fd).st_dev, os.fstat(fd).st_ino) for fd in descriptors.values()]
        require(len(set(identities)) == len(identities))
        directory = private_new_directory(str(work))
        components = {}
        for name in ROLES:
            archive = directory/(name+'.tar')
            manifest = files.capture_directory_fd(descriptors[name], str(archive))
            components[name] = {'manifest': manifest, 'archive': archive_digest(archive)}
        # Establish unchanged sampled content across the complete capture window.
        # This does not replace the caller's writer fence or mount admission.
        for name in ROLES:
            require(files.walk(descriptors[name]) == components[name]['manifest'])
            with files.directory(str(roots[name])) as current:
                require((os.fstat(current).st_dev, os.fstat(current).st_ino) ==
                        (os.fstat(descriptors[name]).st_dev, os.fstat(descriptors[name]).st_ino))
        index = {'schemaVersion': 1, 'binding': binding, 'components': components}
        write_index(directory/'index.json', index)
        outer = files.capture(str(directory), str(output))
        digest = archive_digest(output)
    return {'schemaVersion': 1, 'binding': dict(binding), 'archive': digest, 'manifest': outer,
            'status': 'captured-private-components', 'writerFenceEstablishedByHelper': False,
            'encrypted': False, 'offHost': False, 'databaseRecoveryVerified': False}


def restore(source, receipt, expected_binding, destination):
    """Receipt and expected binding require independent trusted provenance.

    First restore/verify the entire outer archive, then replay exactly six inner
    archives. Never restore onto source/production roots or existing destinations.
    """
    validate_binding(expected_binding)
    require(isinstance(receipt, dict) and set(receipt) == {'schemaVersion', 'binding', 'archive', 'manifest',
        'status', 'writerFenceEstablishedByHelper', 'encrypted', 'offHost', 'databaseRecoveryVerified'})
    require(type(receipt['schemaVersion']) is int and receipt['schemaVersion'] == 1
            and canonical(receipt['binding']) == canonical(expected_binding)
            and receipt['status'] == 'captured-private-components'
            and all(receipt[key] is False for key in ('writerFenceEstablishedByHelper', 'encrypted',
                                                      'offHost', 'databaseRecoveryVerified')))
    source = normalized(source)
    require(archive_digest(source) == receipt['archive'])
    root = private_new_directory(destination)
    raw = root/'verified-components'
    files.restore(str(source), receipt['manifest'], str(raw))
    with files.directory(str(raw), private=True) as fd:
        require(set(os.listdir(fd)) == {'index.json'} | {name+'.tar' for name in ROLES})
        index_fd = os.open('index.json', os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=fd)
        with os.fdopen(index_fd, 'rb') as handle:
            info = os.fstat(handle.fileno())
            require(stat.S_ISREG(info.st_mode) and 0 < info.st_size <= MAX_INDEX)
            data = handle.read(MAX_INDEX+1)
        index = json.loads(data)
        require(canonical(index) == data and isinstance(index, dict)
                and set(index) == {'schemaVersion', 'binding', 'components'}
                and type(index['schemaVersion']) is int and index['schemaVersion'] == 1
                and canonical(index['binding']) == canonical(expected_binding)
                and isinstance(index['components'], dict) and set(index['components']) == set(ROLES))
    components = index['components']
    for name in ROLES:
        require(isinstance(components[name], dict) and set(components[name]) == {'manifest', 'archive'})
        files.validate_manifest(components[name]['manifest'])
        require(archive_digest(raw/(name+'.tar')) == components[name]['archive'])
    # All component digests and manifests pass before any inner replay begins.
    totals = {}
    for name in ROLES:
        totals[name] = files.restore(str(raw/(name+'.tar')), components[name]['manifest'], str(root/name))
    return {'schemaVersion': 1, 'binding': dict(expected_binding), 'status': 'component-files-restored',
            'components': totals, 'databaseRecoveryVerified': False, 'secretsUsable': False,
            'offHost': False}
