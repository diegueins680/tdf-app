#!/usr/bin/env python3
"""Prepare a disposable PG/application copy from one verified recovered bundle.

Inputs and returned manifests are private. Caller owns the shared reservation,
writer fence, encrypted transfer/decryption provenance and trusted binding. This
module never stops/starts production, establishes custody, or repairs source data.
"""
import copy
import json
import os
from pathlib import Path

import importlib.util


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module


bundle = load('recovered_bundle', 'coordinated-recovery-bundle.py')
physical = load('recovered_physical', 'physical-postgres-recovery.py')
files = bundle.files
require = bundle.require


def subtree(manifest, prefix):
    """Project a directory's exact captured metadata, without normalizing it."""
    rows = files.validate_manifest(manifest)
    files.name_parts(prefix)
    require(prefix in rows and rows[prefix]['kind'] == 'directory')
    projected = []
    for row in manifest['entries']:
        name = row['path']
        if name == prefix or name.startswith(prefix+'/'):
            projected.append({**row, 'path': name[len(prefix):].lstrip('/')})
    result = {'schemaVersion': 1, 'entries': projected,
              'bytes': sum(row.get('bytes', 0) for row in projected)}
    files.validate_manifest(result)
    return result


def verify_tree(path, manifest):
    with files.directory(str(path)) as fd:
        require(files.walk(fd) == manifest)


def move_verified(source, destination, manifest):
    """Move only caller-owned replay output; do not overwrite an existing target.

    Both parents are pinned without symlink ancestors. Exclusive reservation and
    no noncooperating privileged namespace changes are environment assumptions.
    """
    source, destination = Path(source), Path(destination)
    require(source != destination and source.name and destination.name)
    with files.directory(str(source.parent)) as parent, \
            files.directory(str(destination.parent), private=True) as target:
        with files.relative_directory(parent, [source.name]) as fd:
            require(files.walk(fd) == manifest)
            identity = (os.fstat(fd).st_dev, os.fstat(fd).st_ino)
            try: os.stat(destination.name, dir_fd=target, follow_symlinks=False)
            except FileNotFoundError: pass
            else: require(False)
            named = os.stat(source.name, dir_fd=parent, follow_symlinks=False)
            require((named.st_dev, named.st_ino) == identity)
            os.rename(source.name, destination.name, src_dir_fd=parent, dst_dir_fd=target)
            os.fsync(parent); os.fsync(target)
    verify_tree(destination, manifest)


def prepare(clone, decrypted_bundle, receipt, binding, *, legacy_uploads):
    """Replay six components, then prepare only the verified disposable copy.

    The binding/receipt must come from the caller's trusted capture and verified
    decryption of the retrieved ciphertext, never an untrusted colocated receipt.
    No off-host or clean-shutdown assertion is inferred from file replay.
    """
    require(clone.reservation_pid == os.getpid() and clone.target is None
            and not clone.creation_attempted and not clone.start_attempted
            and clone.prepared_manifest is None and type(legacy_uploads) is bool)
    bundle.validate_binding(binding)
    require(binding['releaseNonce'] == clone.nonce
            and binding['databaseSystemIdentifier'] == clone.system_id)
    # Reject backup-root aliases before replay writes, not only when the later
    # physical configuration stage checks its prepared database directory.
    physical.verify_mounts()
    root = clone.directory/'recovered-bundle'
    require(not any(os.path.lexists(path) for path in
                    (root, clone.data, clone.config, clone.directory/'canary-assets', clone.directory/'canary-uploads')))
    replay = bundle.restore(str(decrypted_bundle), receipt, binding, str(root))
    components = root/'verified-components'
    # Re-admit the exact trusted outer tree before using its private inner index.
    verify_tree(components, receipt['manifest'])
    with files.directory(str(components), private=True) as parent:
        fd = os.open('index.json', os.O_RDONLY | os.O_NOFOLLOW, dir_fd=parent)
        with os.fdopen(fd, 'rb') as handle:
            raw = handle.read(bundle.MAX_INDEX+1)
    require(len(raw) <= bundle.MAX_INDEX)
    index = json.loads(raw)
    require(bundle.canonical(index) == raw and index['binding'] == binding)
    manifests = {name: row['manifest'] for name, row in index['components'].items()}
    database = manifests['database']
    content = {'assets': subtree(manifests['production'], 'assets'),
               'uploads': copy.deepcopy(manifests['legacy-uploads']) if legacy_uploads
                          else subtree(manifests['production'], 'uploads')}
    # Admit ownership before moving anything or invoking clone configuration writes.
    for manifest in content.values():
        row = files.validate_manifest(manifest)['']
        require(row['uid'] == 1000 and row['gid'] == 1000 and row['mode'] & 0o700 == 0o700)
    sources = {'assets': root/'production/assets',
               'uploads': root/'legacy-uploads' if legacy_uploads else root/'production/uploads'}
    verify_tree(root/'database', database)
    for name, manifest in content.items(): verify_tree(sources[name], manifest)
    move_verified(root/'database', clone.data, database)
    for name, manifest in content.items():
        move_verified(sources[name], clone.directory/('canary-'+name), manifest)
    prepared = clone.prepare(database)
    return {'fileReplay': replay, 'physicalPreparation': prepared, 'contentManifests': content,
            'source': 'verified-recovered-bundle', 'offHostCustodyVerifiedByHelper': False,
            'productionDatabaseWritten': False, 'databaseStarted': False}
