#!/usr/bin/env python3
"""Reject newly added implementation/test surfaces without reviewed traceability.

Existing unmapped debt stays visible; it is neither approved nor silently expanded.
Uses immutable Git trees, including both actual Mobile gitlinks.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('specification_inventory', ROOT/'scripts/specification-inventory.py')
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)


def git(root, *args):
    return subprocess.check_output(['git', *args], cwd=root, stderr=subprocess.PIPE)


def full_sha(value):
    if not re.fullmatch(r'[a-f0-9]{40}', value):
        raise ValueError('A full immutable Git revision is required')
    return value


def added(root, base, head):
    return [p for p in git(root, 'diff', '--no-renames', '--name-only', '--diff-filter=A', '-z', base, head).decode().split('\0') if p]


def mobile_pin(root, revision):
    value = git(root, 'ls-tree', revision, '--', 'tdf-mobile').decode().strip()
    match = re.fullmatch(r'160000 commit ([a-f0-9]{40})\ttdf-mobile', value)
    if not match:
        raise ValueError('Missing committed Mobile pin')
    return match[1]


def validate_new_paths(paths, trace, requirements, read_blob):
    rows = {row['path']: row for row in trace['surfaces']}
    register = {row['id']: row for row in requirements}
    checked = []
    for path in sorted(set(paths)):
        if not inventory.material_source(path):
            continue
        row = rows.get(path)
        if not row or not row.get('requirements'):
            raise ValueError('New material surface has no requirement mapping: ' + path)
        if row['sha256'] != hashlib.sha256(read_blob(path)).hexdigest():
            raise ValueError('New material source fingerprint differs: ' + path)
        for link in row['requirements']:
            owner = register.get(link['requirement'])
            relationship = link['relationship']
            if relationship not in {'implementation', 'tests', 'formalModels'} or not owner or path not in owner.get(relationship, []):
                raise ValueError('Generated mapping is absent from canonical requirement: ' + path)
        checked.append(path)
    return checked


def check(root, base, head):
    base, head = full_sha(base), full_sha(head)
    git(root, 'merge-base', '--is-ancestor', base, head)
    trace = json.loads(git(root, 'show', head + ':formal/system/traceability.json'))
    register = json.loads(git(root, 'show', head + ':formal/system/requirements.json'))['requirements']
    paths = added(root, base, head)
    old_mobile, new_mobile = mobile_pin(root, base), mobile_pin(root, head)
    if trace['mobileRevision'] != new_mobile:
        raise ValueError('Traceability does not identify the candidate Mobile pin')
    mobile = root/'tdf-mobile'
    if old_mobile != new_mobile:
        if mobile.is_symlink() or Path(git(mobile, 'rev-parse', '--show-toplevel').decode().strip()).resolve() != mobile.resolve():
            raise ValueError('Initialize the actual pinned Mobile repository')
        if git(mobile, 'rev-parse', 'HEAD').decode().strip() != new_mobile:
            raise ValueError('Mobile checkout differs from candidate pin')
        paths += ['tdf-mobile/' + name for name in added(mobile, old_mobile, new_mobile)]

    def read_blob(path):
        source_root, revision, name = (mobile, new_mobile, path.removeprefix('tdf-mobile/')) if path.startswith('tdf-mobile/') else (root, head, path)
        entry = git(source_root, 'ls-tree', revision, '--', name).decode()
        if not re.fullmatch(r'100(?:644|755) blob [a-f0-9]{40}\t' + re.escape(name) + r'\n', entry):
            raise ValueError('New material surface must be a regular committed file: ' + path)
        return git(source_root, 'show', revision + ':' + name)

    checked = validate_new_paths(paths, trace, register, read_blob)
    return {'base': base, 'head': head, 'mobileBase': old_mobile, 'mobileHead': new_mobile,
            'newMappedSurfaces': checked,
            'scope': 'New material files, including renames and pinned Mobile additions. Existing unmapped debt and semantic mapping correctness remain separate obligations.'}


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--base', required=True)
    parser.add_argument('--head', default='HEAD')
    args = parser.parse_args()
    head = git(ROOT, 'rev-parse', '--verify', args.head + '^{commit}').decode().strip() if args.head == 'HEAD' else full_sha(args.head)
    print(json.dumps(check(ROOT, args.base, head), indent=2))
