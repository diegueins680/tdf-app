#!/usr/bin/env python3
"""Install only checksum-pinned official age tools into a new private directory."""
import argparse
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path, PurePosixPath
import platform
import tarfile
import urllib.request

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('recovery_files', ROOT/'ops/hetzner/recovery-files.py')
files = importlib.util.module_from_spec(spec); spec.loader.exec_module(files)
MAX_ARCHIVE = 30 * 1024**2


def install(archive, destination, configuration, platform_id):
    files.require(platform_id in configuration['platforms'] and len(archive) <= MAX_ARCHIVE)
    row = configuration['platforms'][platform_id]
    files.require(hashlib.sha256(archive).hexdigest() == row['archiveSha256'])
    binaries = {}
    with tarfile.open(fileobj=io.BytesIO(archive), mode='r:gz') as source:
        for name, expected in row['binaries'].items():
            files.require(name in ('age', 'age-keygen'))
            members = [m for m in source.getmembers() if m.name == 'age/'+name]
            files.require(len(members) == 1 and members[0].isfile() and 0 < members[0].size <= 50 * 1024**2)
            content = source.extractfile(members[0]).read()
            files.require(hashlib.sha256(content).hexdigest() == expected)
            binaries[name] = content
    files.require(set(binaries) == {'age','age-keygen'})
    target = PurePosixPath(destination)
    files.name_parts(target.name)
    with files.directory(str(target.parent), private=True) as parent:
        os.mkdir(target.name, 0o700, dir_fd=parent)
        with files.relative_directory(parent, [target.name]) as directory:
            for name, content in binaries.items():
                fd = os.open(name, os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW, 0o700, dir_fd=directory)
                with os.fdopen(fd,'wb') as output:
                    output.write(content); output.flush(); os.fsync(output.fileno())
            os.fsync(directory)
        os.fsync(parent)
    return {'version': configuration['version'], 'platform': platform_id,
            'archiveSha256': row['archiveSha256'], 'binaries': row['binaries']}


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--destination', required=True)
    parser.add_argument('--archive', help='Previously downloaded pinned archive; avoids network access')
    args=parser.parse_args()
    configuration=json.loads((ROOT/'ops/hetzner/recovery-tools.json').read_text())
    platform_id=platform.system().lower()+'-'+{'x86_64':'amd64'}.get(platform.machine(),'unsupported')
    files.require(platform_id in configuration['platforms'])
    if args.archive:
        with open(args.archive,'rb') as source: content=source.read(MAX_ARCHIVE+1)
    else:
        version=configuration['version']
        url='https://github.com/FiloSottile/age/releases/download/'+version+'/age-'+version+'-'+platform_id+'.tar.gz'
        with urllib.request.urlopen(url, timeout=30) as source: content=source.read(MAX_ARCHIVE+1)
    receipt=install(content,args.destination,configuration,platform_id)
    print(json.dumps(receipt,sort_keys=True))


if __name__=='__main__': main()
