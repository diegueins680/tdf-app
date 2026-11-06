#!/usr/bin/env python3
"""Install pinned CVC5 and build pinned Ethos/CPC signatures outside the checkout."""
import argparse
import hashlib
import io
import json
import os
from pathlib import Path
import platform
import subprocess
import tarfile
import urllib.request
import zipfile

ROOT = Path(__file__).resolve().parent.parent
CONFIG = ROOT/'formal/system/certificate-tools.json'


def sha(data):
    return hashlib.sha256(data).hexdigest()


def download(row, destination):
    with urllib.request.urlopen(row['url'], timeout=60) as source:
        content = source.read(100 * 1024**2 + 1)
    if len(content) > 100 * 1024**2 or sha(content) != row['sha256']:
        raise ValueError('Archive size/checksum mismatch: ' + row['url'])
    destination.write_bytes(content)
    return content


def unpack_tar(content, destination):
    destination.mkdir()
    with tarfile.open(fileobj=io.BytesIO(content), mode='r:gz') as archive:
        # Source archives have one enclosing directory. Refuse links/devices and
        # traversal before extraction, including links that could escape later.
        members = archive.getmembers()
        roots = {Path(m.name).parts[0] for m in members}
        if len(roots) != 1 or any(not (m.isfile() or m.isdir()) or
                                Path(m.name).is_absolute() or '..' in Path(m.name).parts
                                for m in members):
            raise ValueError('Unsupported source archive members')
        for member in members:
            parts = Path(member.name).parts[1:]
            if parts:
                member.name = str(Path(*parts))
                archive.extract(member, destination, filter='data')


def tree_hashes(directory):
    return {str(p.relative_to(directory)): sha(p.read_bytes())
            for p in sorted(directory.rglob('*')) if p.is_file()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    out = args.output.resolve()
    if out.is_relative_to(ROOT):
        raise ValueError('Tool installation must be outside the checkout')
    out.mkdir(mode=0o700)
    config = json.loads(CONFIG.read_text())
    platform_id = platform.system().lower() + '-' + platform.machine()
    row = config['platforms'][platform_id]
    asset = row['asset']
    binary_archive = download({**row, 'url':
        f"https://github.com/cvc5/cvc5/releases/download/cvc5-{config['cvc5Version']}/{asset}.zip"}, out/'cvc5.zip')
    with zipfile.ZipFile(io.BytesIO(binary_archive)) as archive:
        binary = archive.read(asset+'/bin/cvc5')
    (out/'cvc5').write_bytes(binary)
    (out/'cvc5').chmod(0o700)
    for key, directory in (('cvc5Source', 'cvc5-source'), ('ethosSource', 'ethos-source')):
        unpack_tar(download(config[key], out/(directory+'.tar.gz')), out/directory)
    with (out/'build.log').open('w') as log:
        command = ['cmake', '-S', str(out/'ethos-source'), '-B', str(out/'build')]
        # Avoid an interactive Xcode SDK discovery subprocess on macOS. An
        # explicitly supplied SDKROOT/CMAKE_TAPI also works for local installs.
        if platform.system() == 'Darwin':
            sdk = os.environ.get('SDKROOT', '/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk')
            tapi = os.environ.get('CMAKE_TAPI', '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/tapi')
            command += ['-DCMAKE_OSX_SYSROOT='+sdk, '-DCMAKE_TAPI='+tapi]
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
        subprocess.run(['cmake', '--build', str(out/'build'), '-j', '2'],
                       stdout=log, stderr=subprocess.STDOUT, check=True, timeout=300)
    (out/'ethos').write_bytes((out/'build/src/ethos').read_bytes())
    (out/'ethos').chmod(0o700)
    cache = (out/'build/CMakeCache.txt').read_text().splitlines()
    dependencies = {}
    for name in ('CMAKE_CXX_COMPILER', 'GMP_LIBRARIES', 'GMP_INCLUDE_DIR'):
        value = next(line.split('=', 1)[1] for line in cache if line.startswith(name+':'))
        path = Path(value)/'gmp.h' if name == 'GMP_INCLUDE_DIR' else Path(value)
        dependencies[name] = {'path': str(path), 'sha256': sha(path.read_bytes())}
    receipt = {'configurationSha256': sha(CONFIG.read_bytes()), 'platform': platform_id,
               'binaries': {name: sha((out/name).read_bytes()) for name in ('cvc5', 'ethos')},
               'signatures': tree_hashes(out/'cvc5-source/proofs/eo/cpc'),
               'versions': {name: subprocess.check_output([str(out/name), option], text=True)
                            for name, option in (('cvc5', '--version'), ('ethos', '--show-config'))},
               'host': {'python': platform.python_version(), 'platform': platform.platform(),
                        'buildDependencies': dependencies,
                        'cmake': subprocess.check_output(['cmake', '--version'], text=True),
                        'compiler': subprocess.check_output([os.environ.get('CXX', 'c++'), '--version'], text=True)},
               'limitations': 'Compiler, system GMP, operating system and hardware remain trusted; archive pins are not a reproducible-build proof.'}
    (out/'installation.json').write_text(json.dumps(receipt, indent=2)+'\n')
    print(json.dumps({'installed': str(out), 'configurationSha256': receipt['configurationSha256']}))


if __name__ == '__main__':
    main()
