#!/usr/bin/env python3
"""Pinned age encryption/recovery of an already coordinated private bundle.

The caller establishes snapshot coherence, sender provenance, key custody and
transfer ownership. This module neither transfers files nor creates keys.
"""
from contextlib import contextmanager
import hashlib
import fcntl
import importlib.util
import json
import os
from pathlib import Path, PurePosixPath
import platform
import re
import resource
import stat
import subprocess

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('recovery_files', HERE/'recovery-files.py')
files = importlib.util.module_from_spec(spec)
spec.loader.exec_module(files)
require = files.require
MAX_PLAINTEXT = 4 * 1024**3
MAX_CIPHERTEXT = MAX_PLAINTEXT + 4 * 1024**2
ENVIRONMENT = {'PATH': '/usr/bin:/bin', 'LC_ALL': 'C'}


def hash_handle(handle, size):
    handle.seek(0)
    result = files.digest(handle, size)
    handle.seek(0)
    return result


@contextmanager
def private_file(path, maximum):
    path = PurePosixPath(path)
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
        with os.fdopen(fd, 'rb') as handle:
            before = os.fstat(handle.fileno())
            require(stat.S_ISREG(before.st_mode) and before.st_nlink == 1
                    and before.st_uid == os.geteuid() and before.st_mode & 0o077 == 0
                    and 0 < before.st_size <= maximum)
            yield handle, before
            require(files.identity(os.fstat(handle.fileno())) == files.identity(before))


@contextmanager
def verified_executable(path):
    # Linux memfd seals bind execution to immutable verified bytes. A held ordinary
    # file descriptor alone still permits another writer to modify its inode.
    require(platform.system() == 'Linux' and hasattr(os, 'memfd_create'))
    key = 'linux-' + {'x86_64': 'amd64'}.get(platform.machine(), 'unsupported')
    tools = json.loads((HERE/'recovery-tools.json').read_text())
    require(key in tools['platforms'])
    expected = tools['platforms'][key]['binaries']['age']
    fd = os.memfd_create('tdf-recovery-age', os.MFD_CLOEXEC | os.MFD_ALLOW_SEALING)
    try:
        with private_file(path, 50 * 1024**2) as (handle, info):
            require(info.st_mode & 0o111)
            count, value = 0, hashlib.sha256()
            for block in iter(lambda: handle.read(files.BLOCK), b''):
                count += len(block); require(count <= info.st_size)
                value.update(block)
                view = memoryview(block)
                while view:
                    written = os.write(fd, view); require(written > 0); view = view[written:]
            require(count == info.st_size and value.hexdigest() == expected)
        seals = fcntl.F_SEAL_WRITE | fcntl.F_SEAL_GROW | fcntl.F_SEAL_SHRINK | fcntl.F_SEAL_SEAL
        fcntl.fcntl(fd, fcntl.F_ADD_SEALS, seals)
        require(fcntl.fcntl(fd, fcntl.F_GET_SEALS) == seals)
        os.lseek(fd, 0, os.SEEK_SET)
        with os.fdopen(os.dup(fd), 'rb') as immutable:
            require(hash_handle(immutable, count) == expected)
        yield fd, expected
    finally:
        os.close(fd)


def process(executable_fd, arguments, source, destination, *, descriptors=()):
    path = PurePosixPath(destination)
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_RDWR | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=parent)
        with os.fdopen(fd, 'w+b') as output:
            def limit():
                resource.setrlimit(resource.RLIMIT_FSIZE, (MAX_CIPHERTEXT, MAX_CIPHERTEXT))
            result = subprocess.run(['/proc/self/fd/'+str(executable_fd), *arguments], stdin=source, stdout=output,
                stderr=subprocess.DEVNULL, env=ENVIRONMENT, timeout=120, pass_fds=(executable_fd, *descriptors), preexec_fn=limit)
            require(result.returncode == 0)
            output.flush(); os.fsync(output.fileno())
            size = os.fstat(output.fileno()).st_size
            require(0 < size <= MAX_CIPHERTEXT)
            sha = hash_handle(output, size)
            os.fsync(parent)
            return {'sha256': sha, 'bytes': size}


def encrypt(binary, source, destination, recipient):
    require(isinstance(recipient, str) and re.fullmatch(r'age1[0-9a-z]{58}', recipient))
    with verified_executable(binary) as (executable, binary_hash), private_file(source, MAX_PLAINTEXT) as (handle, info):
        plain = {'sha256': hash_handle(handle, info.st_size), 'bytes': info.st_size}
        cipher = process(executable, ['--encrypt', '--recipient', recipient], handle, destination)
        require(hash_handle(handle, info.st_size) == plain['sha256'])
    return {'schemaVersion': 1, 'status': 'encrypted-not-transferred', 'plaintext': plain,
            'ciphertext': cipher, 'recipientSha256': hashlib.sha256(recipient.encode()).hexdigest(),
            'toolSha256': binary_hash, 'offHost': False, 'restored': False}


def decrypt(binary, source, destination, identity, expected):
    # age authenticates ciphertext but public-key encryption does not authenticate
    # a sender. The caller must bind expected hashes to its trusted capture record.
    require(set(expected) == {'plaintext', 'ciphertext'})
    for key, maximum in [('plaintext', MAX_PLAINTEXT), ('ciphertext', MAX_CIPHERTEXT)]:
        row = expected[key]
        require(set(row) == {'sha256', 'bytes'} and type(row['bytes']) is int and 0 < row['bytes'] <= maximum
                and isinstance(row['sha256'], str) and re.fullmatch(r'[a-f0-9]{64}', row['sha256']))
    with verified_executable(binary) as (executable, binary_hash), \
            private_file(identity, 1024) as (key_handle, _), private_file(source, MAX_CIPHERTEXT) as (handle, info):
        lines = [line for line in key_handle.read().decode('ascii').splitlines() if line and not line.startswith('#')]
        require(len(lines) == 1 and re.fullmatch(r'AGE-SECRET-KEY-1[0-9A-Z]{58}', lines[0]))
        key_handle.seek(0)
        require(info.st_size == expected['ciphertext']['bytes']
                and hash_handle(handle, info.st_size) == expected['ciphertext']['sha256'])
        plain = process(executable, ['--decrypt', '--identity', '/dev/fd/'+str(key_handle.fileno())],
                        handle, destination, descriptors=(key_handle.fileno(),))
        require(plain == expected['plaintext'])
        require(hash_handle(handle, info.st_size) == expected['ciphertext']['sha256'])
    return {'schemaVersion': 1, 'status': 'decrypted-content-verified', 'plaintext': plain,
            'ciphertext': expected['ciphertext'], 'toolSha256': binary_hash,
            'databaseRestored': False, 'filesRestored': False, 'offHost': False}
