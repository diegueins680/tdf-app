#!/usr/bin/env python3
"""Bounded encrypted-bundle retention/retrieval over an authenticated byte channel.

The caller pins both hosts and supplies the trusted encryption receipt. This
module accepts no network address, command, plaintext, key or cleanup request.
"""
from contextlib import contextmanager
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import stat
import struct
import time

_spec = importlib.util.spec_from_file_location('transfer_files', Path(__file__).with_name('recovery-files.py'))
files = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(files)
MAX_BYTES = 4 * 1024**3 + 4 * 1024**2
BLOCK = 65536
HEADER_LIMIT = 2048


def require(value):
    if not value: raise ValueError('Recovery transfer boundary rejected')


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'))+'\n').encode()


def binding(nonce, expected):
    require(isinstance(nonce, str) and re.fullmatch('[a-f0-9]{32}', nonce))
    require(isinstance(expected, dict) and set(expected) == {'bytes', 'sha256'})
    require(type(expected['bytes']) is int and 0 < expected['bytes'] <= MAX_BYTES)
    require(isinstance(expected['sha256'], str) and re.fullmatch('[a-f0-9]{64}', expected['sha256']))
    return {'schemaVersion': 1, 'nonce': nonce, **expected}


class Channel:
    """Exclusive use of caller-owned pipe/socket FDs, under one absolute deadline."""
    def __init__(self, reader, writer, timeout=180):
        require(type(timeout) in (int, float) and 0 < timeout <= 300)
        self.reader, self.writer = reader, writer
        self.deadline = time.monotonic() + timeout
        self.flags = {}
        self.closed = True

    def __enter__(self):
        require(self.closed and not self.flags)
        try:
            for fd in {self.reader, self.writer}:
                require(type(fd) is int and fd >= 0)
                info = os.fstat(fd)
                require(stat.S_ISFIFO(info.st_mode) or stat.S_ISSOCK(info.st_mode))
                self.flags[fd] = os.get_blocking(fd)
                os.set_blocking(fd, False)
            self.closed = False
            return self
        except BaseException:
            self.__exit__(None, None, None)
            raise

    def __exit__(self, *_):
        self.closed = True
        for fd, blocking in self.flags.items(): os.set_blocking(fd, blocking)
        self.flags.clear()

    def wait(self, writing):
        require(not self.closed)
        remaining = self.deadline - time.monotonic()
        if remaining <= 0: raise TimeoutError('Recovery transfer deadline exceeded')
        readable, writable, _ = select.select([] if writing else [self.reader],
                                              [self.writer] if writing else [], [], remaining)
        if not (writable if writing else readable):
            raise TimeoutError('Recovery transfer deadline exceeded')

    def read(self, count):
        require(type(count) is int and 0 <= count <= BLOCK)
        value = bytearray()
        while len(value) < count:
            self.wait(False)
            try: data = os.read(self.reader, count-len(value))
            except (BlockingIOError, InterruptedError): continue
            require(data)
            value.extend(data)
        return bytes(value)

    def write(self, data):
        require(isinstance(data, bytes) and len(data) <= BLOCK)
        view = memoryview(data)
        while view:
            self.wait(True)
            try: count = os.write(self.writer, view)
            except (BlockingIOError, InterruptedError): continue
            require(count > 0); view = view[count:]

    def send_header(self, value):
        data = canonical(value)
        require(0 < len(data) <= HEADER_LIMIT)
        self.write(struct.pack('!I', len(data))+data)

    def receive_header(self, expected):
        size = struct.unpack('!I', self.read(4))[0]
        require(0 < size <= HEADER_LIMIT)
        raw = self.read(size)
        # Byte correspondence also excludes Python's True == 1 and 1.0 == 1.
        require(raw == canonical(expected))


def file_identity(fd, expected):
    info = os.fstat(fd)
    require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1
            and info.st_uid == os.geteuid() and stat.S_IMODE(info.st_mode) == 0o600
            and info.st_size == expected['bytes'])
    return info


def send(channel, source, nonce, expected, kind):
    require(kind in ('offer', 'retrieval'))
    header = {**binding(nonce, expected), 'kind': kind}
    path = Path(source)
    files.name_parts(path.name)
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
        try:
            before = file_identity(fd, expected)
            # Verify before advertising this input, then verify the transmitted
            # bytes and inode again. A changed stream cannot receive completion.
            with os.fdopen(os.dup(fd), 'rb') as handle:
                require(files.digest(handle, before.st_size) == expected['sha256'])
            os.lseek(fd, 0, os.SEEK_SET)
            require(files.identity(os.fstat(fd)) == files.identity(before))
            channel.send_header(header)
            count, sha = 0, hashlib.sha256()
            while count < before.st_size:
                data = os.read(fd, min(BLOCK, before.st_size-count)); require(data)
                channel.write(data); sha.update(data); count += len(data)
            require(sha.hexdigest() == expected['sha256'] and count == before.st_size)
            require(files.identity(os.fstat(fd)) == files.identity(before)
                    and files.identity(os.stat(path.name, dir_fd=parent, follow_symlinks=False)) == files.identity(before))
        finally: os.close(fd)


def receive(channel, destination, nonce, expected, kind):
    require(kind in ('offer', 'retrieval'))
    channel.receive_header({**binding(nonce, expected), 'kind': kind})
    path = Path(destination); files.name_parts(path.name)
    with files.directory(str(path.parent), private=True) as parent:
        fd = os.open(path.name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=parent)
        try:
            count, sha = 0, hashlib.sha256()
            while count < expected['bytes']:
                data = channel.read(min(BLOCK, expected['bytes']-count))
                view = memoryview(data)
                while view:
                    written = os.write(fd, view); require(written > 0); view = view[written:]
                sha.update(data); count += len(data)
            require(count == expected['bytes'] and sha.hexdigest() == expected['sha256'])
            os.fsync(fd)
            captured = file_identity(fd, expected)
            os.fsync(parent)
            # Reopen the retained name; a cached streaming hash alone is not
            # evidence that the persisted artifact can be read back correctly.
            reread = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
            try:
                require(files.identity(os.fstat(reread)) == files.identity(captured))
                with os.fdopen(os.dup(reread), 'rb') as handle:
                    require(files.digest(handle, expected['bytes']) == expected['sha256'])
                require(files.identity(os.fstat(reread)) == files.identity(captured)
                        and files.identity(os.stat(path.name, dir_fd=parent, follow_symlinks=False)) == files.identity(captured))
            finally: os.close(reread)
        finally: os.close(fd)


def retain_and_return(channel, destination, nonce, expected):
    """Receiver persists and reopens the ciphertext before returning its bytes."""
    receive(channel, destination, nonce, expected, 'offer')
    send(channel, destination, nonce, expected, 'retrieval')
    channel.receive_header({**binding(nonce, expected), 'kind': 'retrieved'})
    return {'status': 'ciphertext-retained-and-returned', **binding(nonce, expected),
            'decrypted': False, 'restored': False}


def round_trip(channel, source, retrieved, nonce, expected):
    """Sender binds retrieved bytes to its original trusted capture envelope."""
    send(channel, source, nonce, expected, 'offer')
    receive(channel, retrieved, nonce, expected, 'retrieval')
    channel.send_header({**binding(nonce, expected), 'kind': 'retrieved'})
    return {'status': 'ciphertext-round-trip-verified', **binding(nonce, expected),
            'decrypted': False, 'restored': False}
