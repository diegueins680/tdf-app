#!/usr/bin/env python3
"""Private file-tree capture/replay primitive; caller must fence all writers.

No deployment, encryption, remote transfer, or database consistency is implied.
Manifests contain private paths and must not be published in audit output.
"""
from contextlib import contextmanager
import hashlib
import ctypes
import sys
import os
from pathlib import PurePosixPath
import stat
import tarfile

MAX_BYTES = 2 * 1024**3
MAX_ENTRIES = 100_000
BLOCK = 1024**2


def require(condition):
    if not condition:
        raise ValueError('Recovery file boundary rejected')


@contextmanager
def directory(path, *, private=False):
    # Open each component, not just the leaf: no symlink ancestor may redirect IO.
    value = os.fspath(path)
    require(value.startswith('/') and '..' not in value.split('/'))
    fd = os.open('/', os.O_RDONLY | os.O_DIRECTORY)
    try:
        for part in value.split('/'):
            if not part:
                continue
            child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
            os.close(fd)
            fd = child
        info = os.fstat(fd)
        if private:
            require(info.st_uid == os.geteuid() and info.st_mode & 0o077 == 0)
        yield fd
    finally:
        os.close(fd)


def no_extended_attributes(fd):
    if hasattr(os, 'listxattr'):
        require(not os.listxattr(fd))
    elif sys.platform == 'darwin':
        # macOS Python omits the xattr API; query the already-open inode.
        library = ctypes.CDLL(None, use_errno=True)
        query = library.flistxattr
        query.argtypes = [ctypes.c_int, ctypes.c_void_p, ctypes.c_size_t, ctypes.c_int]
        query.restype = ctypes.c_ssize_t
        require(query(fd, None, 0, 0) == 0)
    else:
        require(False)  # Do not silently lose metadata on unsupported platforms.


def identity(info):
    return (info.st_dev, info.st_ino, info.st_mode, info.st_uid, info.st_gid,
            info.st_size, info.st_mtime_ns, info.st_ctime_ns, info.st_nlink)


def name_parts(name):
    require(isinstance(name, str) and name and len(name.encode('utf-8')) <= 4096
            and '\x00' not in name and '\\' not in name)
    parts = name.split('/')
    require(all(part not in ('', '.', '..') for part in parts))
    return parts


def metadata(info, name, kind):
    require(info.st_mode & 0o7000 == 0)
    require(0 <= info.st_uid < 2**31 and 0 <= info.st_gid < 2**31)
    return {'path': name, 'kind': kind, 'mode': stat.S_IMODE(info.st_mode),
            'uid': info.st_uid, 'gid': info.st_gid, 'mtimeNs': info.st_mtime_ns}


def digest(handle, expected_bytes):
    value, count = hashlib.sha256(), 0
    for block in iter(lambda: handle.read(BLOCK), b''):
        count += len(block)
        require(count <= expected_bytes)
        value.update(block)
    require(count == expected_bytes)
    return value.hexdigest()


def walk(fd, *, archive=None):
    result, size = [], 0
    device = os.fstat(fd).st_dev

    def visit(parent, name, relative):
        nonlocal size
        before = os.stat(name, dir_fd=parent, follow_symlinks=False)
        require(before.st_dev == device and not stat.S_ISLNK(before.st_mode))
        is_directory = stat.S_ISDIR(before.st_mode)
        require(is_directory or (stat.S_ISREG(before.st_mode) and before.st_nlink == 1))
        child = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK
                        | (os.O_DIRECTORY if is_directory else 0), dir_fd=parent)
        try:
            require(identity(os.fstat(child)) == identity(before))
            # This format does not silently discard ACL/xattr semantics.
            no_extended_attributes(child)
            row = metadata(before, relative, 'directory' if is_directory else 'file')
            require(len(result) < MAX_ENTRIES)
            if is_directory:
                result.append(row)
                if archive:
                    add(archive, row, None)
                for entry in sorted(os.listdir(child)):
                    name_parts(entry)
                    visit(child, entry, relative + '/' + entry if relative else entry)
            else:
                size += before.st_size
                require(size <= MAX_BYTES)
                with os.fdopen(os.dup(child), 'rb') as handle:
                    row.update(bytes=before.st_size, sha256=digest(handle, before.st_size))
                    result.append(row)
                    if archive:
                        handle.seek(0)
                        add(archive, row, handle)
            require(identity(os.fstat(child)) == identity(before)
                    and identity(os.stat(name, dir_fd=parent, follow_symlinks=False)) == identity(before))
        finally:
            os.close(child)

    # Root itself is captured as a distinguished empty path, never an archive member.
    root = os.fstat(fd)
    no_extended_attributes(fd)
    result.append(metadata(root, '', 'directory'))
    for entry in sorted(os.listdir(fd)):
        name_parts(entry)
        visit(fd, entry, entry)
    require(identity(os.fstat(fd)) == identity(root))
    return {'schemaVersion': 1, 'entries': result, 'bytes': size}


def add(archive, row, handle):
    item = tarfile.TarInfo(row['path'])
    item.type = tarfile.DIRTYPE if row['kind'] == 'directory' else tarfile.REGTYPE
    item.uid, item.gid, item.mode = row['uid'], row['gid'], row['mode']
    item.mtime = row['mtimeNs'] // 10**9
    item.size = row.get('bytes', 0)
    archive.addfile(item, handle)


def capture(source, destination):
    """Create an exclusive private tar; return a private content/metadata manifest.

    Failures retain partial evidence. No receipt means no successful capture.
    The caller must reject nested mounts and establish an actual writer fence;
    sampled file identities cannot prove absence of concurrent privileged writers.
    """
    destination = PurePosixPath(destination)
    name_parts(destination.name)
    with directory(source) as source_fd, directory(str(destination.parent), private=True) as parent:
        output = os.open(destination.name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         0o600, dir_fd=parent)
        with os.fdopen(output, 'wb') as handle:
            with tarfile.open(fileobj=handle, mode='w', format=tarfile.PAX_FORMAT) as archive:
                manifest = walk(source_fd, archive=archive)
                validate_manifest(manifest)
            handle.flush()
            os.fsync(handle.fileno())
        require(walk(source_fd) == manifest)
        os.fsync(parent)
    return manifest


def validate_manifest(manifest):
    require(set(manifest) == {'schemaVersion', 'entries', 'bytes'} and manifest['schemaVersion'] == 1)
    entries = manifest['entries']
    require(isinstance(entries, list) and 1 <= len(entries) <= MAX_ENTRIES)
    rows, total = {}, 0
    for index, row in enumerate(entries):
        kind, name = row.get('kind'), row.get('path')
        require(kind in ('file', 'directory'))
        require(set(row) == {'path', 'kind', 'mode', 'uid', 'gid', 'mtimeNs'}
                | ({'bytes', 'sha256'} if kind == 'file' else set()))
        if index == 0:
            require(name == '' and kind == 'directory')
        else:
            name_parts(name)
            parent = name.rpartition('/')[0]
            require(parent in rows and rows[parent]['kind'] == 'directory')
        require(name not in rows)
        require(type(row['mode']) is int and 0 <= row['mode'] <= 0o777)
        require(all(type(row[k]) is int and 0 <= row[k] < 2**31 for k in ('uid', 'gid')))
        require(type(row['mtimeNs']) is int and 0 <= row['mtimeNs'] < 2**63)
        if kind == 'file':
            require(type(row['bytes']) is int and 0 <= row['bytes'] <= MAX_BYTES)
            require(isinstance(row['sha256'], str) and len(row['sha256']) == 64
                    and all(c in '0123456789abcdef' for c in row['sha256']))
            total += row['bytes']
        rows[name] = row
    require(type(manifest['bytes']) is int and total == manifest['bytes'] and total <= MAX_BYTES)
    return rows


@contextmanager
def relative_directory(root, parts):
    fd = os.dup(root)
    try:
        for part in parts:
            child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
            os.close(fd)
            fd = child
        yield fd
    finally:
        os.close(fd)


def apply_metadata(fd, row):
    current = os.fstat(fd)
    if (current.st_uid, current.st_gid) != (row['uid'], row['gid']):
        require(os.geteuid() == 0)
        os.fchown(fd, row['uid'], row['gid'])
    os.fchmod(fd, row['mode'])
    os.utime(fd, ns=(row['mtimeNs'], row['mtimeNs']))
    os.fsync(fd)


def restore(archive_path, manifest, destination):
    """Replay only into a newly created child of a private owned parent.

    The manifest must be bound to separately trusted bundle evidence. This is not
    an authenticator for an attacker who can replace both archive and manifest.
    Failed partial targets are retained; never restore over existing content.
    """
    rows = validate_manifest(manifest)
    destination = PurePosixPath(destination)
    name_parts(destination.name)
    archive_path = PurePosixPath(archive_path)
    with directory(str(destination.parent), private=True) as parent, \
            directory(str(archive_path.parent), private=True) as archive_parent:
        archive_fd = os.open(archive_path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=archive_parent)
        with os.fdopen(archive_fd, 'rb') as handle:
            info = os.fstat(handle.fileno())
            require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1 and info.st_uid == os.geteuid()
                    and info.st_mode & 0o077 == 0 and info.st_size <= MAX_BYTES + MAX_ENTRIES * 8192)
            os.mkdir(destination.name, 0o700, dir_fd=parent)
            with relative_directory(parent, [destination.name]) as root:
                seen = {''}
                with tarfile.open(fileobj=handle, mode='r:') as archive:
                    for item in archive:
                        name_parts(item.name)
                        require(item.name in rows and item.name not in seen)
                        row = rows[item.name]
                        require((item.isdir() and row['kind'] == 'directory')
                                or (item.isreg() and row['kind'] == 'file'))
                        require(not item.linkname and not item.sparse
                                and item.mode == row['mode'] and item.uid == row['uid'] and item.gid == row['gid']
                                and item.mtime == row['mtimeNs'] // 10**9 and item.size == row.get('bytes', 0)
                                and set(item.pax_headers) <= {'path', 'uid', 'gid', 'mtime'})
                        parts = name_parts(item.name)
                        with relative_directory(root, parts[:-1]) as target_parent:
                            if row['kind'] == 'directory':
                                os.mkdir(parts[-1], 0o700, dir_fd=target_parent)
                            else:
                                output = os.open(parts[-1], os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                                 0o600, dir_fd=target_parent)
                                with os.fdopen(output, 'wb') as target, archive.extractfile(item) as source:
                                    value, written = hashlib.sha256(), 0
                                    for block in iter(lambda: source.read(BLOCK), b''):
                                        written += len(block)
                                        require(written <= row['bytes'])
                                        value.update(block)
                                        target.write(block)
                                    require(written == row['bytes'] and value.hexdigest() == row['sha256'])
                                    target.flush()
                                    apply_metadata(target.fileno(), row)
                        seen.add(item.name)
                require(seen == set(rows) and identity(os.fstat(handle.fileno())) == identity(info))
                for row in reversed(manifest['entries']):
                    if row['kind'] == 'directory':
                        with relative_directory(root, name_parts(row['path']) if row['path'] else []) as fd:
                            apply_metadata(fd, row)
                require(walk(root) == manifest)
                os.fsync(parent)
    return {'status': 'file-tree-restored', 'entries': len(rows), 'bytes': manifest['bytes'],
            'coordinatedDatabase': False, 'offHost': False}
