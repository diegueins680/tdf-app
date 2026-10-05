#!/usr/bin/env python3
"""Pin an admitted live container root, then capture legacy uploads after stop.

This helper does not stop a container. The coordinator owns writer fencing,
canonical source authorization, a durable journal and image/mount admission.
"""
from contextlib import contextmanager
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import subprocess
import sys

_spec = importlib.util.spec_from_file_location('stopped_files', Path(__file__).with_name('recovery-files.py'))
files = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(files)
DOCKER = ['docker', '--host', 'unix:///var/run/docker.sock']
ENVIRONMENT = {'PATH': '/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8'}
MOUNT_PROBE = r'''
import os,sys
assert hasattr(os,'setns')
proc = os.open('/proc/self',os.O_RDONLY|os.O_DIRECTORY)
try:
 os.setns(int(sys.argv[1]),0)
 os.fchdir(int(sys.argv[2])); os.chroot('.'); os.chdir('/')
 fd = os.open('mountinfo',os.O_RDONLY,dir_fd=proc)
 try:
  with os.fdopen(fd,'rb',closefd=False) as source: data=source.read(4*1024*1024+1)
  assert len(data)<=4*1024*1024
  sys.stdout.buffer.write(data)
 finally: os.close(fd)
finally: os.close(proc)
'''


def require(value):
    if not value: raise ValueError('Stopped application storage boundary rejected')


def inspection(target):
    result = subprocess.run(DOCKER+['inspect', target], env=ENVIRONMENT, capture_output=True,
                            text=True, timeout=15)
    require(result.returncode == 0 and len(result.stdout) <= 1024**2)
    rows = json.loads(result.stdout)
    require(len(rows) == 1)
    return rows[0]


def admit_legacy_mounts(mounts):
    require(isinstance(mounts, list))
    for mount in mounts:
        value = mount.get('Destination')
        require(isinstance(value, str) and value.startswith('/'))
        path = Path(value)
        require(str(path) == value and '..' not in path.parts)
        target = Path('/app/uploads')
        require(path != target and path not in target.parents and target not in path.parents)


def admit_mountinfo(text):
    require(isinstance(text,str) and 0 < len(text) <= 4*1024*1024)
    root_seen = False
    for line in text.splitlines():
        fields = line.split()
        require(len(fields) >= 10 and '-' in fields[6:])
        value = re.sub(r'\\([0-7]{3})',lambda match:chr(int(match[1],8)),fields[4])
        require('\\' not in value)
        if value == '/':
            require(not root_seen);root_seen = True
        else: admit_legacy_mounts([{'Destination':value}])
    require(root_seen)


def verify_namespace(namespace_fd, root_fd):
    # Open our host proc task directory before setns in a separate process. A
    # container procfs may use another PID namespace and cannot locate that task
    # through /proc/self after setns. No namespace changes occur in the caller.
    result = subprocess.run([sys.executable,'-c',MOUNT_PROBE,str(namespace_fd),str(root_fd)],
        env=ENVIRONMENT,pass_fds=(namespace_fd,root_fd),capture_output=True,text=True,timeout=10)
    require(result.returncode == 0)
    admit_mountinfo(result.stdout)


class RetainedRoot:
    def __init__(self, target, image, image_id):
        require(isinstance(target, str) and re.fullmatch('[a-f0-9]{64}', target))
        require(isinstance(image, str) and re.fullmatch(r'[a-zA-Z0-9./_-]+@sha256:[a-f0-9]{64}', image))
        require(isinstance(image_id, str) and re.fullmatch(r'sha256:[a-f0-9]{64}', image_id))
        self.target, self.image, self.image_id = target, image, image_id
        self.root_fd = self.namespace_fd = self.pid_fd = None
        self.start_time = self.mounts = self.pid = None
        self.owner_pid = None

    def inspect(self, *, running):
        value = inspection(self.target)
        require(value['Id'] == self.target and value['Config']['Image'] == self.image
                and value['Image'] == self.image_id)
        state = value['State']
        require(state['Running'] is running and all(state[key] is False
                for key in ('Restarting', 'Paused', 'Dead', 'OOMKilled')))
        require(isinstance(state['StartedAt'], str) and re.fullmatch(
                r'[1-9][0-9]{3}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(?:\.[0-9]{1,9})?Z', state['StartedAt']))
        if self.start_time is not None:
            require(state['StartedAt'] == self.start_time and value['Mounts'] == self.mounts)
        admit_legacy_mounts(value['Mounts'])
        if running:
            require(type(state['Pid']) is int and state['Pid'] > 0)
            if self.pid is not None: require(state['Pid'] == self.pid)
        else:
            require(type(state['Pid']) is int and state['Pid'] == 0 and state['Status'] == 'exited'
                    and type(state['ExitCode']) is int and state['ExitCode'] in (0, 143))
        return value

    def guard(self):
        require(self.owner_pid == os.getpid() and self.root_fd is not None
                and self.namespace_fd is not None and self.pid_fd is not None)

    @contextmanager
    def pinned(self):
        require(sys.platform == 'linux' and os.geteuid() == 0 and hasattr(os, 'pidfd_open'))
        require(self.owner_pid is None and self.root_fd is None and self.start_time is None)
        value = self.inspect(running=True)
        self.pid = value['State']['Pid']; self.start_time = value['State']['StartedAt']; self.mounts = value['Mounts']
        try:
            self.pid_fd = os.pidfd_open(self.pid)
            base = '/proc/'+str(self.pid)
            # Bind the observed process to this exact Docker cgroup before
            # retaining its namespace/root. This is a rootful Docker boundary.
            groups = Path(base+'/cgroup').read_text().splitlines()
            require(any(line.split(':', 2)[-1].endswith('/docker-'+self.target+'.scope')
                        or line.split(':', 2)[-1].endswith('/docker/'+self.target) for line in groups))
            self.namespace_fd = os.open(base+'/ns/mnt', os.O_RDONLY)
            # This kernel magic link is intentional; arbitrary source path
            # symlinks are never admitted by the recovery file primitives.
            self.root_fd = os.open(base+'/root', os.O_RDONLY | os.O_DIRECTORY)
            self.inspect(running=True)
            require((os.stat(base+'/ns/mnt').st_dev, os.stat(base+'/ns/mnt').st_ino)
                    == (os.fstat(self.namespace_fd).st_dev, os.fstat(self.namespace_fd).st_ino))
            require((os.stat(base+'/root').st_dev, os.stat(base+'/root').st_ino)
                    == (os.fstat(self.root_fd).st_dev, os.fstat(self.root_fd).st_ino))
            verify_namespace(self.namespace_fd,self.root_fd)
            require(not select.select([self.pid_fd],[],[],0)[0])
            self.inspect(running=True)
            self.owner_pid = os.getpid()
            yield self
        finally:
            self.owner_pid = None
            for name in ('root_fd', 'namespace_fd', 'pid_fd'):
                fd = getattr(self, name)
                if fd is not None: os.close(fd); setattr(self, name, None)

    def capture_uploads(self, destination):
        """Full actual inode metadata, never Docker cp/export metadata guesses."""
        self.guard(); self.inspect(running=False); verify_namespace(self.namespace_fd,self.root_fd)
        fd = os.dup(self.root_fd)
        present, manifest = True, None
        try:
            for component in ('app', 'uploads'):
                try: child = os.open(component, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
                except FileNotFoundError:
                    present = False; break
                os.close(fd); fd = child
            if present:
                manifest = files.capture_directory_fd(fd, destination)
        finally: os.close(fd)
        self.guard(); self.inspect(running=False); verify_namespace(self.namespace_fd,self.root_fd)
        return {'sourceContainer': self.target, 'sourceImage': self.image,
                'sourceImageId': self.image_id, 'sourceStartedAt': self.start_time,
                'presence': 'present' if present else 'absent', 'manifest': manifest,
                'productionContainerStoppedByHelper': False}
