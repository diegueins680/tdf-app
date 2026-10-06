#!/usr/bin/env python3
"""Opt-in synthetic ciphertext-sized byte round trip over the canonical SSH host.

Uses a sandboxed transient DynamicUser with no network or production-file access.
Local fixtures persist privately for inspection. No production archive or key is
read, generated, transferred, deleted or claimed recoverable by this command.
"""
import argparse
import base64
import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shlex
import subprocess
import uuid

ROOT = Path(__file__).resolve().parent.parent
SOURCE_PATHS = ['ops/hetzner/recovery-files.py', 'ops/hetzner/recovery-transfer.py']
spec = importlib.util.spec_from_file_location('transfer', ROOT/SOURCE_PATHS[1])
t = importlib.util.module_from_spec(spec); spec.loader.exec_module(t)
CONTENT = bytes(range(256))*16384
EXPECTED = {'bytes':len(CONTENT), 'sha256':hashlib.sha256(CONTENT).hexdigest()}
REMOTE = r'''
import base64,hashlib,importlib.util,json,os,pathlib,sys,tempfile
os.umask(0o077)
raw=sys.stdin.buffer.readline(1024*1024+1)
assert raw.endswith(b'\n') and len(raw)<=1024*1024
payload=json.loads(raw)
assert set(payload)=={'sources','nonce','expected'}
assert set(payload['sources'])=={'ops/hetzner/recovery-files.py','ops/hetzner/recovery-transfer.py'}
with tempfile.TemporaryDirectory(prefix='tdf-transfer-test-') as temporary:
 root=pathlib.Path(temporary)
 for name,encoded in payload['sources'].items():
  path=root/name;path.parent.mkdir(parents=True,exist_ok=True,mode=0o700)
  path.write_bytes(base64.b64decode(encoded,validate=True));path.chmod(0o600)
 spec=importlib.util.spec_from_file_location('transfer',root/'ops/hetzner/recovery-transfer.py')
 transfer=importlib.util.module_from_spec(spec);spec.loader.exec_module(transfer)
 content=bytes(range(256))*16384
 assert payload['expected']=={'bytes':len(content),'sha256':hashlib.sha256(content).hexdigest()}
 source=root/'synthetic-bytes';source.write_bytes(content);source.chmod(0o600)
 with transfer.Channel(sys.stdin.fileno(),sys.stdout.fileno(),60) as channel:
  transfer.round_trip(channel,source,root/'retrieved-bytes',payload['nonce'],payload['expected'])
'''


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--identity-file',required=True)
    parser.add_argument('--output-directory',required=True)
    args=parser.parse_args()
    directory=Path(args.output_directory).absolute()
    # Caller supplies an existing private directory; every output is exclusive.
    with t.files.directory(str(directory),private=True): pass
    nonce=uuid.uuid4().hex
    sources={name:(ROOT/name).read_bytes() for name in SOURCE_PATHS}
    own_source=Path(__file__).read_bytes()
    revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
    dirty=bool(subprocess.check_output(['git','status','--porcelain'],cwd=ROOT,text=True).strip())
    remote=['systemd-run','--pipe','--wait','--collect','--quiet',
            '--property=DynamicUser=yes','--property=PrivateNetwork=yes','--property=PrivateTmp=yes',
            '--property=ProtectSystem=strict','--property=ProtectHome=yes','--property=NoNewPrivileges=yes',
            '--property=MemoryMax=128M','--property=TasksMax=16','--property=RuntimeMaxSec=90',
            '--','python3','-c',REMOTE]
    command=['ssh','-F','/dev/null','-T','-o','BatchMode=yes','-o','IdentitiesOnly=yes',
             '-o','StrictHostKeyChecking=yes','-o','ConnectTimeout=10',
             '-i',str(Path(args.identity_file).absolute()),'root@178.105.93.101',shlex.join(remote)]
    started=datetime.datetime.now(datetime.timezone.utc).isoformat()
    # Diagnostics never mix into framed stdout; errors produce no success receipt.
    process=subprocess.Popen(command,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.DEVNULL,
                             bufsize=0)
    try:
        payload={'sources':{name:base64.b64encode(data).decode() for name,data in sources.items()},
                 'nonce':nonce,'expected':EXPECTED}
        data=t.canonical(payload)
        t.require(len(data)<1024*1024)
        with t.Channel(process.stdout.fileno(),process.stdin.fileno(),75) as channel:
            for offset in range(0,len(data),t.BLOCK):channel.write(data[offset:offset+t.BLOCK])
            result=t.retain_and_return(channel,directory/('synthetic-'+nonce+'.bin'),nonce,EXPECTED)
        process.stdin.close()
        t.require(process.wait(timeout=20)==0)
        t.require(all((ROOT/name).read_bytes()==data for name,data in sources.items())
                  and Path(__file__).read_bytes()==own_source)
        receipt={'revision':revision,'sourceWorktreeDirty':dirty,'sourceUnchanged':True,
                 'startedAt':started,'completedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),
                 'sourceHashes':{name:hashlib.sha256(data).hexdigest() for name,data in sources.items()},
                 'runnerSha256':hashlib.sha256(own_source).hexdigest(),'result':result,
                 'transport':'Strict existing SSH trust; canonical host; transient DynamicUser sandbox',
                 'scope':'Synthetic byte transfer only; no production data, encryption key custody or deployment',
                 'productionDataAccessed':False}
        with (directory/('transfer-'+nonce+'.json')).open('x') as output:
            os.fchmod(output.fileno(),0o600);json.dump(receipt,output,indent=2);output.write('\n')
            output.flush();os.fsync(output.fileno())
        with t.files.directory(str(directory),private=True) as parent:os.fsync(parent)
        print(json.dumps(receipt))
    finally:
        if process.poll() is None:
            process.terminate()
            try:process.wait(timeout=5)
            except subprocess.TimeoutExpired:process.kill();process.wait(timeout=5)
        for handle in (process.stdin,process.stdout):
            if handle and not handle.closed:handle.close()


if __name__=='__main__': main()
