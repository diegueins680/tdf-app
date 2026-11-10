#!/usr/bin/env python3
"""Opt-in synthetic bundle -> encryption -> off-host retrieval -> exact file replay.

The remote is a DynamicUser sandbox with private network/tmp and read-only system.
Only generated synthetic files and a temporary synthetic age identity are used.
This does not establish production backup coherence, key custody or DB recovery.
"""
import argparse
import base64
import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import re
import shlex
import struct
import subprocess
import tempfile
import uuid

ROOT=Path(__file__).resolve().parent.parent
SOURCES=['ops/hetzner/recovery-files.py','ops/hetzner/recovery-transfer.py',
         'ops/hetzner/recovery-envelope.py','ops/hetzner/recovery-tools.json',
         'ops/hetzner/coordinated-recovery-bundle.py']
spec=importlib.util.spec_from_file_location('transfer',ROOT/SOURCES[1])
t=importlib.util.module_from_spec(spec);spec.loader.exec_module(t)
MAX_PAYLOAD=16*1024**2
REMOTE=r'''
import base64,hashlib,importlib.util,json,os,pathlib,struct,sys,tempfile
os.umask(0o077)
raw=sys.stdin.buffer.readline(16*1024**2+1)
assert raw.endswith(b'\n') and len(raw)<=16*1024**2
payload=json.loads(raw)
assert set(payload)=={'sources','syntheticIdentity','recipient','binding'}
names={'ops/hetzner/recovery-files.py','ops/hetzner/recovery-transfer.py',
       'ops/hetzner/recovery-envelope.py','ops/hetzner/recovery-tools.json',
       'ops/hetzner/coordinated-recovery-bundle.py','tools/age'}
assert set(payload['sources'])==names
with tempfile.TemporaryDirectory(prefix='tdf-synthetic-bundle-') as temporary:
 root=pathlib.Path(temporary)
 for name,value in payload['sources'].items():
  path=root/name;path.parent.mkdir(parents=True,exist_ok=True,mode=0o700)
  path.write_bytes(base64.b64decode(value,validate=True));path.chmod(0o700 if name=='tools/age' else 0o600)
 def load(name,path):
  spec=importlib.util.spec_from_file_location(name,root/path)
  module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module
 t=load('transfer','ops/hetzner/recovery-transfer.py')
 b=load('bundle','ops/hetzner/coordinated-recovery-bundle.py')
 e=load('envelope','ops/hetzner/recovery-envelope.py')
 key=root/'synthetic-identity';key.write_bytes(base64.b64decode(payload['syntheticIdentity'],validate=True));key.chmod(0o600)
 sources={}
 for i,name in enumerate(b.ROLES):
  path=root/name;path.mkdir(mode=0o700)
  (path/'sentinel').write_bytes(bytes(range(256))*(1024+i));(path/'sentinel').chmod(0o600)
  (path/'empty').mkdir(mode=0o700);sources[name]=str(path)
 (root/'production'/'synthetic-secret').write_bytes(b'SYNTHETIC ONLY - recovery integration secret')
 (root/'production'/'synthetic-secret').chmod(0o600)
 binding=payload['binding'];b.validate_binding(binding);nonce=binding['releaseNonce']
 bundle=b.capture(sources,str(root/'capture'),str(root/'plain.tar'),binding)
 encrypted=e.encrypt(str(root/'tools/age'),str(root/'plain.tar'),str(root/'encrypted.age'),payload['recipient'])
 assert encrypted['plaintext']==bundle['archive']
 expected={name:encrypted[name] for name in ('plaintext','ciphertext')}
 offer={'schemaVersion':1,'kind':'synthetic-encrypted-bundle','nonce':nonce,
        'ciphertext':expected['ciphertext'],'plaintext':expected['plaintext'],
        'bundleReceiptSha256':hashlib.sha256(b.canonical(bundle)).hexdigest()}
 with t.Channel(sys.stdin.fileno(),sys.stdout.fileno(),180) as channel:
  channel.send_header(offer)
  t.round_trip(channel,root/'encrypted.age',root/'retrieved.age',nonce,expected['ciphertext'])
  # The receiver's retained bytes are returned through the authenticated channel.
  # Decryption and file replay consume precisely this retrieved copy.
  corrupt=root/'corrupt.age';corrupt.write_bytes((root/'retrieved.age').read_bytes());corrupt.chmod(0o600)
  with corrupt.open('r+b') as out:out.seek(64);value=out.read(1);out.seek(64);out.write(bytes([value[0]^1]))
  try:e.decrypt(str(root/'tools/age'),str(corrupt),str(root/'never.tar'),str(key),expected)
  except ValueError:pass
  else:raise ValueError('Changed retrieved ciphertext accepted')
  assert not (root/'never.tar').exists()
  recovered=e.decrypt(str(root/'tools/age'),str(root/'retrieved.age'),str(root/'decrypted.tar'),str(key),expected)
  wrong=dict(binding);wrong['releaseNonce']='0'*32
  try:b.restore(str(root/'decrypted.tar'),bundle,wrong,str(root/'never-restored'))
  except ValueError:pass
  else:raise ValueError('Mismatched release accepted')
  assert not (root/'never-restored').exists()
  result=b.restore(str(root/'decrypted.tar'),bundle,binding,str(root/'restored'))
  assert result['databaseRecoveryVerified'] is False and result['secretsUsable'] is False
  for name in b.ROLES:
   with b.files.directory(sources[name]) as source,b.files.directory(str(root/'restored'/name)) as target:
    assert b.files.walk(source)==b.files.walk(target)
  assert (root/'restored'/'production'/'synthetic-secret').read_bytes()==b'SYNTHETIC ONLY - recovery integration secret'
  channel.send_header({'schemaVersion':1,'kind':'synthetic-bundle-restored','nonce':nonce,
    'bundleReceiptSha256':offer['bundleReceiptSha256'],'components':len(b.ROLES),
    'ciphertextTamperRejected':True,'wrongReleaseRejected':True,'syntheticSecretMatched':True,
    'productionDataAccessed':False,'productionDatabaseRecoveryVerified':False})
'''


def header(channel):
    size=struct.unpack('!I',channel.read(4))[0];t.require(0<size<=t.HEADER_LIMIT)
    raw=channel.read(size);value=json.loads(raw);t.require(t.canonical(value)==raw)
    return value


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--identity-file',required=True)
    parser.add_argument('--tools-directory',required=True)
    parser.add_argument('--linux-tools-directory',required=True)
    parser.add_argument('--output-directory',required=True)
    args=parser.parse_args()
    directory=Path(args.output_directory).absolute()
    with t.files.directory(str(directory),private=True):pass
    sources={name:(ROOT/name).read_bytes() for name in SOURCES}
    own_source=Path(__file__).read_bytes()
    revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
    mobile=subprocess.check_output(['git','rev-parse','HEAD:tdf-mobile'],cwd=ROOT,text=True).strip()
    dirty=bool(subprocess.check_output(['git','status','--porcelain'],cwd=ROOT,text=True).strip())
    t.require(re.fullmatch('[a-f0-9]{40}',revision) and re.fullmatch('[a-f0-9]{40}',mobile))
    pins=json.loads(sources['ops/hetzner/recovery-tools.json'])['platforms']
    arch={'x86_64':'amd64'}.get(platform.machine(),'unsupported')
    local_platform=platform.system().lower()+'-'+arch;t.require(local_platform in pins)
    keygen=Path(args.tools_directory).absolute()/'age-keygen'
    with t.files.directory(str(keygen.parent),private=True):pass
    keygen_bytes=keygen.read_bytes();t.require(hashlib.sha256(keygen_bytes).hexdigest()==pins[local_platform]['binaries']['age-keygen'])
    linux_age=(Path(args.linux_tools_directory).absolute()/'age').read_bytes()
    t.require(hashlib.sha256(linux_age).hexdigest()==pins['linux-amd64']['binaries']['age'])
    nonce=uuid.uuid4().hex
    binding={'sourceRevision':revision,'mobileRevision':mobile,'releaseNonce':nonce,
             'runtimeSha256':hashlib.sha256(b'synthetic runtime only').hexdigest(),
             'migrationManifestSha256':hashlib.sha256(b'synthetic migration boundary only').hexdigest(),
             'databaseSystemIdentifier':'123456'}
    remote=['systemd-run','--pipe','--wait','--collect','--quiet',
            '--property=DynamicUser=yes','--property=PrivateNetwork=yes','--property=PrivateTmp=yes',
            '--property=ProtectSystem=strict','--property=ProtectHome=yes','--property=NoNewPrivileges=yes',
            '--property=MemoryMax=256M','--property=TasksMax=32','--property=RuntimeMaxSec=220',
            '--','python3','-c',REMOTE]
    command=['ssh','-F','/dev/null','-T','-o','BatchMode=yes','-o','IdentitiesOnly=yes',
             '-o','StrictHostKeyChecking=yes','-o','ConnectTimeout=10','-i',
             str(Path(args.identity_file).absolute()),'root@178.105.93.101',shlex.join(remote)]
    started=datetime.datetime.now(datetime.timezone.utc).isoformat()
    with tempfile.TemporaryDirectory(prefix='tdf-synthetic-key-') as temporary:
        identity=Path(temporary)/'identity'
        subprocess.run([str(keygen),'-o',str(identity)],check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,timeout=10)
        recipient=subprocess.check_output([str(keygen),'-y',str(identity)],stderr=subprocess.DEVNULL,text=True,timeout=10).strip()
        t.require(re.fullmatch(r'age1[0-9a-z]{58}',recipient))
        t.require(keygen.read_bytes()==keygen_bytes)
        payload={'sources':{name:base64.b64encode(data).decode() for name,data in
                           {**sources,'tools/age':linux_age}.items()},
                 'syntheticIdentity':base64.b64encode(identity.read_bytes()).decode(),
                 'recipient':recipient,'binding':binding}
        data=t.canonical(payload);t.require(len(data)<=MAX_PAYLOAD)
        process=subprocess.Popen(command,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.DEVNULL,bufsize=0)
        try:
            with t.Channel(process.stdout.fileno(),process.stdin.fileno(),200) as channel:
                for offset in range(0,len(data),t.BLOCK):channel.write(data[offset:offset+t.BLOCK])
                offer=header(channel)
                t.require(isinstance(offer,dict) and set(offer)=={'schemaVersion','kind','nonce','ciphertext','plaintext','bundleReceiptSha256'}
                    and type(offer['schemaVersion']) is int and offer['schemaVersion']==1
                    and offer['kind']=='synthetic-encrypted-bundle' and offer['nonce']==nonce
                    and isinstance(offer['bundleReceiptSha256'],str) and re.fullmatch('[a-f0-9]{64}',offer['bundleReceiptSha256']))
                t.binding(nonce,offer['ciphertext']);t.binding(nonce,offer['plaintext'])
                t.require(offer['ciphertext']['bytes'] <= 8*1024**2 and offer['plaintext']['bytes'] <= 8*1024**2)
                retained=t.retain_and_return(channel,directory/('synthetic-'+nonce+'.age'),nonce,offer['ciphertext'])
                expected={'schemaVersion':1,'kind':'synthetic-bundle-restored','nonce':nonce,
                    'bundleReceiptSha256':offer['bundleReceiptSha256'],'components':6,
                    'ciphertextTamperRejected':True,'wrongReleaseRejected':True,'syntheticSecretMatched':True,
                    'productionDataAccessed':False,'productionDatabaseRecoveryVerified':False}
                channel.receive_header(expected)
            process.stdin.close();t.require(process.wait(timeout=20)==0)
            t.require(all((ROOT/name).read_bytes()==value for name,value in sources.items()) and Path(__file__).read_bytes()==own_source)
            receipt={'revision':revision,'mobileRevision':mobile,'sourceWorktreeDirty':dirty,'sourceUnchanged':True,
                'startedAt':started,'completedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),
                'sourceHashes':{name:hashlib.sha256(value).hexdigest() for name,value in sources.items()},
                'runnerSha256':hashlib.sha256(own_source).hexdigest(),'envelope':offer,'transfer':retained,'replay':expected,
                'scope':'Synthetic complete file bundle, pinned age, SSH off-host retention/retrieval and exact metadata replay; no production data, real key custody or PostgreSQL recovery qualification'}
            with (directory/('bundle-'+nonce+'.json')).open('x') as output:
                os.fchmod(output.fileno(),0o600);json.dump(receipt,output,indent=2);output.write('\n');output.flush();os.fsync(output.fileno())
            with t.files.directory(str(directory),private=True) as parent:os.fsync(parent)
            print(json.dumps(receipt))
        finally:
            if process.poll() is None:
                process.terminate()
                try:process.wait(timeout=5)
                except subprocess.TimeoutExpired:process.kill();process.wait(timeout=5)
            for stream in (process.stdin,process.stdout):
                if stream and not stream.closed:stream.close()


if __name__=='__main__':
    try:main()
    except Exception:raise SystemExit('Synthetic recovery bundle integration failed; no production recovery claimed')
