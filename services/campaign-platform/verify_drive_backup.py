"""Decrypt and qualify one campaign backup locally, without starting any service.

The private recovery key remains on this computer. SSH carries encrypted objects
only. Large media are authenticated and hashed as a stream, with no second copy.
"""
import argparse
import contextlib
import hashlib
import io
import json
import re
import sqlite3
import subprocess
import tempfile
import time
from pathlib import Path
from backup_crypto import decrypt_stream
from providers import Vault


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--key-file',required=True)
    parser.add_argument('--ssh-host',required=True)
    parser.add_argument('--ssh-key',required=True)
    parser.add_argument('--remote-stream',default='/opt/tdf-campaign-platform/current/stream_backup_object.py')
    parser.add_argument('--manifest',required=True)
    parser.add_argument('--receipt',required=True)
    args=parser.parse_args()
    if not re.fullmatch(r'[A-Za-z0-9._@-]+',args.ssh_host):raise ValueError('Invalid SSH host')
    if not re.fullmatch(r'/[A-Za-z0-9_./-]+',args.remote_stream):raise ValueError('Invalid remote command path')
    private=Path(args.key_file).read_bytes()
    checks=[]

    @contextlib.contextmanager
    def remote(identifier):
        if not re.fullmatch(r'[A-Za-z0-9_-]{1,200}',identifier):raise ValueError('Invalid Drive id')
        command=['ssh','-i',args.ssh_key,'-o','BatchMode=yes','-o','IdentitiesOnly=yes',
                 '-o','StrictHostKeyChecking=yes',args.ssh_host,
                 '/opt/tdf-campaign-platform/venv/bin/python',args.remote_stream,identifier]
        process=subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        try:
            yield process.stdout
            if process.wait(timeout=30)!=0:raise ValueError('Encrypted remote download failed')
        finally:
            if process.poll() is None:process.terminate();process.wait(timeout=10)
            process.stdout.close();process.stderr.close()

    def read_object(item,sink=None):
        with remote(item['id']) as stream:proof=decrypt_stream(stream,private,sink)
        if item.get('source_sha256') and proof['sha256']!=item['source_sha256']:raise ValueError('Object checksum mismatch')
        if item.get('size') is not None and proof['size']!=item['size']:raise ValueError('Object size mismatch')
        checks.append({'id':item['id'],**proof})
        return proof

    manifest_buffer=io.BytesIO()
    read_object({'id':args.manifest},manifest_buffer)
    if manifest_buffer.tell()>4*1024*1024:raise ValueError('Unexpectedly large manifest')
    manifest=json.loads(manifest_buffer.getvalue())
    if manifest['schemaVersion']!=1:raise ValueError('Unknown manifest version')
    configuration=io.BytesIO();read_object(manifest['configuration'],configuration)
    config=json.loads(configuration.getvalue())
    vault=Vault(config['ENCRYPTION_KEY'].encode())
    with tempfile.TemporaryDirectory(prefix='tdf-backup-isolated-restore-') as directory:
        database=Path(directory)/'campaigns.sqlite3'
        with database.open('xb') as sink:read_object(manifest['database'],sink)
        database.chmod(0o600)
        with sqlite3.connect(database) as db:
            db.row_factory=sqlite3.Row
            if db.execute('PRAGMA integrity_check').fetchone()[0]!='ok':raise ValueError('SQLite integrity failure')
            if db.execute('PRAGMA foreign_key_check').fetchall():raise ValueError('Foreign-key failure')
            ready={row['id']:dict(row) for row in db.execute('SELECT id,size,sha256 FROM assets WHERE status="ready"')}
            if set(ready)!={item['asset_id'] for item in manifest['assets']}:raise ValueError('Manifest does not cover ready assets')
            accounts=list(db.execute('SELECT credentials FROM accounts WHERE revoked=0'))
            for account in accounts:
                if not vault.open(account['credentials']).get('access_token'):raise ValueError('Credential recovery failed')
            for item in manifest['assets']:
                original=ready[item['asset_id']]
                if original['sha256']!=item['source_sha256'] or original['size']!=item['size']:raise ValueError('Manifest asset mismatch')
                read_object(item)
    receipt={'result':'PASS','at':time.time(),'manifest_id':args.manifest,
             'recipient':manifest['recipient'],'objects':checks,'ready_assets':len(manifest['assets']),
             'credential_vault_records_recovered':len(accounts),
             'sqlite_integrity':'ok','foreign_keys':'ok','private_key_sent_to_server':False,
             'scope':'All captured ready objects decrypted/authenticated and hashed; isolated SQLite restored and vault recovered. '
                     'No restored service started, provider calls made with restored credentials, production failover or core-TDF PostgreSQL recovery.'}
    Path(args.receipt).write_text(json.dumps(receipt,indent=2))
    print(json.dumps({'result':'PASS','ready_assets':receipt['ready_assets'],'objects':len(checks),
                      'receipt':args.receipt,'private_key_sent_to_server':False}))


if __name__=='__main__':main()
