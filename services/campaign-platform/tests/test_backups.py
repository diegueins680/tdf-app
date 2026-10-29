import base64
import hashlib
import io
import json
import os
import random
import sqlite3
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch
import sys

sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from cryptography.exceptions import InvalidTag
from cryptography.fernet import Fernet
from cryptography.hazmat.primitives import hashes,serialization
from cryptography.hazmat.primitives.asymmetric import padding
from backup_crypto import EncryptedSource, recovery_key, decrypt_stream, BLOCK
from backup_control import authority,generate_key,key_challenge,confirm_key,enable,queue,status
from drive_backup import DriveBackup
from domain import Store,Denied,uid
from providers import Providers,Vault,ProviderError
from backup import run


class BackupEnvelopeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):cls.keys=recovery_key()
    def setUp(self):self.tmp=tempfile.TemporaryDirectory();self.path=Path(self.tmp.name)/'source';self.data=os.urandom(BLOCK*2+33);self.path.write_bytes(self.data)
    def tearDown(self):self.tmp.cleanup()
    def encrypted(self,data=None):
        if data is not None:self.data=data;self.path.write_bytes(data)
        return EncryptedSource(self.path,self.keys['public_key'],hashlib.sha256(self.data).hexdigest(),len(self.data))
    def test_random_access_resume_is_identical_and_restores_every_byte(self):
        source=self.encrypted();whole=source.read_range(0,source.total);randomizer=random.Random(34)
        for _ in range(16):
            offset=randomizer.randrange(source.total);size=min(randomizer.randrange(1,200000),source.total-offset)
            self.assertEqual(source.read_range(offset,size),whole[offset:offset+size])
        resumed=EncryptedSource(self.path,self.keys['public_key'],hashlib.sha256(self.data).hexdigest(),len(self.data),json.loads(json.dumps(source.material)))
        self.assertEqual(resumed.read_range(100,123456),whole[100:123556])
        sink=io.BytesIO();proof=decrypt_stream(io.BytesIO(whole),self.keys['private_key'].encode(),sink)
        self.assertEqual(sink.getvalue(),self.data);self.assertTrue(proof['authenticated'])
    def test_empty_payload_still_has_authenticated_encryption(self):
        source=self.encrypted(b'');cipher=source.read_range(0,source.total)
        self.assertEqual(decrypt_stream(io.BytesIO(cipher),self.keys['private_key'].encode())['size'],0)
        with self.assertRaises((ValueError,InvalidTag)):decrypt_stream(io.BytesIO(cipher[:-1]),self.keys['private_key'].encode())
    def test_corruption_reordering_truncation_and_extra_bytes_fail(self):
        source=self.encrypted();cipher=source.read_range(0,source.total)
        changed=bytearray(cipher);changed[-2]^=1
        variants=[bytes(changed),cipher[:-20],cipher+b'extra']
        prefix=len(source.prefix);record=BLOCK+20
        variants.append(cipher[:prefix]+cipher[prefix+record:prefix+2*record]+cipher[prefix:prefix+record]+cipher[prefix+2*record:])
        for invalid in variants:
            with self.subTest(length=len(invalid)):
                with self.assertRaises((ValueError,InvalidTag)):decrypt_stream(io.BytesIO(invalid),self.keys['private_key'].encode())
    def test_source_change_refused_before_reusing_a_nonce(self):
        source=self.encrypted();self.path.write_bytes(b'x'+self.data[1:])
        with self.assertRaisesRegex(ValueError,'source block changed'):source.read_range(len(source.prefix),100)


class BackupBoundaryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):cls.keys=recovery_key()
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.store=Store(Path(self.tmp.name)/'campaigns.sqlite3')
        self.admin={'id':9,'name':'TDF','admin':True};self.other={'id':2,'name':'Usuario','admin':False};self.store.user(self.admin);self.store.user(self.other)
        self.config={'STATE_DIR':self.tmp.name,'GOOGLE_CLIENT_ID':'client','GOOGLE_CLIENT_SECRET':'secret','PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform'}
        self.providers=Providers(self.store,Vault(Fernet.generate_key()),self.config);self.aid=uid()
        with self.store.db(True) as db:
            db.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',(self.aid,9,'drive','permission-owner','Drive de TDF','tdf',self.providers.vault.seal({'access_token':'test'}),'[]',None,0))
            db.execute('INSERT INTO backup_settings(id,owner,account,public_key,fingerprint) VALUES(1,?,?,?,?)',(9,self.aid,self.keys['public_key'],self.keys['fingerprint']))
    def tearDown(self):self.tmp.cleanup()
    def settings(self):
        with self.store.db() as db:return dict(db.execute('SELECT * FROM backup_settings').fetchone())
    def proof(self):
        challenge=key_challenge(self.store,self.admin);key=serialization.load_pem_private_key(self.keys['private_key'].encode(),None)
        signature=key.sign(base64.b64decode(challenge['challenge']),padding.PSS(mgf=padding.MGF1(hashes.SHA256()),salt_length=32),hashes.SHA256())
        return {'fingerprint':self.keys['fingerprint'],'signature':base64.b64encode(signature).decode()}
    def confirmed(self):confirm_key(self.store,self.admin,self.proof())
    def test_admin_and_custodian_boundaries_and_private_account_hiding(self):
        for actor in (self.other,{'id':3,'name':'Otro admin','admin':True}):
            with self.assertRaises(Denied):authority(self.store,actor)
        self.assertEqual(self.store.account_list(self.other),[])
        with self.assertRaises(Denied):self.providers.start(self.other,'drive','tdf')
        self.assertNotIn('private_key',json.dumps(status(self.store,self.admin)))
    def test_key_custody_requires_real_one_use_signature(self):
        proof=self.proof()
        with self.assertRaises(Denied):confirm_key(self.store,self.admin,{**proof,'signature':base64.b64encode(b'not a signature').decode()})
        confirm_key(self.store,self.admin,proof)
        self.assertTrue(self.settings()['key_confirmed_at'])
        with self.assertRaises(Denied):confirm_key(self.store,self.admin,proof)
        with self.assertRaises(Denied):generate_key(self.store,self.admin,{'replace_unconfirmed':True})
    def test_quota_failure_precedes_folder_creation_and_enable(self):
        self.confirmed()
        quota={'user':{'permissionId':'permission-owner'},'storageQuota':{'limit':'1000','usage':'900'}}
        with patch('drive_backup.request',return_value=(quota,{})) as network:
            with self.assertRaisesRegex(Denied,'espacio suficiente'):enable(self.store,self.providers,self.admin,{'account':self.aid,'private_backup_approved':True})
            self.assertEqual(network.call_count,1);self.assertIn('/about?',network.call_args.args[0])
        self.assertFalse(self.settings()['enabled'])
    def test_shared_folder_is_refused(self):
        self.confirmed();drive=DriveBackup(self.store,self.providers,self.settings())
        data={'id':'folder','ownedByMe':True,'mimeType':'application/vnd.google-apps.folder','permissions':[{'id':'permission-owner','type':'user','role':'owner'},{'id':'anyone','type':'anyone','role':'reader'}]}
        with patch.object(drive,'call',return_value=data):
            with self.assertRaises(Denied):drive.private_folder('folder')
    def test_disable_or_revocation_stops_in_flight_transfer(self):
        self.confirmed()
        with self.store.db(True) as db:db.execute('UPDATE backup_settings SET enabled=1')
        drive=DriveBackup(self.store,self.providers,self.settings());drive.enabled()
        with self.store.db(True) as db:db.execute('UPDATE accounts SET revoked=1 WHERE id=?',(self.aid,))
        with self.assertRaises(Denied):drive.enabled()
    def test_durable_backup_requests_are_not_publication_jobs(self):
        self.confirmed()
        with self.store.db(True) as db:db.execute('UPDATE backup_settings SET enabled=1')
        first=queue(self.store,self.admin);self.assertEqual(queue(self.store,self.admin),first)
        with self.store.db() as db:self.assertEqual(db.execute('SELECT count(*) FROM jobs').fetchone()[0],0)
    def test_repeated_process_interruption_is_bounded_before_transfers(self):
        self.confirmed()
        with self.store.db(True) as db:db.execute('UPDATE backup_settings SET enabled=1')
        identifier=queue(self.store,self.admin)
        with self.store.db(True) as db:
            db.execute('UPDATE backup_requests SET state="running" WHERE id=?',(identifier,))
            for n in range(3):self.store.audit(db,None,'backup.attempt_started',subject=identifier,details={'attempt':n+1})
        with patch('backup.external_backup') as transfer:
            result=run(self.store,self.providers,self.config,requested=True)
            self.assertEqual(result['status'],'blocked');transfer.assert_not_called()
    def test_snapshot_is_consistent_and_no_offhost_claim_without_configuration(self):
        result=run(self.store,self.providers,self.config)
        self.assertFalse(result['off_host'])
        restored=Path(self.tmp.name)/'backups'/result['backup']
        with sqlite3.connect(restored) as db:
            self.assertEqual(db.execute('PRAGMA integrity_check').fetchone()[0],'ok')
            self.assertEqual(db.execute('SELECT count(*) FROM users').fetchone()[0],2)
    def test_file_identity_is_checked_and_transfer_is_reused(self):
        self.confirmed()
        with self.store.db(True) as db:db.execute('UPDATE backup_settings SET enabled=1,folder_id="folder"')
        drive=DriveBackup(self.store,self.providers,self.settings());path=Path(self.tmp.name)/'source';path.write_bytes(b'abc');digest=hashlib.sha256(b'abc').hexdigest()
        def metadata(identifier,total,source_sha,marker):return {'id':identifier,'size':str(total)}
        with patch.object(drive,'folder',return_value='folder'),patch.object(drive,'object_metadata',side_effect=metadata),patch('drive_backup.request',return_value=({}, {'Location':'https://www.googleapis.com/upload/drive/v3/files?upload_id=test'})),patch.object(drive,'upload_call',return_value={'complete':True,'id':'remote','offset':1000}) as upload:
            first=drive.put(path,digest,3);second=drive.put(path,digest,3)
            self.assertFalse(first['reused']);self.assertTrue(second['reused']);self.assertEqual(upload.call_count,1)
        with self.store.db() as db:
            record=dict(db.execute('SELECT * FROM backup_objects').fetchone());plain=self.providers.vault.open(record['upload_state'])
            self.assertEqual(set(plain),{'marker'});self.assertNotIn('test-token',record['upload_state'])


if __name__=='__main__':unittest.main()
