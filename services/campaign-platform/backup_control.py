"""Strict-admin backup setup. Private recovery keys are returned once, never stored."""
import json
import base64
import os
import time
from pathlib import Path
from cryptography.hazmat.primitives import hashes,serialization
from cryptography.hazmat.primitives.asymmetric import padding
from cryptography.exceptions import InvalidSignature

from backup_crypto import recovery_key
from domain import Denied, uid
from drive_backup import DriveBackup


def authority(store, actor, create=False):
    if not actor.get('admin'):raise Denied('Solo un administrador de TDF puede gestionar respaldos.', 403)
    with store.db(True) as db:
        row = db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
        if not row and create:
            db.execute('INSERT INTO backup_settings(id,owner) VALUES(1,?)', (actor['id'],))
            row = db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
        if row and row['owner'] != actor['id']:
            raise Denied('La configuración del respaldo pertenece al administrador custodio de su clave.', 403)
        return dict(row) if row else None


def status(store, actor):
    settings = authority(store, actor)
    with store.db() as db:
        accounts = [dict(row) for row in db.execute('SELECT id,handle,external_id,revoked FROM accounts '
                                                   'WHERE platform="drive" AND scope="tdf" AND owner=?', (actor['id'],))]
        requests = [dict(row) for row in db.execute('SELECT * FROM backup_requests ORDER BY requested_at DESC LIMIT 20')]
        count = db.execute('SELECT count(*) FROM backup_objects WHERE remote_id IS NOT NULL').fetchone()[0]
        progress=db.execute('SELECT at,details FROM heartbeat WHERE name="backup-transfer"').fetchone()
    return {'settings': {key: value for key, value in (settings or {}).items() if key not in ('public_key','challenge','challenge_expires_at')},
            'accounts': accounts, 'requests': requests, 'verified_remote_objects': count,
            'transfer':{'at':progress['at'],**json.loads(progress['details'])} if progress else None,
            'key_custody': 'La clave privada de recuperación queda fuera del servidor. Las copias no requieren la Mac encendida.'}


def generate_key(store, actor, data):
    settings = authority(store, actor, create=True)
    with store.db() as db:
        exists = db.execute('SELECT 1 FROM backup_objects LIMIT 1').fetchone()
    if settings.get('public_key') and (exists or settings['enabled'] or settings['key_confirmed_at']):
        raise Denied('Ya existe una clave de recuperación en uso. Su rotación requiere un procedimiento separado.', 409)
    if settings.get('public_key') and data.get('replace_unconfirmed') is not True:
        raise Denied('Confirma reemplazar la clave que todavía no fue guardada ni utilizada.', 409)
    key = recovery_key()
    with store.db(True) as db:
        current = db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
        if current['enabled'] or current['key_confirmed_at'] or current['fingerprint']!=settings['fingerprint']:
            raise Denied('La configuración cambió. No se reemplazó la clave.', 409)
        db.execute('UPDATE backup_settings SET public_key=?,fingerprint=?,key_confirmed_at=NULL WHERE id=1',
                   (key['public_key'], key['fingerprint']))
        store.audit(db, actor['id'], 'backup.recipient.create', details={'fingerprint': key['fingerprint'],
                                                                       'private_key_persisted': False})
    return {'private_key': key['private_key'], 'fingerprint': key['fingerprint'],
            'filename': 'tdf-campaign-recovery-' + key['fingerprint'][:16] + '.pem'}


def key_challenge(store,actor):
    settings=authority(store,actor)
    if not settings or not settings['public_key']:raise Denied('Primero crea y guarda la clave de recuperación.',409)
    challenge=base64.b64encode(os.urandom(32)).decode()
    with store.db(True) as db:db.execute('UPDATE backup_settings SET challenge=?,challenge_expires_at=? WHERE id=1',(challenge,time.time()+300))
    return {'challenge':challenge,'fingerprint':settings['fingerprint']}


def confirm_key(store, actor, data):
    settings = authority(store, actor)
    if (not settings or data.get('fingerprint')!=settings['fingerprint'] or not settings['challenge']
            or settings['challenge_expires_at']<time.time()):raise Denied('La comprobación de la clave venció. Selecciona nuevamente tu archivo.',409)
    try:
        signature=base64.b64decode(data.get('signature',''),validate=True)
        serialization.load_pem_public_key(settings['public_key'].encode()).verify(signature,base64.b64decode(settings['challenge']),padding.PSS(mgf=padding.MGF1(hashes.SHA256()),salt_length=32),hashes.SHA256())
    except (ValueError,InvalidSignature,TypeError):raise Denied('La clave seleccionada no corresponde al respaldo. La clave privada debe permanecer en tu equipo.',403) from None
    with store.db(True) as db:
        changed=db.execute('UPDATE backup_settings SET key_confirmed_at=?,challenge=NULL,challenge_expires_at=NULL WHERE id=1 AND fingerprint=? AND challenge=?',
                          (time.time(),settings['fingerprint'],settings['challenge'])).rowcount
        if changed!=1:raise Denied('La configuración cambió; vuelve a comprobar la clave.',409)
        store.audit(db, actor['id'], 'backup.recipient.custody_confirmed',
                    details={'fingerprint': settings['fingerprint']})


def needed_bytes(store, settings):
    with store.db() as db:
        assets = db.execute('SELECT sha256,size FROM assets WHERE status="ready"').fetchall()
        known = {(r['source_sha256'], r['source_size']) for r in db.execute(
            'SELECT source_sha256,source_size FROM backup_objects WHERE account=? '
            'AND recipient=? AND folder_id=? AND remote_id IS NOT NULL',
            (settings['account'], settings['fingerprint'],settings.get('folder_id')))}
    # Include encryption framing, metadata, config, database and two snapshots' growth.
    unique = {(r['sha256'], r['size']) for r in assets}
    return sum(size + 20 * max(1, (size + 1048575)//1048576) + 8192
               for sha, size in unique if (sha, size) not in known) + 16 * 1024 * 1024


def enable(store, providers, actor, data):
    settings = authority(store, actor)
    if not settings or not settings['key_confirmed_at']:
        raise Denied('Guarda y confirma la clave de recuperación antes de activar el respaldo.', 409)
    if data.get('private_backup_approved') is not True:
        raise Denied('Confirma la copia cifrada de campañas, configuración y archivos al Drive privado de TDF.', 409)
    with store.db() as db:
        account = db.execute('SELECT * FROM accounts WHERE id=? AND owner=? AND platform="drive" '
                             'AND scope="tdf" AND revoked=0', (data.get('account'), actor['id'])).fetchone()
    if not account:raise Denied('Conecta Google Drive de TDF.', 409)
    if settings['account'] and settings['account'] != account['id']:
        raise Denied('Cambiar el destino existente requiere revisar la recuperación anterior.', 409)
    settings['account'] = account['id']
    drive = DriveBackup(store, providers, settings)
    quota = drive.quota(needed_bytes(store, settings))  # Before creating any folder or object.
    folder = drive.folder()
    with store.db(True) as db:
        db.execute('UPDATE backup_settings SET account=?,folder_id=?,enabled=1 WHERE id=1 AND fingerprint=?',
                   (account['id'], folder, settings['fingerprint']))
        store.audit(db, actor['id'], 'backup.enable', details={'folder_id': folder, 'quota': quota,
                                                              'encrypted': True, 'purchased_storage': False})
    return {'enabled': True, 'folder_url': 'https://drive.google.com/drive/folders/' + folder,
            'quota': quota, 'request': queue(store, actor)}


def queue(store, actor):
    settings = authority(store, actor)
    if not settings or not settings['enabled']:
        raise Denied('El respaldo externo todavía no está activado.', 409)
    with store.db(True) as db:
        existing = db.execute('SELECT id FROM backup_requests WHERE state IN ("queued","running") '
                              'ORDER BY requested_at LIMIT 1').fetchone()
        if existing:return existing['id']
        identifier = uid()
        db.execute('INSERT INTO backup_requests(id,owner,requested_at) VALUES(?,?,?)',
                   (identifier, actor['id'], time.time()))
        store.audit(db, actor['id'], 'backup.queue', subject=identifier)
        return identifier


def disable(store, actor):
    authority(store, actor)
    with store.db(True) as db:
        db.execute('UPDATE backup_settings SET enabled=0 WHERE id=1')
        store.audit(db, actor['id'], 'backup.disable', details={'remote_objects_deleted': False})
