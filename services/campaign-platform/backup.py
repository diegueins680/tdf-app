"""Independent backups. Large transfers never occupy the publishing worker."""
import datetime
import fcntl
import hashlib
import json
import os
import sqlite3
import sys
import time
from pathlib import Path
from domain import Denied, uid
from providers import ProviderError


def snapshot(store, config, folder, stamp):
    destination = folder / (stamp + '.sqlite3')
    with store.db() as source:
        with sqlite3.connect(destination) as target:source.backup(target)
    with sqlite3.connect(destination) as db:
        db.row_factory = sqlite3.Row
        assets = [dict(row) for row in db.execute('SELECT id,path,size,sha256,status FROM assets')]
    receipt = {'at': time.time(), 'database': destination.name,
               'database_sha256': hashlib.sha256(destination.read_bytes()).hexdigest(),
               'assets': assets,
               'secret_recovery': 'The external envelope includes configuration, encrypted for the separately held private recovery key.',
               'off_host_backup': 'Not complete until an external manifest is committed and verified.'}
    (folder / (stamp + '.json')).write_text(json.dumps(receipt, indent=2))
    return destination, receipt


def write_private(path, value):
    with path.open('xb') as stream:
        os.chmod(path, 0o600)
        stream.write(value)
    return path


def external_backup(store, providers, config, settings, database, receipt, folder, stamp):
    from backup_control import needed_bytes
    from drive_backup import DriveBackup
    drive = DriveBackup(store, providers, settings)
    drive.enabled()
    drive.quota(needed_bytes(store, settings))
    drive.folder()
    cfg_bytes = json.dumps(config, sort_keys=True).encode()
    cfg_file = write_private(folder / ('config-' + stamp + '.json'), cfg_bytes)
    manifest_file = folder / ('manifest-' + stamp + '.json')
    try:
        result = {'schemaVersion': 1, 'captured_at': receipt['at'],
                  'recipient': settings['fingerprint'], 'assets': [],
                  'scope': 'Consistent SQLite snapshot, service configuration and all ready objects referenced by that snapshot. '
                           'Uploading/partial objects are excluded and explicitly listed. Restore must start isolated, with publishing disabled.'}
        result['database'] = drive.put(database, receipt['database_sha256'], database.stat().st_size)
        result['configuration'] = drive.put(cfg_file, hashlib.sha256(cfg_bytes).hexdigest(), len(cfg_bytes))
        root = Path(config['STATE_DIR']) / 'assets'
        for asset in sorted(receipt['assets'], key=lambda item: item['size'], reverse=True):
            if asset['status'] != 'ready':continue
            if not asset['sha256']:raise Denied('Un archivo terminado no tiene una huella verificable; respaldo detenido.', 409)
            path = root / asset['path']
            if path.parent != root or path.is_symlink():raise Denied('Ruta de archivo no admitida para respaldo.', 409)
            result['assets'].append({'asset_id': asset['id'], **drive.put(path, asset['sha256'], asset['size'])})
        result['excluded_assets'] = [{'id': asset['id'], 'status': asset['status']}
                                     for asset in receipt['assets'] if asset['status'] not in ('ready', 'deleted')]
        encoded = json.dumps(result, sort_keys=True, separators=(',', ':')).encode()
        write_private(manifest_file, encoded)
        remote_manifest = drive.put(manifest_file, hashlib.sha256(encoded).hexdigest(), len(encoded))
        # The commit marker is uploaded last. No complete-backup claim precedes it.
        receipt.update(off_host_backup='manifest_committed', external_manifest=remote_manifest,
                       ready_assets=len(result['assets']), excluded_assets=result['excluded_assets'],
                       private_folder_id=drive.settings['folder_id'],
                       restore_verification='Encrypted objects acknowledged and metadata checked. Independent decryption/restore requires its own receipt.')
        (folder / (stamp + '.json')).write_text(json.dumps(receipt, indent=2))
        return remote_manifest
    finally:
        # Only this process's temporary cleartext config/manifest are removed.
        cfg_file.unlink(missing_ok=True)
        manifest_file.unlink(missing_ok=True)


def run(store, providers, config, requested=False):
    root = Path(config['STATE_DIR'])
    folder = root / 'backups'
    folder.mkdir(exist_ok=True)
    with (folder / '.executor.lock').open('a+b') as lock:
        try:fcntl.flock(lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:return {'status': 'already_running'}
        with store.db(True) as db:
            settings = db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
            pending = db.execute('SELECT * FROM backup_requests WHERE state IN ("queued","running") '
                                 'ORDER BY requested_at LIMIT 1').fetchone()
            if requested and not pending:return {'status': 'no_requested_backup'}
            if not pending and settings and settings['enabled']:
                identifier = uid()
                db.execute('INSERT INTO backup_requests(id,owner,requested_at) VALUES(?,?,?)',
                           (identifier, settings['owner'], time.time()))
                pending = db.execute('SELECT * FROM backup_requests WHERE id=?', (identifier,)).fetchone()
            if pending:
                attempts=db.execute('SELECT count(*) FROM audit WHERE action="backup.attempt_started" AND subject=?',(pending['id'],)).fetchone()[0]
                if attempts>=3:
                    message='El ejecutor de respaldo fue interrumpido tres veces. Revisa el estado antes de reanudar.'
                    db.execute('UPDATE backup_requests SET state="blocked",finished_at=?,error=? WHERE id=?',(time.time(),message,pending['id']))
                    store.notice(db,pending['owner'],message,severity='error')
                    return {'status':'blocked','error':message,'off_host':False}
                store.audit(db,None,'backup.attempt_started',subject=pending['id'],details={'attempt':attempts+1})
                db.execute('UPDATE backup_requests SET state="running",started_at=?,error=NULL WHERE id=?',
                           (time.time(), pending['id']))
        stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S') + '-' + uid()[:8]
        try:
            database, receipt = snapshot(store, config, folder, stamp)
            external = None
            if settings and settings['enabled'] and settings['key_confirmed_at']:
                external = external_backup(store, providers, config, dict(settings), database, receipt, folder, stamp)
            with store.db(True) as db:
                if pending:
                    if not external:raise Denied('El respaldo externo no está habilitado.', 409)
                    db.execute('UPDATE backup_requests SET state="completed",finished_at=?,manifest_id=? WHERE id=?',
                               (time.time(), external['id'], pending['id']))
                    db.execute('UPDATE backup_settings SET last_completed_at=? WHERE id=1', (time.time(),))
                    store.audit(db, None, 'backup.external_manifest_committed',
                                details={'manifest_id': external['id'], 'recipient': settings['fingerprint'],
                                         'assets': receipt['ready_assets'], 'independent_restore_verified': False})
            return {'backup': database.name, 'asset_count': len(receipt['assets']),
                    'off_host': bool(external), 'external_manifest_id': external['id'] if external else None}
        except Exception as error:
            message = str(error) if isinstance(error, (Denied, ProviderError)) else 'backup_internal_error_requires_review'
            with store.db(True) as db:
                if pending:
                    db.execute('UPDATE backup_requests SET state="blocked",finished_at=?,error=? WHERE id=?',
                               (time.time(), message, pending['id']))
                    store.notice(db, pending['owner'], 'El respaldo no terminó: ' + message, severity='error')
            return {'status': 'blocked', 'error': message, 'off_host': False}


if __name__ == '__main__':
    from server import make_runtime
    store, providers, config = make_runtime()
    result = run(store, providers, config, '--requested' in sys.argv)
    print(json.dumps(result))
    if result.get('status') == 'blocked':raise SystemExit(2)
