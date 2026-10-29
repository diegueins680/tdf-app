"""Private, quota-checked Drive objects, with encrypted resumable transfer state."""
import hashlib
import json
import re
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

from backup_crypto import EncryptedSource
from domain import Denied, uid
from providers import NoRedirect, ProviderError, request

BASE = 'https://www.googleapis.com/drive/v3'
CHUNK = 8 * 1024 * 1024
RESERVE = 256 * 1024 * 1024


def safe_id(value):
    if not isinstance(value, str) or not re.fullmatch(r'[A-Za-z0-9_-]{1,200}', value):
        raise Denied('Identificador de Drive inválido.')
    return value


class DriveBackup:
    def __init__(self, store, providers, settings):
        self.store, self.providers, self.settings = store, providers, dict(settings)
        with store.db() as db:
            row = db.execute('SELECT * FROM accounts WHERE id=? AND platform="drive" '
                             'AND scope="tdf" AND revoked=0', (settings['account'],)).fetchone()
        if not row or row['owner'] != settings['owner']:
            raise Denied('Conecta el Google Drive autorizado para este respaldo.', 409)
        self.account = dict(row)
        self.access_token = None
        self.token_at = 0

    def token(self):
        if not self.access_token or time.time() - self.token_at > 2700:
            self.access_token = self.providers.token(self.account)
            self.token_at = time.time()
        return self.access_token

    def call(self, path, data=None, method=None):
        return request(BASE + path, self.token(), data, method)[0]

    def quota(self, needed):
        info = self.call('/about?fields=user(permissionId,emailAddress),storageQuota')
        if info.get('user', {}).get('permissionId') != self.account['external_id']:
            raise Denied('Drive respondió con una identidad diferente.', 409)
        quota = info.get('storageQuota', {})
        if 'usage' not in quota:
            raise Denied('No se pudo verificar el espacio de Drive; el respaldo no comenzó.', 409)
        used = int(quota['usage'])
        limit = int(quota['limit']) if 'limit' in quota else None
        available = None if limit is None else max(0, limit - used)
        if available is not None and needed + RESERVE > available:
            raise Denied('Drive no tiene espacio suficiente para el respaldo cifrado. '
                         'Se necesitan ' + str(needed) + ' bytes y una reserva de ' +
                         str(RESERVE) + '; hay ' + str(available) + ' disponibles. '
                         'No se contratará almacenamiento.', 409)
        return {'used_bytes': used, 'limit_bytes': limit, 'available_bytes': available,
                'estimated_new_bytes': needed, 'reserve_bytes': RESERVE}

    def private_folder(self, identifier):
        data = self.call('/files/' + safe_id(identifier) +
                         '?fields=id,mimeType,trashed,ownedByMe,permissions(id,type,role)')
        permissions = data.get('permissions', [])
        if (data.get('mimeType') != 'application/vnd.google-apps.folder'
                or data.get('trashed') or data.get('ownedByMe') is not True
                or not permissions or any(p.get('type') != 'user' or p.get('role') != 'owner'
                                         or p.get('id') != self.account['external_id'] for p in permissions)):
            raise Denied('El respaldo requiere una carpeta privada, sin accesos compartidos, y propiedad de la cuenta de TDF conectada.', 409)
        return data

    def folder(self):
        if self.settings.get('folder_id'):
            return self.private_folder(self.settings['folder_id'])['id']
        query = "trashed=false and mimeType='application/vnd.google-apps.folder' and appProperties has { key='tdfCampaignBackup' and value='root' }"
        found = self.call('/files?' + urllib.parse.urlencode({'q': query, 'fields': 'files(id)', 'pageSize': 10}))
        if len(found.get('files', [])) > 1:
            raise Denied('Hay varias carpetas de respaldo de TDF. Revisa cuál debe usarse.', 409)
        if found.get('files'):
            identifier = self.private_folder(found['files'][0]['id'])['id']
        else:
            created = self.call('/files?fields=id', {'name': 'TDF — Respaldos de campañas',
                                                     'mimeType': 'application/vnd.google-apps.folder',
                                                     'appProperties': {'tdfCampaignBackup': 'root'}})
            identifier = self.private_folder(created['id'])['id']
        with self.store.db(True) as db:
            db.execute('UPDATE backup_settings SET folder_id=? WHERE id=1 AND account=?',
                       (identifier, self.account['id']))
        self.settings['folder_id'] = identifier
        return identifier

    def enabled(self):
        with self.store.db() as db:
            current = db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
            connected=db.execute('SELECT revoked FROM accounts WHERE id=?',(self.account['id'],)).fetchone()
        if (not current or not current['enabled'] or not current['key_confirmed_at']
                or current['account'] != self.account['id']
                or current['fingerprint'] != self.settings['fingerprint'] or not connected or connected['revoked']):
            raise Denied('El respaldo se desactivó o cambió su cuenta o clave. Transferencia detenida.', 409)

    def progress(self,identity,offset,total):
        with self.store.db(True) as db:
            db.execute('INSERT INTO heartbeat VALUES(?,?,?) ON CONFLICT(name) DO UPDATE SET at=excluded.at,details=excluded.details',
                       ('backup-transfer',time.time(),json.dumps({'object':identity,'confirmed_bytes':offset,'total_bytes':total})))

    def upload_call(self, url, body, offset, total, probe=False):
        parsed = urllib.parse.urlsplit(url)
        if (parsed.scheme != 'https' or parsed.hostname != 'www.googleapis.com'
                or parsed.username or parsed.password or parsed.port not in (None,443) or parsed.path != '/upload/drive/v3/files'):
            raise ProviderError('drive_upload_destination_refused')
        content_range = 'bytes */' + str(total) if probe else f'bytes {offset}-{offset+len(body)-1}/{total}'
        req = urllib.request.Request(url, data=body, method='PUT',
                                     headers={'Authorization': 'Bearer ' + self.token(),
                                              'Content-Type': 'application/octet-stream',
                                              'Content-Length': str(len(body)), 'Content-Range': content_range})
        try:
            with urllib.request.build_opener(NoRedirect).open(req, timeout=120) as response:
                data = json.loads(response.read(100_000))
            return {'complete': True, 'id': safe_id(data['id']), 'offset': total}
        except urllib.error.HTTPError as error:
            if error.code == 308:
                acknowledged = error.headers.get('Range', '')
                if not acknowledged and probe:return {'complete': False, 'offset': 0}
                match = re.fullmatch(r'bytes=0-(\d+)', acknowledged)
                if not match or int(match[1]) + 1 > total:
                    raise ProviderError('drive_invalid_acknowledged_range') from None
                return {'complete': False, 'offset': int(match[1]) + 1}
            raise ProviderError('drive_upload_http_' + str(error.code), error.code >= 500) from None
        except (OSError, ValueError, KeyError):
            raise ProviderError('drive_upload_interrupted_resume_available') from None

    def object_metadata(self, identifier, expected_size, source_sha, marker):
        data = self.call('/files/' + safe_id(identifier) +
                         '?fields=id,size,trashed,parents,ownedByMe,appProperties')
        props = data.get('appProperties', {})
        if (data.get('trashed') or data.get('ownedByMe') is not True
                or self.settings['folder_id'] not in data.get('parents', [])
                or int(data.get('size', -1)) != expected_size
                or props.get('sourceSha256') != source_sha
                or props.get('recipient') != self.settings['fingerprint']
                or props.get('transferId') != marker):
            raise Denied('El objeto de Drive no coincide con el respaldo esperado.', 409)
        return data

    def put(self, source, source_sha, size):
        self.enabled()
        folder = self.folder()
        identity = hashlib.sha256(json.dumps([self.account['id'], folder, source_sha, size,
                                              self.settings['fingerprint']]).encode()).hexdigest()
        with self.store.db() as db:
            row = db.execute('SELECT * FROM backup_objects WHERE id=?', (identity,)).fetchone()
        if row and row['remote_id']:
            saved = self.providers.vault.open(row['upload_state'])
            self.object_metadata(row['remote_id'], row['cipher_size'], source_sha, saved['marker'])
            return {'id': row['remote_id'], 'source_sha256': source_sha, 'size': size,
                    'cipher_size': row['cipher_size'], 'recipient': self.settings['fingerprint'], 'reused': True}
        saved = self.providers.vault.open(row['upload_state']) if row else None
        encrypted = EncryptedSource(source, self.settings['public_key'], source_sha, size,
                                    saved['material'] if saved else None)
        if not saved:
            saved = {'marker': uid(), 'material': encrypted.material, 'url': None}
            with self.store.db(True) as db:
                db.execute('INSERT INTO backup_objects(id,account,folder_id,source_sha256,source_size,'
                           'recipient,cipher_size,upload_state,updated_at) VALUES(?,?,?,?,?,?,?,?,?)',
                           (identity, self.account['id'], folder, source_sha, size,
                            self.settings['fingerprint'], encrypted.total,
                            self.providers.vault.seal(saved), time.time()))
            metadata = {'name': 'object-' + identity + '.tdfbk', 'parents': [folder],
                        'mimeType': 'application/octet-stream',
                        'appProperties': {'tdfCampaignBackup': 'object', 'sourceSha256': source_sha,
                                          'recipient': self.settings['fingerprint'], 'transferId': saved['marker']}}
            _, headers = request('https://www.googleapis.com/upload/drive/v3/files?uploadType=resumable&fields=id',
                                 self.token(), metadata,
                                 headers={'X-Upload-Content-Length': str(encrypted.total),
                                          'X-Upload-Content-Type': 'application/octet-stream'})
            saved['url'] = headers.get('Location') or headers.get('location')
            if not saved['url']:raise ProviderError('drive_upload_session_missing', True)
            with self.store.db(True) as db:
                db.execute('UPDATE backup_objects SET upload_state=?,updated_at=? WHERE id=?',
                           (self.providers.vault.seal(saved), time.time(), identity))
            state = {'complete': False, 'offset': 0}
        elif saved.get('url'):
            state = self.upload_call(saved['url'], b'', 0, encrypted.total, probe=True)
        else:
            raise ProviderError('drive_session_initialization_outcome_unknown_requires_review', True)
        self.progress(identity,state['offset'],encrypted.total)
        while not state['complete']:
            self.enabled()
            offset = state['offset']
            if offset==encrypted.total:
                state=self.upload_call(saved['url'],b'',0,encrypted.total,probe=True)
                if not state['complete']:raise ProviderError('drive_full_range_not_finalized_requires_review',True)
                break
            body = encrypted.read_range(offset, min(CHUNK, encrypted.total - offset))
            previous = offset
            state = self.upload_call(saved['url'], body, offset, encrypted.total)
            self.progress(identity,state['offset'],encrypted.total)
            if not state['complete'] and state['offset'] <= previous:
                raise ProviderError('drive_upload_did_not_advance')
        remote_id = state['id']
        self.object_metadata(remote_id, encrypted.total, source_sha, saved['marker'])
        # Completed objects no longer retain symmetric per-upload secrets locally.
        with self.store.db(True) as db:
            db.execute('UPDATE backup_objects SET remote_id=?,verified_at=?,updated_at=?,upload_state=? WHERE id=?',
                       (remote_id, time.time(), time.time(), self.providers.vault.seal({'marker': saved['marker']}), identity))
        return {'id': remote_id, 'source_sha256': source_sha, 'size': size,
                'cipher_size': encrypted.total, 'recipient': self.settings['fingerprint'], 'reused': False}
