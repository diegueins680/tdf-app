"""Serialize TikTok refresh-token rotation across API and worker processes."""
import fcntl
import json
import os
import re
import time
import urllib.parse
from pathlib import Path
from domain import Denied
from providers import ProviderError, request


def refresh(providers, account):
    if not re.fullmatch(r'[0-9a-f]{32}', account['id']):raise Denied('Cuenta inválida.')
    path = Path(providers.config['STATE_DIR']) / ('oauth-refresh-' + account['id'] + '.lock')
    fd = os.open(path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, 'a+b') as lock:
        fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
        with providers.store.db() as db:
            row = db.execute('SELECT * FROM accounts WHERE id=? AND platform="tiktok" AND revoked=0', (account['id'],)).fetchone()
        if not row:raise Denied('La conexión de TikTok fue revocada.', 409)
        token = providers.vault.open(row['credentials'])
        from tiktok_environment import mode, binding
        saved_binding = token.get('_tdf_tiktok_client_binding')
        if ((mode(providers.config) == 'sandbox' or saved_binding is not None)
                and saved_binding != binding(providers.config)):
            raise Denied('Reconecta TikTok en el entorno y aplicación que corresponden.', 409)
        if row['expires_at'] and row['expires_at'] > time.time() + 300:
            return token['access_token']
        if not token.get('refresh_token'):
            raise Denied('Reconecta TikTok para renovar el acceso.', 409)
        if not providers.config.get('TIKTOK_CLIENT_KEY') or not providers.config.get('TIKTOK_CLIENT_SECRET'):
            raise Denied('Falta configurar la aplicación de TikTok.', 409)
        body = {'client_key': providers.config['TIKTOK_CLIENT_KEY'],
                'client_secret': providers.config['TIKTOK_CLIENT_SECRET'],
                'grant_type': 'refresh_token', 'refresh_token': token['refresh_token']}
        fresh, _ = request('https://open.tiktokapis.com/v2/oauth/token/',
                            data=urllib.parse.urlencode(body).encode(),
                            headers={'Content-Type': 'application/x-www-form-urlencoded'})
        if (not isinstance(fresh.get('access_token'), str) or not fresh.get('refresh_token')
                or not isinstance(fresh.get('expires_in'), int) or fresh['expires_in'] <= 0
                or fresh.get('open_id') != account['external_id']):
            raise ProviderError('tiktok_token_refresh_not_confirmed')
        token.update(fresh)
        with providers.store.db(True) as db:
            changed = db.execute('UPDATE accounts SET credentials=?,expires_at=?,permissions=? '
                                 'WHERE id=? AND revoked=0 AND credentials=?',
                                 (providers.vault.seal(token), time.time()+fresh['expires_in'],
                                  json.dumps(fresh.get('scope', '')), account['id'], row['credentials'])).rowcount
            if changed != 1:raise Denied('La conexión cambió durante la renovación. Revisa TikTok.', 409)
        return fresh['access_token']
