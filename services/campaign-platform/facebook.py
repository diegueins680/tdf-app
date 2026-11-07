"""Page publishing through Meta's documented Graph API. No browser automation.

Video publication is separate from upload. Its durable receipt is written before
the final provider boundary; uncertain outcomes are never published again.
"""
import json
import re
import time
import urllib.parse
from pathlib import Path

from domain import Denied, uid
from providers import ProviderError, request


def checkpoint(providers, piece, data):
    data = {**data, 'version': piece['version'], 'account': piece['account']}
    with providers.store.db(True) as db:
        db.execute('INSERT INTO provider_sessions VALUES(?,?,?) ON CONFLICT(piece) '
                   'DO UPDATE SET payload=excluded.payload,updated_at=excluded.updated_at',
                   (piece['id'], providers.vault.seal(data), time.time()))


def session(providers, piece):
    with providers.store.db() as db:
        row = db.execute('SELECT payload FROM provider_sessions WHERE piece=?',
                         (piece['id'],)).fetchone()
    return providers.vault.open(row[0]) if row else None


def remember(providers, piece, external_id):
    with providers.store.db(True) as db:
        db.execute('UPDATE pieces SET external_id=?,status="processing" WHERE id=?',
                   (external_id, piece['id']))


def owned_assets(providers, piece, identifiers, suffixes):
    if not identifiers or any(not isinstance(value, str) for value in identifiers):
        raise Denied('Selecciona los archivos revisados de esta campaña.')
    result = []
    with providers.store.db() as db:
        for identifier in identifiers:
            row = db.execute('SELECT * FROM assets WHERE id=? AND campaign=? '
                             'AND status="ready"', (identifier, piece['campaign'])).fetchone()
            if not row or Path(row['name']).suffix.lower() not in suffixes:
                raise Denied('Facebook necesita archivos del formato indicado y de esta campaña.')
            result.append(dict(row))
    return result


def permalink(value):
    """Keep only a provider-returned Facebook address; never invent a public URL."""
    parsed = urllib.parse.urlsplit(value or '')
    return value if (parsed.scheme == 'https' and parsed.hostname in
                     ('facebook.com', 'www.facebook.com', 'm.facebook.com',
                      'fb.watch') and not parsed.username and not parsed.password) else None


def finish_photo_post(providers, piece, account, token, result):
    identifier = result.get('post_id') or result.get('id')
    if not identifier:
        raise ProviderError('facebook_missing_publication_identity', True)
    remember(providers, piece, identifier)
    checkpoint(providers, piece, {'kind': 'post', 'phase': 'provider_acknowledged',
                                  'external_id': identifier})
    data, _ = request(providers.graph + '/' + identifier + '?fields=id,permalink_url', token)
    return {'external_id': identifier, 'url': permalink(data.get('permalink_url')),
            'state': 'published'}  # Provider acknowledgment, not independent public playback.


def publish(providers, piece, account, token, media_url):
    permissions = json.loads(account['permissions'])
    if not isinstance(permissions, list) or 'pages_manage_posts' not in permissions:
        raise Denied('Reconecta Facebook y concede el permiso de administrar publicaciones de la página.', 409)
    if session(providers, piece):
        raise ProviderError('facebook_existing_effect_requires_reconciliation', True)
    fmt = piece['format']
    settings = json.loads(piece['settings'])
    page_url = providers.graph + '/' + account['external_id']
    providers.effect_guard(piece)
    if fmt == 'story':
        raise Denied('Las Stories de Facebook requieren el paso nativo para conservar el enlace y verificar su publicación.', 409)
    if fmt == 'post' and not piece['asset']:
        providers.effect_guard(piece)
        result, _ = request(page_url + '/feed', token,
                            {'message': piece['copy'], 'link': piece['destination']})
        return finish_photo_post(providers, piece, account, token, result)
    if piece['destination'] and piece['destination'] not in piece['copy']:
        raise Denied('Incluye el enlace de destino en el texto de Facebook antes de aprobar la pieza.')
    if fmt in ('post', 'carousel'):
        identifiers = [piece['asset']] if fmt == 'post' else settings.get('children', [])
        if not isinstance(identifiers, list) or not (1 if fmt == 'post' else 2) <= len(identifiers) <= 10:
            raise Denied('Selecciona una imagen o entre dos y diez imágenes para la publicación múltiple.')
        # Validate every child before transferring any image to the provider.
        images = owned_assets(providers, piece, identifiers, {'.jpg', '.jpeg', '.png'})
        from server import signed_media_url
        photos = []
        for image in images:
            providers.effect_guard(piece)
            result, _ = request(page_url + '/photos', token,
                                {'url': signed_media_url(providers.config, image['id']),
                                 'published': False})
            if not result.get('id'):
                raise ProviderError('facebook_photo_identity_missing', True)
            photos.append(result['id'])
            checkpoint(providers, piece, {'kind': fmt, 'phase': 'private_photos', 'photos': photos})
        providers.effect_guard(piece)
        checkpoint(providers, piece, {'kind': fmt, 'phase': 'publish_requested', 'photos': photos})
        result, _ = request(page_url + '/feed', token,
                            {'message': piece['copy'],
                             'attached_media': [{'media_fbid': identifier} for identifier in photos]})
        return finish_photo_post(providers, piece, account, token, result)
    if fmt not in ('video', 'reel'):
        raise Denied('Este formato de Facebook requiere publicación asistida.', 409)
    owned_assets(providers, piece, [piece['asset']], {'.mp4'})
    if not media_url:
        raise Denied('Falta el video revisado.')
    if fmt == 'video':
        # published=False prevents processing from silently making it public.
        result, _ = request(page_url + '/videos', token,
                            {'file_url': media_url, 'published': False,
                             'title': str(settings.get('title') or piece['name'])[:255],
                             'description': piece['copy']})
        identifier = result.get('id')
        if not identifier:
            raise ProviderError('facebook_video_identity_missing', True)
        remember(providers, piece, identifier)
    else:
        created, _ = request(page_url + '/video_reels', token, {'upload_phase': 'start'})
        identifier = created.get('video_id')
        upload_url = created.get('upload_url', '')
        remember(providers, piece, identifier)
        checkpoint(providers, piece, {'kind': fmt, 'phase': 'upload_created',
                                     'external_id': identifier})
        parsed = urllib.parse.urlsplit(upload_url)
        if (not identifier or not re.fullmatch(r'[0-9]+', identifier)
                or parsed.scheme != 'https' or parsed.hostname != 'rupload.facebook.com'
                or parsed.username or parsed.password
                or parsed.path != '/video-upload/' + providers.config.get('META_API_VERSION', 'v23.0') + '/' + identifier):
            raise ProviderError('facebook_upload_destination_refused', True)
        providers.effect_guard(piece)
        uploaded, _ = request(upload_url, data=b'', method='POST',
                              headers={'Authorization': 'OAuth ' + token, 'file_url': media_url})
        if uploaded.get('success') is not True:
            raise ProviderError('facebook_upload_acknowledgment_missing', True)
    checkpoint(providers, piece, {'kind': fmt, 'phase': 'uploaded', 'external_id': identifier})
    with providers.store.db(True) as db:
        db.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?) '
                   'ON CONFLICT(piece,kind) DO NOTHING',
                   (uid(), piece['campaign'], piece['id'], 'verify_facebook', '{}', time.time() + 30))
    return {'external_id': identifier, 'url': None, 'state': 'processing'}


def verify(providers, piece, account):
    if time.time()>piece['due_at']+86400:raise Denied('La verificación de Facebook requiere revisión después de 24 horas; no se reenviará la publicación.',409)
    saved = session(providers, piece)
    if (not saved or saved.get('account') != piece['account']
            or saved.get('version') != piece['version']
            or saved.get('external_id') != piece['external_id']):
        raise Denied('La carga de Facebook no corresponde a esta versión y cuenta.', 409)
    token = providers.token(account)
    data, _ = request(providers.graph + '/' + piece['external_id'] +
                      '?fields=id,status,published,permalink_url', token)
    status = data.get('status', {})
    if status.get('video_status') in ('error', 'expired', 'upload_failed'):
        raise Denied('Facebook rechazó o no pudo procesar el video. Revisa la restricción en Meta; no se volverá a cargar.', 409)
    if data.get('published') is True and status.get('video_status') == 'ready':
        return {'state': 'published', 'external_id': piece['external_id'],
                'url': permalink(data.get('permalink_url'))}
    if status.get('video_status') != 'ready':
        return {'state': 'waiting_for_facebook'}
    if saved['phase'] in ('publish_requested', 'provider_acknowledged'):
        # Read-only reconciliation. A previous finish must never be sent twice.
        return {'state': 'waiting_for_facebook'}
    if saved['phase'] != 'uploaded':
        raise Denied('La carga de Facebook necesita revisión antes de publicar.', 409)
    providers.effect_guard(piece)
    checkpoint(providers, piece, {**saved, 'phase': 'publish_requested'})
    if saved['kind'] == 'reel':
        result, _ = request(providers.graph + '/' + account['external_id'] + '/video_reels', token,
                            {'video_id': piece['external_id'], 'upload_phase': 'finish',
                             'video_state': 'PUBLISHED', 'description': piece['copy'],
                             'title': json.loads(piece['settings']).get('title', piece['name'])})
    else:
        result, _ = request(providers.graph + '/' + piece['external_id'], token, {'published': True})
    if result.get('success') is not True:
        raise ProviderError('facebook_finish_outcome_unknown', True)
    checkpoint(providers, piece, {**saved, 'phase': 'provider_acknowledged'})
    return {'state': 'waiting_for_facebook'}
