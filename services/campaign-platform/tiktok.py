"""TikTok Direct Post protocol, blocked until audited client/domain configuration.

Only PULL_FROM_URL is used for this server's media. Initialization is the external
publication boundary: a timed-out initialization is never submitted again.
"""
import hashlib
import json
import math
import sqlite3
import re
import subprocess
import time
import urllib.parse
from pathlib import Path
import tiktok_environment as environment

from domain import Denied, uid
from providers import ProviderError, request

API = 'https://open.tiktokapis.com/v2'
PRIVACY = {'PUBLIC_TO_EVERYONE', 'MUTUAL_FOLLOW_FRIENDS', 'FOLLOWER_OF_CREATOR', 'SELF_ONLY'}
BOOLS = ('allow_comment', 'allow_duet', 'allow_stitch', 'commercial', 'brand_own', 'brand_paid', 'is_aigc')


def configured(providers):
    cfg = providers.config
    prefix = cfg['PUBLIC_BASE'].rstrip('/') + '/media/'
    if environment.mode(cfg) == 'sandbox' and Path(providers.store.path).parent.resolve() != Path(cfg['STATE_DIR']).resolve():
        raise Denied('El sandbox no puede utilizar la base de datos de otro entorno.', 409)
    if not cfg.get('TIKTOK_CLIENT_KEY') or not cfg.get('TIKTOK_CLIENT_SECRET'):
        raise Denied('La conexión con TikTok aún no está habilitada en TDF. Puedes guardar el borrador.', 409)
    if environment.mode(cfg) == 'production' and cfg.get('TIKTOK_CLIENT_AUDITED') is not True:
        raise Denied('TikTok aún no habilita la publicación desde esta plataforma. Puedes guardar el borrador; su visibilidad no cambiará automáticamente.', 409)
    if cfg.get('TIKTOK_VERIFIED_MEDIA_PREFIX') != prefix:
        raise Denied('TDF debe completar la verificación de archivos con TikTok. Puedes conservar el borrador.', 409)
    return prefix


def call(path, token, body):
    response, _ = request(API + path, token, body, method='POST')
    error = response.get('error', {})
    code = error.get('code')
    if code != 'ok':
        safe = code if isinstance(code, str) and re.fullmatch(r'[a-z_]{1,100}', code) else 'invalid_response'
        raise ProviderError('tiktok_' + safe)
    if not isinstance(response.get('data'), dict):
        raise ProviderError('tiktok_response_missing_data', True)
    return response['data']


def permission(account):
    grants = json.loads(account['permissions'])
    if isinstance(grants, str):grants = grants.split(',')
    if not isinstance(grants, list) or 'video.publish' not in grants:
        raise Denied('Reconecta TikTok y autoriza el permiso de publicación.', 409)


def account_for(providers, actor, campaign, identifier):
    with providers.store.db() as db:
        owner = providers.store.access(db, actor, campaign, 'edit')['owner']
        row = db.execute('SELECT * FROM accounts WHERE id=? AND platform="tiktok" AND revoked=0', (identifier,)).fetchone()
    if not row or (row['owner'] != owner and row['scope'] != 'tdf'):
        raise Denied('Cuenta de TikTok no disponible en esta campaña.', 404)
    return dict(row)


def creator(providers, account):
    configured(providers)
    permission(account)
    data = call('/post/publish/creator_info/query/', providers.token(account), {})
    options = data.get('privacy_level_options')
    if (not isinstance(options, list) or not options or any(x not in PRIVACY for x in options)
            or not isinstance(data.get('creator_nickname'), str)
            or not isinstance(data.get('creator_username'), str)
            or any(type(data.get(k)) is not bool for k in ('comment_disabled', 'duet_disabled', 'stitch_disabled'))
            or type(data.get('max_video_post_duration_sec')) is not int):
        raise ProviderError('tiktok_invalid_creator_information')
    result = {key: data[key] for key in ('creator_username', 'creator_nickname', 'privacy_level_options',
                                       'comment_disabled', 'duet_disabled', 'stitch_disabled',
                                       'max_video_post_duration_sec')}
    if environment.mode(providers.config) == 'sandbox':
        if data['creator_username'] != providers.config['TIKTOK_SANDBOX_USERNAME'] or 'SELF_ONLY' not in options:
            raise Denied('TikTok no confirmó la cuenta o la visibilidad privada autorizada para esta prueba.', 409)
        result['privacy_level_options'] = ['SELF_ONLY']
        result['environment'] = 'sandbox'
    return result


def review_context(providers, actor, campaign, identifier):
    environment.actor(providers.config, actor)
    account = account_for(providers, actor, campaign, identifier)
    data = creator(providers, account)
    token = providers.vault.seal({'purpose': 'tiktok_creator_review', 'actor': actor['id'],
                                  'campaign': campaign, 'account': identifier,
                                  'external_id': account['external_id'], 'at': time.time(), 'creator': data,
                                  'client_binding': environment.binding(providers.config)})
    return {'creator': data, 'context': token}


def units(text):return len(text.encode('utf-16-le')) // 2


def fields(piece):
    settings = json.loads(piece['settings']) if isinstance(piece['settings'], str) else piece['settings']
    if not isinstance(settings, dict) or not isinstance(settings.get('tiktok'), dict):raise Denied('Revisa los ajustes de TikTok.')
    value = dict(settings['tiktok'])
    value.pop('review_token', None)
    if (any(type(value.get(k)) is not bool for k in BOOLS)
            or value.get('privacy') not in PRIVACY or type(value.get('cover_index', 0)) is not int):
        raise Denied('Selecciona privacidad y controles de TikTok en el formulario de revisión.')
    if value['commercial'] != bool(value['brand_own'] or value['brand_paid']):
        raise Denied('Indica qué marca promociona el contenido antes de autorizarlo.')
    if value['brand_paid'] and value['privacy'] == 'SELF_ONLY':
        raise Denied('TikTok no permite contenido de colaboración pagada con visibilidad Solo yo.')
    if piece['format'] == 'photo' and (value['allow_duet'] or value['allow_stitch']):
        raise Denied('Duet y Stitch no se aplican a publicaciones de fotos.')
    if piece['format'] not in ('video', 'photo'):raise Denied('Formato de TikTok no admitido.')
    if units(piece['copy']) > (2200 if piece['format'] == 'video' else 4000):
        raise Denied('El texto excede el límite de TikTok. Revísalo antes de aprobar.')
    if units(str(value.get('title', ''))) > 90:raise Denied('El título de fotos de TikTok admite hasta 90 caracteres UTF-16.')
    return value


def choices(value, info):
    if value['privacy'] not in info['privacy_level_options']:
        raise Denied('Las opciones de privacidad de TikTok cambiaron. Vuelve a revisar la pieza.', 409)
    for name in ('comment', 'duet', 'stitch'):
        if value['allow_' + name] and info[name + '_disabled']:
            raise Denied('TikTok tiene desactivada esta interacción. Vuelve a revisar la pieza.', 409)


def assets_for(providers, piece, value):
    identifiers = [piece['asset']] if piece['format'] == 'video' else value.get('images', [])
    if (not isinstance(identifiers, list) or not 1 <= len(identifiers) <= (1 if piece['format'] == 'video' else 35)
            or any(not isinstance(x, str) for x in identifiers) or len(set(identifiers)) != len(identifiers)):
        raise Denied('Selecciona el video o entre una y 35 fotos revisadas de la campaña.')
    if piece['format'] == 'photo' and not 0 <= value.get('cover_index', 0) < len(identifiers):
        raise Denied('Selecciona una portada válida para las fotos.')
    with providers.store.db() as db:
        result = []
        for identifier in identifiers:
            row = db.execute('SELECT * FROM assets WHERE id=? AND campaign=? AND status="ready"',
                             (identifier, piece['campaign'])).fetchone()
            suffixes = {'.mp4', '.mov', '.webm'} if piece['format'] == 'video' else {'.jpg', '.jpeg', '.webp'}
            if not row or Path(row['name']).suffix.lower() not in suffixes or not row['sha256']:
                raise Denied('TikTok necesita archivos terminados, del formato admitido y de esta campaña.')
            result.append(dict(row))
    return result


def content_digest(piece, value, media):
    content = {k: piece[k] for k in ('campaign', 'account', 'format', 'asset', 'copy', 'destination')}
    content['due_at'] = float(piece['due_at'])
    content.update(options=value, media=[{k: a[k] for k in ('id', 'sha256', 'size')} for a in media])
    return hashlib.sha256(json.dumps(content, sort_keys=True, separators=(',', ':')).encode()).hexdigest()


def consent(providers, actor, campaign, data):
    environment.actor(providers.config, actor)
    account = account_for(providers, actor, campaign, data.get('account'))
    try:context = providers.vault.open(data.get('context', ''))
    except Exception:raise Denied('Consulta nuevamente las opciones actuales de TikTok.', 409) from None
    if (context.get('purpose') != 'tiktok_creator_review' or context.get('actor') != actor['id']
            or context.get('campaign') != campaign or context.get('account') != account['id']
            or context.get('external_id') != account['external_id']
            or context.get('client_binding') != environment.binding(providers.config)
            or not 0 <= time.time() - context.get('at', 0) <= 900):
        raise Denied('La revisión de TikTok venció o corresponde a otra cuenta.', 409)
    if (data.get('music_agreed') is not True or data.get('preview_confirmed') is not True
            or data.get('original_content_confirmed') is not True):
        raise Denied('Revisa el archivo y confirma contenido original, envío a TikTok y uso de música.')
    piece = {**data, 'campaign': campaign, 'platform': 'tiktok', 'account':account['id'],
             'asset':data.get('asset') or None,'copy':str(data.get('copy','')),
             'destination':str(data.get('destination') or ''),'settings':data.get('settings',{})}
    if type(piece.get('due_at')) not in (int,float) or not math.isfinite(piece['due_at']):raise Denied('Selecciona una fecha válida para TikTok.')
    value = fields(piece);environment.private_choice(providers.config, value);choices(value, context['creator'])
    if value['brand_paid'] and data.get('brand_policy_agreed') is not True:
        raise Denied('Confirma la política de contenido de marca de TikTok.')
    media = assets_for(providers, piece, value)
    token = providers.vault.seal({'purpose': 'tiktok_explicit_consent', 'actor': actor['id'],
                                  'campaign': campaign, 'account': account['id'],
                                  'external_id': account['external_id'], 'at': time.time(),
                                  'digest': content_digest(piece, value, media),
                                  'client_binding': environment.binding(providers.config)})
    with providers.store.db(True) as db:
        try:
            db.execute('INSERT INTO tiktok_consents(id,token_hash,context_hash,campaign,actor,digest,created_at) VALUES(?,?,?,?,?,?,?)',
                       (uid(),hashlib.sha256(token.encode()).hexdigest(),hashlib.sha256(data['context'].encode()).hexdigest(),campaign,actor['id'],content_digest(piece,value,media),time.time()))
        except sqlite3.IntegrityError:raise Denied('Esta revisión de cuenta ya fue utilizada. Actualiza las opciones de TikTok y vuelve a revisar.',409) from None
        providers.store.audit(db, actor['id'], 'tiktok.content_consent', campaign,
                              details={'account': account['id'], 'digest': content_digest(piece, value, media),
                                       'privacy': value['privacy'], 'music_agreed': True})
    return {'review_token': token}


def bind_piece_consent(db, actor, campaign, piece, version, settings):
    options = settings.get('tiktok', {})
    if not isinstance(options, dict):raise Denied('Ajustes de TikTok inválidos.')
    token = options.get('review_token')
    if not token:return  # Unreviewed drafts remain possible before connecting an account.
    if not isinstance(token, str):raise Denied('Consentimiento de TikTok inválido.')
    changed = db.execute('UPDATE tiktok_consents SET piece=?,version=? WHERE token_hash=? '
                         'AND campaign=? AND actor=? AND piece IS NULL AND created_at>?',
                         (piece, version, hashlib.sha256(token.encode()).hexdigest(), campaign,
                          actor['id'], time.time()-900)).rowcount
    if changed != 1:raise Denied('El consentimiento de TikTok ya fue utilizado o venció. Vuelve a revisar esta pieza.',409)


def check_media(providers, piece, media, info):
    for asset in media:
        path = Path(providers.config['STATE_DIR']) / 'assets' / asset['path']
        if path.is_symlink() or path.parent != Path(providers.config['STATE_DIR']) / 'assets':
            raise Denied('Ruta de archivo no admitida.')
        if path.stat().st_size != asset['size']:raise Denied('El archivo cambió; vuelve a revisarlo.', 409)
        digest = hashlib.sha256()
        with path.open('rb') as stream:
            for block in iter(lambda: stream.read(1024*1024), b''):digest.update(block)
        if digest.hexdigest() != asset['sha256']:raise Denied('El archivo cambió; vuelve a revisarlo.', 409)
        try:
            meta = json.loads(subprocess.run(['ffprobe', '-v', 'error', '-show_streams', '-show_format', '-of', 'json', str(path)],
                                             capture_output=True, check=True, timeout=30).stdout)
            video = next(s for s in meta['streams'] if s.get('codec_type') == 'video')
            w, h = int(video['width']), int(video['height'])
            if piece['format'] == 'video':
                duration = float(meta['format']['duration'])
                numerator, denominator = map(int, video['avg_frame_rate'].split('/'));fps = numerator / denominator
                valid = (360 <= w <= 4096 and 360 <= h <= 4096 and 23 <= fps <= 60
                         and 0 < duration <= info['max_video_post_duration_sec'] and asset['size'] <= 4_000_000_000
                         and video.get('codec_name') in ('h264', 'hevc', 'vp8', 'vp9'))
            else:valid = 0 < min(w, h) <= 1080 and max(w, h) <= 1920 and asset['size'] <= 20_000_000 and video.get('codec_name') in ('mjpeg', 'webp')
        except (OSError, ValueError, KeyError, StopIteration, ZeroDivisionError, subprocess.SubprocessError):
            raise Denied('No se pudo verificar el formato real del material de TikTok.') from None
        if not valid:raise Denied('El archivo supera las opciones de duración, tamaño o formato de esta cuenta de TikTok.')


def publish(providers, piece, account, media_url=None):
    configured(providers);permission(account)
    value = fields(piece);environment.private_choice(providers.config, value);media = assets_for(providers, piece, value)
    try:saved_consent = providers.vault.open(json.loads(piece['settings'])['tiktok']['review_token'])
    except Exception:raise Denied('Completa la revisión y el consentimiento de TikTok para esta pieza.', 409) from None
    if (saved_consent.get('purpose') != 'tiktok_explicit_consent'
            or saved_consent.get('campaign') != piece['campaign'] or saved_consent.get('account') != account['id']
            or saved_consent.get('external_id') != account['external_id']
            or saved_consent.get('client_binding') != environment.binding(providers.config)
            or saved_consent.get('digest') != content_digest(piece, value, media)):
        raise Denied('La pieza o su cuenta cambiaron después del consentimiento de TikTok.', 409)
    with providers.store.db() as db:
        binding=db.execute('SELECT piece,version FROM tiktok_consents WHERE token_hash=?',
                           (hashlib.sha256(json.loads(piece['settings'])['tiktok']['review_token'].encode()).hexdigest(),)).fetchone()
        if not binding or binding['piece']!=piece['id'] or binding['version']!=piece['version']:
            raise Denied('Este consentimiento de TikTok no corresponde a esta pieza y versión.',409)
        if db.execute('SELECT 1 FROM provider_sessions WHERE piece=?', (piece['id'],)).fetchone():
            raise ProviderError('tiktok_existing_effect_requires_reconciliation', True)
    info = creator(providers, account);choices(value, info);check_media(providers, piece, media, info)
    post = {'privacy_level': value['privacy'], 'disable_comment': not value['allow_comment'],
            'brand_content_toggle': value['brand_paid'], 'brand_organic_toggle': value['brand_own']}
    from server import signed_media_url
    urls = [signed_media_url(providers.config, item['id']) for item in media]
    if any(not url.startswith(configured(providers)) for url in urls):raise Denied('Los archivos no usan el prefijo verificado de TikTok.')
    if piece['format'] == 'video':
        post.update(title=piece['copy'], disable_duet=not value['allow_duet'], disable_stitch=not value['allow_stitch'], is_aigc=value['is_aigc'])
        body = {'post_info': post, 'source_info': {'source': 'PULL_FROM_URL', 'video_url': urls[0]}}
        endpoint = '/post/publish/video/init/'
    else:
        post.update(title=value.get('title', ''), description=piece['copy'], auto_add_music=False)
        body = {'media_type': 'PHOTO', 'post_mode': 'DIRECT_POST', 'post_info': post,
                'is_aigc': value['is_aigc'], 'source_info': {'source': 'PULL_FROM_URL',
                'photo_images': urls, 'photo_cover_index': value.get('cover_index', 0)}}
        endpoint = '/post/publish/content/init/'
    access_token = providers.token(account)
    providers.effect_guard(piece)
    session = {'platform': 'tiktok', 'version': piece['version'], 'account': account['id'],
               'created_at': time.time(), 'privacy': value['privacy'], 'phase': 'init_requested',
               'client_binding': environment.binding(providers.config)}
    with providers.store.db(True) as db:
        db.execute('INSERT INTO provider_sessions VALUES(?,?,?)', (piece['id'], providers.vault.seal(session), time.time()))
    providers.effect_guard(piece)
    result = call(endpoint, access_token, body)
    identifier = result.get('publish_id')
    if not isinstance(identifier, str) or not 1 <= len(identifier) <= 64:raise ProviderError('tiktok_missing_publish_identity', True)
    session.update(phase='processing', publish_id=identifier)
    with providers.store.db(True) as db:
        db.execute('UPDATE provider_sessions SET payload=?,updated_at=? WHERE piece=?', (providers.vault.seal(session), time.time(), piece['id']))
        db.execute('UPDATE pieces SET external_id=?,status="processing" WHERE id=?', (identifier, piece['id']))
        db.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?)',
                   (uid(), piece['campaign'], piece['id'], 'verify_tiktok', '{}', time.time()+60))
    return {'state': 'processing', 'external_id': identifier, 'url': None}


def verify(providers, piece, account):
    with providers.store.db() as db:row = db.execute('SELECT payload FROM provider_sessions WHERE piece=?', (piece['id'],)).fetchone()
    session = providers.vault.open(row[0]) if row else {}
    if (session.get('platform') != 'tiktok' or session.get('publish_id') != piece['external_id']
            or session.get('client_binding') != environment.binding(providers.config)
            or session.get('account') != account['id'] or session.get('version') != piece['version']):
        raise Denied('El seguimiento de TikTok no corresponde a esta pieza.', 409)
    if time.time()-session['created_at'] > 86400:raise Denied('TikTok requiere una revisión de estado; no se volverá a enviar el material.', 409)
    result = call('/post/publish/status/fetch/', providers.token(account), {'publish_id': session['publish_id']})
    state = result.get('status')
    if state == 'FAILED':raise Denied('TikTok rechazó o no pudo procesar el contenido. Revisa la restricción; no se reenviará.', 409)
    if state in ('PROCESSING_UPLOAD', 'PROCESSING_DOWNLOAD'):return {'state': 'waiting_for_tiktok'}
    if state != 'PUBLISH_COMPLETE':raise ProviderError('tiktok_unrecognized_post_status')
    if session['privacy'] != 'PUBLIC_TO_EVERYONE':return {'state': 'published_restricted', 'url': None}
    identifiers = result.get('publicaly_available_post_id', [])
    if not isinstance(identifiers, list) or not identifiers:return {'state': 'waiting_for_tiktok'}
    if any(not re.fullmatch(r'[0-9]+', str(x)) for x in identifiers):raise ProviderError('tiktok_invalid_public_post_identity')
    # No invented profile/video URL. Independent playback remains a separate step.
    return {'state': 'published', 'url': None, 'public_post_ids': [str(x) for x in identifiers]}
