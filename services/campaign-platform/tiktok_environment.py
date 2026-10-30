"""Explicit, isolated TikTok sandbox policy; never implies production audit."""
import hashlib
import json
import re
from pathlib import Path
from domain import Denied


def mode(config):
    value = config.get('TIKTOK_MODE', 'production')
    if value not in ('production', 'sandbox'):
        raise Denied('Entorno de TikTok inválido.', 409)
    if value == 'sandbox' and (
            config.get('PUBLIC_BASE') != 'https://api.tdfrecords.net/campaign-platform-sandbox'
            or Path(config.get('STATE_DIR', '')).name != 'tdf-campaign-platform-sandbox'
            or config.get('TIKTOK_CLIENT_AUDITED') is True
            or type(config.get('TIKTOK_SANDBOX_ADMIN_ID')) is not int
            or config['TIKTOK_SANDBOX_ADMIN_ID'] <= 0
            or not re.fullmatch(r'[A-Za-z0-9_.]{1,24}', config.get('TIKTOK_SANDBOX_USERNAME', ''))):
        raise Denied('El sandbox necesita su propio espacio, aplicación sin auditoría y cuenta de prueba autorizada.', 409)
    return value


def actor(config, user):
    if mode(config) == 'sandbox' and (not user.get('admin') or user['id'] != config['TIKTOK_SANDBOX_ADMIN_ID']):
        raise Denied('Este entorno de prueba está reservado al administrador designado.', 403)


def connection(config, user, platform, scope):
    actor(config, user)
    if mode(config) == 'sandbox' and (platform != 'tiktok' or scope != 'tdf'):
        raise Denied('El sandbox admite únicamente la cuenta de TikTok de TDF autorizada para la prueba.', 403)


def binding(config):
    value = [mode(config), config.get('TIKTOK_CLIENT_KEY'), config.get('PUBLIC_BASE')]
    return hashlib.sha256(json.dumps(value, separators=(',', ':')).encode()).hexdigest()


def private_choice(config, value):
    if mode(config) == 'sandbox' and value.get('privacy') != 'SELF_ONLY':
        raise Denied('El sandbox solo permite una prueba privada seleccionada expresamente como Solo yo.', 409)
