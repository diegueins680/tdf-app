#!/usr/bin/env python3
"""Bounded, source-fenced media checks. Never fetch caller supplied URLs from HTTP handlers."""
import hashlib
import hmac
import http.client
import ipaddress
import json
import os
import re
import socket
import ssl
import subprocess
import sys
import time
import uuid
from datetime import datetime, timezone
from urllib.parse import quote, urljoin, urlsplit

MAX_BYTES = 1024 * 1024
DEADLINE_SECONDS = 25

class ProbeFailure(Exception):
    def __init__(self, reason, permanent=False):
        self.reason, self.permanent = reason, permanent


def public_addresses(host):
    # DNS runs in a killable child, including a resolver that ignores timeouts.
    result = subprocess.run([sys.executable, '-c',
        'import socket,json,sys;print(json.dumps(sorted({x[4][0] for x in socket.getaddrinfo(sys.argv[1],443,type=socket.SOCK_STREAM)})))', host],
        capture_output=True, text=True, timeout=4, check=True)
    addresses = json.loads(result.stdout)
    if not addresses or any(not safe_address(value) for value in addresses):
        raise ProbeFailure('unsafe_url')
    return addresses


def safe_address(value):
    ip = ipaddress.ip_address(value)
    # IPv4 translation can route a seemingly global IPv6 answer to a private
    # IPv4 destination (RFC 6052). Refuse these and transition mechanisms.
    if ip.version == 6 and any(ip in ipaddress.ip_network(prefix) for prefix in
        ('64:ff9b::/96', '64:ff9b:1::/48', '::ffff:0:0:0/96')):
        return False
    return (ip.is_global and not ip.is_multicast and not ip.is_unspecified
            and not getattr(ip, 'ipv4_mapped', None) and not getattr(ip, 'sixtofour', None)
            and not getattr(ip, 'teredo', None))


def validated_url(url):
    parsed = urlsplit(url)
    try:
        if parsed.scheme != 'https' or not parsed.hostname or parsed.username or parsed.password or parsed.port not in (None, 443):
            raise ValueError()
        if any(ord(c) < 33 or ord(c) == 127 for c in url) or len(url) > 8192:
            raise ValueError()
    except ValueError:
        raise ProbeFailure('unsafe_url') from None
    return parsed


def s3_headers(url, env):
    """Sign only the operator configured object store, never legacy redirects."""
    p = validated_url(url)
    now = datetime.now(timezone.utc)
    timestamp, date = now.strftime('%Y%m%dT%H%M%SZ'), now.strftime('%Y%m%d')
    region = env['MUSIC_S3_REGION']
    payload = hashlib.sha256(b'').hexdigest()
    headers = {'host': p.netloc, 'x-amz-content-sha256': payload, 'x-amz-date': timestamp}
    if env.get('MUSIC_S3_SESSION_TOKEN'):
        headers['x-amz-security-token'] = env['MUSIC_S3_SESSION_TOKEN']
    names = ';'.join(sorted(headers))
    canonical = '\n'.join(['GET', p.path, p.query, ''.join(k + ':' + headers[k] + '\n' for k in sorted(headers)), names, payload])
    scope = date + '/' + region + '/s3/aws4_request'
    sign = '\n'.join(['AWS4-HMAC-SHA256', timestamp, scope, hashlib.sha256(canonical.encode()).hexdigest()])
    key = ('AWS4' + env['MUSIC_S3_SECRET_ACCESS_KEY']).encode()
    for part in [date, region, 's3', 'aws4_request']:
        key = hmac.new(key, part.encode(), hashlib.sha256).digest()
    headers['Authorization'] = ('AWS4-HMAC-SHA256 Credential=' + env['MUSIC_S3_ACCESS_KEY_ID'] + '/' + scope
                                + ', SignedHeaders=' + names + ', Signature=' + hmac.new(key, sign.encode(), hashlib.sha256).hexdigest())
    return headers


def fetch_prefix(url, *, signed=False, env=None, resolver=public_addresses, connection=http.client.HTTPSConnection):
    deadline = time.monotonic() + DEADLINE_SECONDS
    for redirect in range(4):
        p = validated_url(url)
        addresses = resolver(p.hostname)
        if not addresses or any(not safe_address(x) for x in addresses):
            raise ProbeFailure('unsafe_url')
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise ProbeFailure('timeout')
        conn = connection(p.hostname, timeout=min(8, remaining), context=ssl.create_default_context())
        # Preserve original TLS SNI/certificate and Host while pinning the TCP
        # peer to an already checked address. No second DNS lookup or proxy use.
        conn._create_connection = lambda _address, timeout=None, source_address=None: socket.create_connection(
            (addresses[0], 443), timeout=timeout, source_address=source_address)
        try:
            headers = {'Range': 'bytes=0-' + str(MAX_BYTES - 1), 'Accept-Encoding': 'identity'}
            if signed:
                headers.update(s3_headers(url, env))
            conn.request('GET', p.path + ('?' + p.query if p.query else ''), headers=headers)
            response = conn.getresponse()
            code = response.status
            if code in (301, 302, 303, 307, 308):
                # Do not forward object-store credentials to any redirect.
                if signed or redirect == 3:
                    raise ProbeFailure('transport')
                url = urljoin(url, response.getheader('Location', ''))
                continue
            if code in (401, 403):
                raise ProbeFailure('authentication')
            if code in (404, 410):
                raise ProbeFailure('missing', permanent=True)
            if code == 429 or code >= 500:
                raise ProbeFailure('server')
            if code not in (200, 206) or response.getheader('Content-Encoding', 'identity') != 'identity':
                raise ProbeFailure('transport')
            media = response.getheader('Content-Type', '').split(';')[0].strip().lower()
            if media.startswith('text/') or media in ('application/json', 'application/xhtml+xml'):
                raise ProbeFailure('unsupported', permanent=True)
            # An absolute deadline also bounds servers dribbling a byte at a time.
            data = bytearray()
            ended = False
            while len(data) < MAX_BYTES:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise ProbeFailure('timeout')
                if conn.sock:
                    conn.sock.settimeout(min(3, remaining))
                block = response.read1(min(65536, MAX_BYTES - len(data)))
                if not block:
                    ended = True
                    break
                data.extend(block)
            length = response.getheader('Content-Length', '')
            whole_range = re.fullmatch(r'bytes 0-(\d+)/(\d+)', response.getheader('Content-Range', ''))
            complete = (code == 200 and (ended or (length.isdigit() and int(length) == len(data))))
            if code == 206 and whole_range:
                last, total = map(int, whole_range.groups())
                complete = last + 1 == total == len(data) and total <= MAX_BYTES
            return bytes(data), complete
        finally:
            conn.close()
    raise ProbeFailure('transport')


def probe(url, *, signed=False, env=None):
    try:
        data, complete = fetch_prefix(url, signed=signed, env=env)
        # Feed only these bytes through a pipe. Media containers cannot open
        # network URLs or local file references, including nested playlists.
        check = subprocess.run(['ffprobe', '-v', 'error', '-protocol_whitelist', 'pipe',
            '-show_entries', 'stream=codec_type,codec_name', '-of', 'json', 'pipe:0'],
            input=data, capture_output=True, timeout=5, check=False)
        try:
            streams = json.loads(check.stdout).get('streams', [])
        except (ValueError, TypeError):
            streams = []
        if check.returncode == 0 and any(s.get('codec_type') == 'audio' and s.get('codec_name') for s in streams):
            decode = subprocess.run(['ffmpeg', '-nostdin', '-v', 'error', '-protocol_whitelist', 'pipe',
                '-i', 'pipe:0', '-map', '0:a:0', '-t', '0.25', '-f', 'null', '-'],
                input=data, capture_output=True, timeout=5, check=False)
            if decode.returncode == 0:
                return 'valid', 'decodable'
        return ('permanent', 'unsupported') if complete else ('temporary', 'inconclusive')
    except ProbeFailure as error:
        return ('permanent' if error.permanent else 'temporary'), error.reason
    except (TimeoutError, subprocess.TimeoutExpired, socket.timeout):
        return 'temporary', 'timeout'
    except (OSError, ValueError, KeyError, subprocess.SubprocessError, http.client.HTTPException):
        return 'temporary', 'transport'


def sql(statement, **variables):
    result = subprocess.run(['psql', '-XAtq', '-v', 'ON_ERROR_STOP=1', '--dbname', os.environ['DATABASE_URL']]
        + [arg for k,v in variables.items() for arg in ['-v', k+'='+str(v)]], input=statement,
        capture_output=True, text=True, timeout=10, check=True)
    return result.stdout.strip()


def main():
    # One source, one network attempt, no eager media download on a phone.
    sql("SET statement_timeout='8s'; SET lock_timeout='2s'; SELECT music_media_reconcile(100);")
    token = str(uuid.uuid4())
    raw = sql("SET statement_timeout='8s'; SET lock_timeout='2s'; SELECT music_media_claim(:'token'::uuid);", token=token)
    if not raw or raw == 'null':
        return
    source = json.loads(raw)
    location = source['locator']
    if source['kind'] == 'asset':
        endpoint = os.environ['MUSIC_S3_ENDPOINT'].rstrip('/')
        # Encode every object path segment, including '?' and '#'. No URL from
        # catalog metadata chooses the operator's S3 host or supplies credentials.
        url = endpoint + '/' + quote(location['bucket'], safe='') + '/' + quote(location['key'], safe='/')
        outcome, reason = probe(url, signed=True, env=os.environ)
    else:
        outcome, reason = probe(location['url'])
    sql("SET statement_timeout='8s'; SET lock_timeout='2s'; SELECT music_media_observe(:'token'::uuid,:'outcome',:'reason');",
        token=token, outcome=outcome, reason=reason)
    print(json.dumps({'event':'music_media_validation','kind':source['kind'],'outcome':outcome,'reason':reason}))

if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, KeyError, subprocess.SubprocessError):
        # Never print URLs, credentials, database diagnostics or signed queries.
        print('Music media validation could not complete; its lease will expire.', file=sys.stderr)
        sys.exit(1)
