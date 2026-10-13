"""AUTH-PUBLIC-001 handler enforcement over the real backend.

Every route registered in formal/system/public-routes.json under a boundary
that requires the handler to deny a caller without its capability (a lookup
secret header, a random path capability, a session, the operator secret, a
provider signature or the Meta verify token) is requested twice: without the
capability and with a wrong one. No such request may succeed, none may be a
server fault, and a wrong session token must receive exactly the anonymous
answer. The marketplace cart, whose path identifier is the capability, is
created and read with the real, a random and a sequential identifier.
"""
import json
import urllib.error
import urllib.request
import uuid

DENY_BOUNDARIES = {'capability-header', 'capability-path', 'handler-session', 'operator-secret',
                   'signed-provider-callback', 'provider-verification-challenge'}
# Answers that deny: client errors, or the provider itself being unavailable
# (the fixture configures no payment provider or Meta verify token).
DENIED = {400, 401, 403, 404, 405, 415, 422, 503}
PRIVATE_MARKERS = ('PRIVATE_SYNTHETIC_NOTE', 'fixture-admin', 'lookup_token_hash')


def verify_public_route_denial(root, port, check):
    register = json.loads((root / 'formal/system/public-routes.json').read_text())
    routes = [entry for entry in register['routes'] if entry['boundary'] in DENY_BOUNDARIES]
    assert len(routes) >= 80, len(routes)

    def call(method, path, headers=None, body=None):
        data = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request('http://127.0.0.1:' + str(port) + path, data=data, method=method,
                                     headers={'Content-Type': 'application/json', **(headers or {})})
        opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        try:
            with opener.open(req, timeout=30) as response:
                return response.status, response.read().decode()
        except urllib.error.HTTPError as error:
            return error.code, error.read().decode()

    def concrete(path):
        # Sequential identifiers probe the first record; capability paths and
        # slugs get values no record can carry.
        out = ''
        for piece in path.split('/'):
            if piece.startswith('{') and piece.endswith('}'):
                name = piece[1:-1]
                piece = str(uuid.uuid4()) if name in ('cartId', 'token') else 'synthetic-slug' if name == 'slug' else '1'
            out += ('/' if out or path.startswith('/') else '') + piece if piece else ''
        return out or '/'

    def wrong_capability(entry):
        header = entry.get('header')
        if header:
            value = 'sha256=' + '0' * 64 if 'Signature' in header or 'Sig' in header else str(uuid.uuid4())
            return {header: value}, ''
        if entry['boundary'] == 'handler-session':
            return {'Authorization': 'Bearer ' + str(uuid.uuid4())}, ''
        if entry['boundary'] == 'provider-verification-challenge':
            return {}, '?hub.mode=subscribe&hub.verify_token=' + str(uuid.uuid4()) + '&hub.challenge=synthetic-challenge'
        return {}, ''

    faults, successes, leaks = [], [], []
    for entry in routes:
        method, path = entry['method'], concrete(entry['path'])
        body = {} if method in ('POST', 'PUT', 'PATCH') else None
        anonymous = call(method, path, None, body)
        headers, query = wrong_capability(entry)
        wrong = call(method, path + query, headers, body)
        label = method + ' ' + entry['path']
        for name, (status, text) in (('none', anonymous), ('wrong', wrong)):
            if status >= 500 and status != 503:
                faults.append(label + ' ' + name + ' ' + str(status))
            if any(marker in text for marker in PRIVATE_MARKERS):
                leaks.append(label + ' ' + name)
        if entry['boundary'] == 'handler-session':
            # A wrong session token is no session: the anonymous answer, and
            # when that answer succeeds it carries no session (null or empty).
            if wrong[0] != anonymous[0] or (anonymous[0] < 300 and (wrong[1] != anonymous[1] or anonymous[1].strip() not in ('', 'null'))):
                successes.append(label + ' wrong-token ' + str(wrong[0]) + ' anonymous ' + str(anonymous[0]))
        else:
            for name, (status, _) in (('none', anonymous), ('wrong', wrong)):
                if status not in DENIED:
                    successes.append(label + ' ' + name + ' ' + str(status))
        if entry['boundary'] == 'provider-verification-challenge' and 'synthetic-challenge' in wrong[1]:
            successes.append(label + ' echoed the challenge for a wrong verify token')
    check('every registered deny-boundary public route refuses a caller without its capability and with a wrong one (' + str(len(routes)) + ' routes) ' + ', '.join(successes), not successes)
    check('no deny-boundary public route answers a missing or wrong capability with a server fault ' + ', '.join(faults), not faults)
    check('no deny-boundary public route discloses fixture-private content ' + ', '.join(leaks), not leaks)

    # The capability-path boundary on a real record: the identifier is a random
    # UUIDv4 the server generated; another random one and a sequential one fail.
    status, text = call('POST', '/marketplace/cart', None, {})
    check('marketplace cart intake creates a cart ' + str(status), status == 200)
    cart_id = json.loads(text)['mcCartId']
    parsed = uuid.UUID(cart_id)
    check('marketplace cart identifier is a random version-4 UUID', parsed.version == 4 and parsed.variant == uuid.RFC_4122)
    real, other, sequential = call('GET', '/marketplace/cart/' + cart_id), call('GET', '/marketplace/cart/' + str(uuid.uuid4())), call('GET', '/marketplace/cart/1')
    check('marketplace cart is readable only through its own identifier ' + str((real[0], other[0], sequential[0])),
          real[0] == 200 and json.loads(real[1])['mcCartId'] == cart_id and other[0] == 404 and sequential[0] == 400)
    item = call('POST', '/marketplace/cart/' + str(uuid.uuid4()) + '/items', None, {'listingId': str(uuid.uuid4()), 'quantity': 1})
    check('marketplace cart write through another identifier is refused ' + str(item[0]), item[0] in DENIED)
    status, body = call('GET', '/health')
    check('backend healthy after the public route sweep', status == 200 and json.loads(body).get('db') == 'ok')
