#!/usr/bin/env python3
"""AUTH-WEBHOOK-001: every Meta signed-callback route is served by a function
that verifies the X-Hub-Signature-256 HMAC before decoding the body.

The routes come from formal/system/public-routes.json (boundary
signed-provider-callback with header X-Hub-Signature-256). Each is mapped to
the top-level server function that serves it; an alias definition
(`a = b`) is followed. The gate requires `verifyMetaWebhookSignature` to appear
in that function's body before the first JSON decode. It is a structural check
of the source, not an HTTP-level denial test.
"""
import json
import os
import re
import sys
from pathlib import Path

ROOT = Path(os.environ.get('TDF_WEBHOOK_GATE_ROOT') or Path(__file__).resolve().parents[1])

SERVERS = {
    '/webhooks/whatsapp': ('tdf-hq/src/TDF/Server.hs', 'whatsappWebhookServer'),
    '/hooks/whatsapp': ('tdf-hq/src/TDF/Server.hs', 'whatsappHooksServer'),
    '/instagram/webhook': ('tdf-hq/src/TDF/ServerExtra.hs', 'instagramWebhookServer'),
    '/facebook/webhook': ('tdf-hq/src/TDF/ServerExtra.hs', 'facebookWebhookServer'),
}
VERIFIER = 'verifyMetaWebhookSignature'
DECODE = re.compile(r'\b(eitherDecode|decode|eitherDecodeStrict)\b')


def top_level_body(path, name, depth=0):
    lines = (ROOT / path).read_text().split('\n')
    starts = [i for i, l in enumerate(lines) if re.match(rf'^{re.escape(name)}\b', l) and '::' not in l]
    if not starts:
        raise SystemExit(f'{path}: top-level definition {name} not found')
    start = starts[0]
    end = next((j for j in range(start + 1, len(lines))
                if re.match(r'^[a-z][A-Za-z0-9_\']*\s*(::|=|\s)', lines[j]) and not lines[j].startswith(name)
                and re.match(r'^[a-z]', lines[j])), len(lines))
    body = '\n'.join(lines[start:end])
    alias = re.fullmatch(rf'{re.escape(name)}\s*=\s*([a-z][A-Za-z0-9_\']*)\s*', lines[start].strip())
    if alias and depth < 3:
        return top_level_body(path, alias.group(1), depth + 1)
    return body


def main():
    routes = json.loads((ROOT / 'formal/system/public-routes.json').read_text())['routes']
    meta = [r for r in routes if r.get('boundary') == 'signed-provider-callback'
            and r.get('header') == 'X-Hub-Signature-256']
    failures = []
    for route in meta:
        mapping = SERVERS.get(route['path'])
        if not mapping:
            failures.append(f"{route['method']} {route['path']}: no server mapping")
            continue
        path, name = mapping
        body = top_level_body(path, name)
        verify_at = body.find(VERIFIER)
        decode = DECODE.search(body)
        decode_at = decode.start() if decode else len(body)
        if verify_at < 0:
            failures.append(f"{route['path']}: {name} never calls {VERIFIER}")
        elif verify_at > decode_at:
            failures.append(f"{route['path']}: {name} decodes the body before {VERIFIER}")
    unmapped = set(SERVERS) - {r['path'] for r in meta}
    if unmapped:
        failures.append('mapped paths missing from public-routes.json: ' + ', '.join(sorted(unmapped)))
    print(f'Meta webhook signature gate: {len(meta)} signed routes, {len(failures)} failures')
    for failure in failures:
        print('  ' + failure)
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
