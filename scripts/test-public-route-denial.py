#!/usr/bin/env python3
"""Exercise the denial sweep against a local HTTP fault fixture (AUTH-PUBLIC-001)."""
import json
import tempfile
import threading
import unittest
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlsplit

from lib.public_routes_conformance import verify_public_route_denial


class DenialSweepTest(unittest.TestCase):
    def sweep(self, boundary, status, path='/probe', method='GET'):
        cart = str(uuid.uuid4())

        class Handler(BaseHTTPRequestHandler):
            def respond(self):
                requested = urlsplit(self.path).path
                code, body = 401, {}
                if requested == path.replace('{orderId}', '1') and self.command == method:
                    code = status
                elif requested == '/marketplace/cart' and self.command == 'POST':
                    code, body = 200, {'mcCartId': cart}
                elif requested == '/marketplace/cart/' + cart:
                    code, body = 200, {'mcCartId': cart}
                elif requested.startswith('/marketplace/cart/'):
                    code = 400 if requested.endswith('/1') else 404
                elif requested == '/health':
                    code, body = 200, {'db': 'ok'}
                payload = json.dumps(body).encode()
                self.send_response(code)
                self.send_header('Content-Length', str(len(payload)))
                self.end_headers()
                self.wfile.write(payload)

            do_GET = respond
            do_POST = respond

            def log_message(self, *_args):
                pass

        routes = [{'method': 'GET', 'path': f'/filler/{i}', 'boundary': 'capability-header'}
                  for i in range(79)]
        routes.append({'method': method, 'path': path, 'boundary': boundary})
        checks = []
        with tempfile.TemporaryDirectory(prefix='tdf-denial-test-') as directory:
            root = Path(directory)
            (root / 'formal/system').mkdir(parents=True)
            (root / 'formal/system/public-routes.json').write_text(json.dumps({'routes': routes}))
            with ThreadingHTTPServer(('127.0.0.1', 0), Handler) as server:
                thread = threading.Thread(target=server.serve_forever, daemon=True)
                thread.start()
                try:
                    verify_public_route_denial(root, server.server_port,
                                              lambda label, ok: checks.append((label, ok)))
                finally:
                    server.shutdown()
                    thread.join(timeout=5)
        failures = [label for label, ok in checks if not ok]
        return failures

    def test_ordinary_session_and_capability_503_are_faults(self):
        for boundary in ('handler-session', 'capability-header', 'capability-path', 'operator-secret'):
            with self.subTest(boundary=boundary):
                failures = self.sweep(boundary, 503)
                self.assertEqual(len(failures), 1, failures)
                self.assertIn('server fault', failures[0])
                self.assertIn('GET /probe none 503', failures[0])
                self.assertIn('GET /probe wrong 503', failures[0])

    def test_provider_503_is_permitted_but_other_server_errors_are_not(self):
        for boundary in ('signed-provider-callback', 'provider-verification-challenge'):
            with self.subTest(boundary=boundary):
                self.assertEqual(self.sweep(boundary, 503), [])
                failures = self.sweep(boundary, 500)
                self.assertEqual(len(failures), 1, failures)
                self.assertIn('server fault', failures[0])

    def test_named_provider_gated_route_keeps_its_exception(self):
        path = '/services/storefront/order/{orderId}/stripe/payment-intent'
        self.assertEqual(self.sweep('capability-header', 503, path, 'POST'), [])
        failures = self.sweep('capability-header', 500, path, 'POST')
        self.assertEqual(len(failures), 1, failures)
        self.assertIn('server fault', failures[0])

    def test_ordinary_denial_is_still_accepted(self):
        self.assertEqual(self.sweep('handler-session', 401), [])


if __name__ == '__main__':
    unittest.main()
