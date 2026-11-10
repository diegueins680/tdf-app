#!/usr/bin/env python3
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch
import urllib.request

spec = importlib.util.spec_from_file_location('conflict_probe', Path(__file__).with_name('interactions')/'conflict_probe.py')
m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)


class Response:
    def __init__(self, code, body=b'', headers=None):
        self.code, self.body = code, body
        self.headers = headers if headers is not None else {'Retry-After': '1', 'Cache-Control': 'no-store'}
    def __enter__(self): return self
    def __exit__(self, *args): pass
    def read(self, maximum): return self.body[:maximum]


class ConflictProbeTests(unittest.TestCase):
    def request(self):
        return urllib.request.Request('http://127.0.0.1:18128/interactions/targets/synthetic/commands',
            data=b'{"requestKey":"fixed","command":{"operation":"comment.remove"}}',
            headers={'Authorization': 'Bearer synthetic', 'Content-Type': 'application/json'})

    def test_retries_preserve_exact_actor_resource_payload_and_key(self):
        request = self.request()
        baseline = (request.full_url, request.data, request.get_header('Authorization'))
        seen = []; responses = iter([Response(503), Response(403, b'forbidden')])
        def opener(sent, timeout):
            seen.append((sent.full_url, sent.data, sent.get_header('Authorization')))
            return next(responses)
        with patch.object(m.urllib.request, 'urlopen', side_effect=opener) as send, \
                patch.object(m.time, 'sleep') as sleep:
            self.assertEqual(m.request_after_conflict(request), (403, 'forbidden'))
            self.assertEqual(send.call_count, 2)
            self.assertEqual(seen, [baseline, baseline])
            self.assertTrue(all(call.args[0] is request for call in send.call_args_list))
            self.assertTrue(all(call.args[0].data == request.data for call in send.call_args_list))
            sleep.assert_called_once_with(1)

    def test_permanent503_never_becomes_success(self):
        with patch.object(m.urllib.request, 'urlopen', return_value=Response(503)) as send, patch.object(m.time, 'sleep'):
            with self.assertRaisesRegex(AssertionError, 'did not recover'): m.request_after_conflict(self.request())
            self.assertEqual(send.call_count, 3)

    def test_unrecognized503_never_retries(self):
        for response in [Response(503, b'other'), Response(503, headers={}),
                         Response(503, headers={'Retry-After': '5', 'Cache-Control': 'no-store'}),
                         Response(503, headers={'Retry-After': '1'})]:
            with self.subTest(response=response), patch.object(m.urllib.request, 'urlopen', return_value=response) as send:
                with self.assertRaisesRegex(AssertionError, 'Unexpected503'): m.request_after_conflict(self.request())
                send.assert_called_once()

    def test_revoked_authorization_is_not_retried_or_converted_to_denial_success(self):
        with patch.object(m.urllib.request, 'urlopen', side_effect=[Response(503), Response(401, b'revoked')]) as send, \
                patch.object(m.time, 'sleep'):
            self.assertEqual(m.request_after_conflict(self.request()), (401, 'revoked'))
            self.assertEqual(send.call_count, 2)

    def test_other_failures_and_transport_errors_remain_failures(self):
        with patch.object(m.urllib.request, 'urlopen', return_value=Response(500, b'error')) as send:
            self.assertEqual(m.request_after_conflict(self.request()), (500, 'error')); send.assert_called_once()
        with patch.object(m.urllib.request, 'urlopen', side_effect=OSError('offline')) as send:
            with self.assertRaises(OSError): m.request_after_conflict(self.request())
            send.assert_called_once()

    def test_deadline_exhaustion_prevents_another_attempt(self):
        with patch.object(m.time, 'monotonic', side_effect=[0, 0, 4.1, 4.1]), \
                patch.object(m.urllib.request, 'urlopen', return_value=Response(503)) as send, patch.object(m.time, 'sleep') as sleep:
            with self.assertRaisesRegex(AssertionError, 'did not recover'): m.request_after_conflict(self.request())
            send.assert_called_once(); sleep.assert_not_called()

    def test_late_final_denial_is_never_credited(self):
        for code in (403, 429):
            with self.subTest(code=code), patch.object(m.time, 'monotonic', side_effect=[0, 0, 6]), \
                    patch.object(m.urllib.request, 'urlopen', return_value=Response(code)) as send:
                with self.assertRaisesRegex(AssertionError, 'deadline exhausted after response'):
                    m.request_after_conflict(self.request())
                send.assert_called_once()

    def test_real_httperror_path_retains_final_business_denial(self):
        import io
        error = m.urllib.error.HTTPError('http://127.0.0.1', 503, 'conflict',
                {'Retry-After': '1', 'Cache-Control': 'no-store'}, io.BytesIO(b''))
        with patch.object(m.urllib.request, 'urlopen', side_effect=[error, Response(429, b'rate_limited')]), patch.object(m.time, 'sleep'):
            self.assertEqual(m.request_after_conflict(self.request()), (429, 'rate_limited'))


if __name__ == '__main__': unittest.main()
