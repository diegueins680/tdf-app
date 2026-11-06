#!/usr/bin/env python3
import importlib.util
import io
import json
import math
from pathlib import Path
import shutil
import socket
import struct
import unittest
from unittest.mock import patch
import wave

spec = importlib.util.spec_from_file_location('media_health', Path(__file__).with_name('music-media-health.py'))
health = importlib.util.module_from_spec(spec)
spec.loader.exec_module(health)

class Response:
    def __init__(self, code=200, headers=None, body=b'RIFF'):
        self.status, self.headers, self.body = code, headers or {}, io.BytesIO(body)
    def getheader(self, name, default=None): return self.headers.get(name, default)
    def read1(self, size): return self.body.read(size)

class Connection:
    responses = []
    instances = []
    def __init__(self, host, **kwargs):
        self.host, self.sock, self.headers = host, None, None
        self.__class__.instances.append(self)
    def request(self, method, path, headers):
        self.headers = headers
        self._create_connection((self.host,443), timeout=1)
    def getresponse(self): return self.__class__.responses.pop(0)
    def close(self): pass

class Checks(unittest.TestCase):
    def test_rejects_nonpublic_and_transition_addresses(self):
        for address in ['127.0.0.1','10.1.2.3','169.254.169.254','192.168.0.1','0.0.0.0','::1','fc00::1',
                        'fe80::1','::ffff:127.0.0.1','2002:7f00:1::','2001::1','224.0.0.1']:
            with self.subTest(address=address): self.assertFalse(health.safe_address(address))
        self.assertTrue(health.safe_address('1.1.1.1'))
    def test_rejects_unsafe_url_shapes(self):
        for url in ['http://example.com','file:///etc/passwd','https://u:p@example.com/a',
                    'https://example.com:8443/a','https://example.com/a\r\nX: secret']:
            with self.subTest(url=url), self.assertRaises(health.ProbeFailure): health.validated_url(url)
    def test_mixed_dns_answers_fail_closed(self):
        result = type('Result',(),{'stdout':json.dumps(['1.1.1.1','127.0.0.1'])})()
        with patch.object(health.subprocess,'run',return_value=result), self.assertRaises(health.ProbeFailure):
            health.public_addresses('example.com')
    def fetch(self, responses, resolver=lambda host: ['1.1.1.1'], signed=False):
        Connection.responses, Connection.instances = responses, []
        with patch.object(health.socket,'create_connection',return_value=None) as connect:
            result = health.fetch_prefix('https://media.example/a', resolver=resolver, connection=Connection,
                                         signed=signed,env={})
            self.assertEqual(connect.call_args.args[0], ('1.1.1.1',443))
            self.assertEqual(Connection.instances[0].host,'media.example')
            return result
    def test_pins_dns_and_bounds_a_server_ignoring_ranges(self):
        data, complete = self.fetch([Response(body=b'x'*(health.MAX_BYTES+100))])
        self.assertEqual(len(data),health.MAX_BYTES);self.assertFalse(complete)
    def test_checks_redirect_target_again(self):
        def resolve(host): return ['127.0.0.1'] if host=='private.example' else ['1.1.1.1']
        with self.assertRaises(health.ProbeFailure) as error:
            self.fetch([Response(302,{'Location':'https://private.example/secret'})],resolver=resolve)
        self.assertEqual(error.exception.reason,'unsafe_url')
    def test_bounded_redirect_loop(self):
        with self.assertRaises(health.ProbeFailure):
            self.fetch([Response(302,{'Location':'/loop'}) for _ in range(4)])
        self.assertEqual(len(Connection.instances),4)
    def test_transient_and_permanent_http_classes(self):
        for code, reason, permanent in [(401,'authentication',False),(403,'authentication',False),
                                       (404,'missing',True),(410,'missing',True),(429,'server',False),(503,'server',False)]:
            with self.subTest(code=code), self.assertRaises(health.ProbeFailure) as error:
                self.fetch([Response(code)])
            self.assertEqual((error.exception.reason,error.exception.permanent),(reason,permanent))
    def test_html_is_not_audio(self):
        with self.assertRaises(health.ProbeFailure) as error:
            self.fetch([Response(headers={'Content-Type':'text/html'},body=b'<html>login</html>')])
        self.assertEqual(error.exception.reason,'unsupported')
    def test_timeout_is_temporary(self):
        with patch.object(health,'fetch_prefix',side_effect=socket.timeout):
            self.assertEqual(health.probe('https://media.example/a'),('temporary','timeout'))
    def test_truncated_sample_is_inconclusive(self):
        with patch.object(health,'fetch_prefix',return_value=(b'not audio',False)):
            self.assertEqual(health.probe('https://media.example/a'),('temporary','inconclusive'))
    @unittest.skipUnless(shutil.which('ffprobe') and shutil.which('ffmpeg'),'Native ffmpeg/ffprobe unavailable')
    def test_real_native_decode_and_invalid_full_file(self):
        buffer=io.BytesIO()
        with wave.open(buffer,'wb') as wav:
            wav.setnchannels(1);wav.setsampwidth(2);wav.setframerate(8000)
            wav.writeframes(b''.join(struct.pack('<h',int(3000*math.sin(i*2*math.pi*440/8000))) for i in range(4000)))
        with patch.object(health,'fetch_prefix',return_value=(buffer.getvalue(),True)):
            self.assertEqual(health.probe('https://media.example/a'),('valid','decodable'))
        with patch.object(health,'fetch_prefix',return_value=(b'not audio',True)):
            self.assertEqual(health.probe('https://media.example/a'),('permanent','unsupported'))

if __name__=='__main__': unittest.main()
