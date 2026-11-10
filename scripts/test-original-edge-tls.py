#!/usr/bin/env python3
"""Real local TLS sockets with synthetic CA material; production probe text unchanged.

Only the destination socket is mapped from fixed loopback443 to the owned free
port. The default trust loader is supplied the fixture CA explicitly. No live
container, host trust store, external network or production key is used here.
"""
import contextlib
import http.server
import importlib.util
import io
import json
from pathlib import Path
import socket
import ssl
import subprocess
import sys
import tempfile
import threading
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('tls_application',ROOT/'ops/hetzner/original-application-recovery.py')
a=importlib.util.module_from_spec(spec);spec.loader.exec_module(a)


class TLSControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp=tempfile.TemporaryDirectory(prefix='tdf-edge-tls-');cls.addClassCleanup(cls.temp.cleanup)
        cls.root=Path(cls.temp.name)
        def run(args):subprocess.run(['openssl',*args],cwd=cls.root,check=True,capture_output=True,timeout=30)
        run(['req','-x509','-newkey','rsa:2048','-nodes','-days','1','-keyout','ca.key','-out','ca.crt','-subj','/CN=Synthetic edge test','-addext','basicConstraints=critical,CA:TRUE','-addext','keyUsage=critical,keyCertSign,cRLSign'])
        for prefix,hostname in [('valid','api.tdfrecords.net'),('wrong','wrong.invalid')]:
            run(['req','-newkey','rsa:2048','-nodes','-keyout',prefix+'.key','-out',prefix+'.csr','-subj','/CN='+hostname])
            (cls.root/(prefix+'.ext')).write_text('subjectAltName=DNS:'+hostname+'\nbasicConstraints=CA:FALSE\nextendedKeyUsage=serverAuth\n')
            run(['x509','-req','-in',prefix+'.csr','-CA','ca.crt','-CAkey','ca.key','-CAcreateserial','-days','1','-out',prefix+'.crt','-extfile',prefix+'.ext'])
        for p in cls.root.glob('*.key'):p.chmod(0o600)

    def probe(self,certificate='valid',trusted=True,program=None):
        seen=[]
        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self,*args):pass
            def do_GET(self):
                seen.append((self.command,self.path,self.headers.get('Host')))
                body=b'{"status":"ok","db":"ok"}'
                self.send_response(200);self.send_header('Content-Length',str(len(body)));self.end_headers();self.wfile.write(body)
        server=http.server.HTTPServer(('127.0.0.1',0),Handler)
        context=ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(str(self.root/(certificate+'.crt')),str(self.root/(certificate+'.key')))
        sni=[];context.set_servername_callback(lambda sock,name,ctx:sni.append(name))
        server.socket=context.wrap_socket(server.socket,server_side=True)
        thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
        connect=socket.create_connection
        create_context=ssl.create_default_context
        def loopback(address,*args,**kwargs):
            self.assertEqual(address,('127.0.0.1',443))
            return connect(('127.0.0.1',server.server_port),*args,**kwargs)
        def trust():return create_context(cafile=str(self.root/'ca.crt') if trusted else None)
        output=io.StringIO()
        try:
            with patch.object(socket,'create_connection',side_effect=loopback),patch.object(ssl,'create_default_context',side_effect=trust),patch.object(sys,'argv',['probe','/health']),contextlib.redirect_stdout(output):
                exec(compile(program or a.EDGE_PROBE,'actual-edge-probe','exec'),{})
            self.assertEqual(sni,['api.tdfrecords.net'])
            value=json.loads(output.getvalue())
            if value['valid']:self.assertEqual(seen,[('GET','/health','api.tdfrecords.net')])
            return value
        finally:
            server.shutdown();server.server_close();thread.join(timeout=3)
            self.assertFalse(thread.is_alive())

    def test_trusted_certificate_uses_fixed_sni_host_and_socket(self):
        value=self.probe();self.assertTrue(value['valid']);self.assertEqual(value['code'],200)

    def test_untrusted_certificate_is_permanent_failure(self):
        value=self.probe(trusted=False)
        self.assertEqual(value,{'code':None,'valid':False,'metadata':{},'transportUnavailable':False})

    def test_wrong_hostname_is_permanent_failure(self):
        value=self.probe(certificate='wrong')
        self.assertEqual(value,{'code':None,'valid':False,'metadata':{},'transportUnavailable':False})

    def test_removing_hostname_verification_defeats_the_negative_control(self):
        original='context=ssl.create_default_context()'
        self.assertEqual(a.EDGE_PROBE.count(original),1)
        mutant=a.EDGE_PROBE.replace(original,"context=(lambda c: (setattr(c,'check_hostname',False),c)[1])(ssl.create_default_context())")
        value=self.probe(certificate='wrong',program=mutant)
        self.assertTrue(value['valid'],'controlled insecure client must expose the bad-host counterexample')
        self.assertEqual(value['code'],200)


if __name__=='__main__':unittest.main(verbosity=2)
