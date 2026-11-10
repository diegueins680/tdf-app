#!/usr/bin/env python3
"""Real private-file/socket controls; synthetic bytes, no remote or production IO."""
import hashlib
import importlib.util
import os
from pathlib import Path
import socket
import struct
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('transfer', Path(__file__).resolve().parent.parent/'ops/hetzner/recovery-transfer.py')
t = importlib.util.module_from_spec(spec); spec.loader.exec_module(t)
NONCE = 'a'*32


class TransferTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(); self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.content = bytes(range(256))*1024
        self.expected = {'bytes': len(self.content), 'sha256': hashlib.sha256(self.content).hexdigest()}
        self.source = self.root/'ciphertext'; self.source.write_bytes(self.content); self.source.chmod(0o600)
        self.returned = self.root/'retrieved'; self.stored = self.root/'off-host'

    def sockets(self):
        left, right = socket.socketpair()
        self.addCleanup(left.close); self.addCleanup(right.close)
        return left, right

    def receive_raw(self, raw, expected=None, nonce=NONCE):
        left, right = self.sockets()
        right.settimeout(0.5)
        failures = []
        def writer():
            try: right.sendall(raw); right.shutdown(socket.SHUT_WR)
            except (BrokenPipeError, OSError) as error: failures.append(error)
        thread = threading.Thread(target=writer); thread.start()
        try:
            with t.Channel(left.fileno(), left.fileno(), 2) as channel:
                return t.receive(channel, self.stored, nonce, expected or self.expected, 'offer')
        finally:
            for endpoint in (left, right):
                try: endpoint.shutdown(socket.SHUT_RDWR)
                except OSError: pass  # macOS reports an already closed peer here.
            thread.join(2)
            self.assertFalse(thread.is_alive())

    def frame(self, *, header=None, body=None):
        header = header or {**t.binding(NONCE, self.expected), 'kind': 'offer'}
        data = t.canonical(header)
        return struct.pack('!I', len(data))+data+(self.content if body is None else body)

    def test_durable_round_trip_reopens_each_copy_and_restores_channel_flags(self):
        left, right = self.sockets(); results, failures = [], []
        def receiver():
            try:
                with t.Channel(right.fileno(), right.fileno(), 3) as channel:
                    results.append(t.retain_and_return(channel, self.stored, NONCE, self.expected))
            except BaseException as error: failures.append(error)
        thread = threading.Thread(target=receiver); thread.start()
        with patch.object(os, 'fsync', wraps=os.fsync) as sync:
            with t.Channel(left.fileno(), left.fileno(), 3) as channel:
                result = t.round_trip(channel, self.source, self.returned, NONCE, self.expected)
            self.assertGreaterEqual(sync.call_count, 4)
        thread.join(3); self.assertFalse(thread.is_alive()); self.assertFalse(failures)
        self.assertEqual(result['status'], 'ciphertext-round-trip-verified')
        self.assertEqual(results[0]['status'], 'ciphertext-retained-and-returned')
        for path in (self.stored, self.returned):
            self.assertEqual(path.read_bytes(), self.content)
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
        self.assertFalse(result['decrypted']); self.assertFalse(result['restored'])
        self.assertNotIn('offHost', result)
        self.assertTrue(os.get_blocking(left.fileno())); self.assertTrue(os.get_blocking(right.fileno()))

    def test_modified_or_truncated_stream_retains_only_private_failed_output(self):
        for body in (self.content[:-1], b'X'+self.content[1:]):
            with self.subTest(size=len(body)):
                self.stored = self.root/('failed-'+str(len(body)))
                with self.assertRaises(ValueError): self.receive_raw(self.frame(body=body))
                self.assertEqual(self.stored.stat().st_mode & 0o777, 0o600)

    def test_wrong_nonce_hash_count_kind_or_extra_field_rejected_before_output(self):
        for field, value in [('nonce','b'*32), ('sha256','b'*64), ('bytes',1),
                             ('schemaVersion',True), ('bytes',float(len(self.content))),
                             ('kind','retrieval'), ('extra','x')]:
            header = {**t.binding(NONCE, self.expected), 'kind':'offer', field:value}
            with self.subTest(field=field), self.assertRaises(ValueError): self.receive_raw(self.frame(header=header))
            self.assertFalse(self.stored.exists())

    def test_duplicate_json_and_oversized_or_truncated_header_rejected(self):
        header = t.canonical({**t.binding(NONCE,self.expected),'kind':'offer'})
        duplicated = b'{"kind":"offer",'+header[1:]
        for raw in (struct.pack('!I',len(duplicated))+duplicated,
                    struct.pack('!I',t.HEADER_LIMIT+1), b'\x00\x00'):
            with self.subTest(raw=raw[:4]), self.assertRaises(ValueError): self.receive_raw(raw)
            self.assertFalse(self.stored.exists())

    def test_existing_output_and_symlink_are_never_overwritten(self):
        self.stored.write_bytes(b'retained')
        with self.assertRaises(FileExistsError): self.receive_raw(self.frame())
        self.assertEqual(self.stored.read_bytes(),b'retained')
        self.stored = self.root/'alias'; self.stored.symlink_to(self.source)
        with self.assertRaises(FileExistsError): self.receive_raw(self.frame())
        self.assertEqual(self.source.read_bytes(),self.content)

    def test_parent_alias_or_nonprivate_directory_rejected(self):
        alias = self.root/'alias'; alias.symlink_to(self.root,target_is_directory=True)
        self.stored=alias/'output'
        with self.assertRaises(OSError): self.receive_raw(self.frame())
        public=self.root/'public';public.mkdir(mode=0o755);public.chmod(0o755)
        self.stored=public/'output'
        with self.assertRaises(ValueError): self.receive_raw(self.frame())

    def test_persistence_or_reread_failure_never_sends_retrieval(self):
        for failed in ('fsync','digest'):
            self.stored=self.root/failed
            left,right=self.sockets(); raw=self.frame()
            def writer():
                try: right.sendall(raw)
                except OSError: pass
            thread=threading.Thread(target=writer);thread.start()
            target=os if failed=='fsync' else t.files
            with patch.object(target,failed,side_effect=OSError('Synthetic disk failure')):
                with t.Channel(left.fileno(),left.fileno(),2) as channel:
                    with self.assertRaises(OSError):t.retain_and_return(channel,self.stored,NONCE,self.expected)
            left.shutdown(socket.SHUT_RDWR);thread.join(2)
            self.assertEqual(right.recv(1),b'')

    def test_invalid_source_rejected_before_header(self):
        left,right=self.sockets()
        for mutation in ('public','hash','alias','hardlink'):
            with self.subTest(mutation=mutation):
                source=self.source; expected=dict(self.expected)
                if mutation=='public':self.source.chmod(0o644)
                elif mutation=='hash':expected['sha256']='f'*64
                elif mutation=='alias':source=self.root/'source-alias';source.symlink_to(self.source)
                else:os.link(self.source,self.root/'hardlink')
                with t.Channel(left.fileno(),left.fileno(),1) as channel:
                    with self.assertRaises((ValueError,OSError)):t.send(channel,source,NONCE,expected,'offer')
                self.source.chmod(0o600)
        right.setblocking(False)
        with self.assertRaises(BlockingIOError):right.recv(1)

    def test_deadline_covers_silent_peer_and_backpressure(self):
        for writing in (False, True):
            left,right=self.sockets();start=time.monotonic()
            with self.subTest(writing=writing),t.Channel(left.fileno(),left.fileno(),0.05) as channel:
                with self.assertRaises(TimeoutError):
                    if writing:
                        while True:channel.write(b'x'*t.BLOCK)
                    else:channel.read(1)
            self.assertLess(time.monotonic()-start,1)
            self.assertTrue(os.get_blocking(left.fileno()))

    def test_source_change_during_transmission_cannot_complete(self):
        left,right=self.sockets();drained=[]
        def drain():
            while True:
                data=right.recv(t.BLOCK)
                if not data:return
                drained.append(len(data))
        thread=threading.Thread(target=drain);thread.start()
        try:
            with t.Channel(left.fileno(),left.fileno(),1) as channel:
                original=channel.send_header
                def mutate(header):
                    original(header);self.source.write_bytes(b'X'+self.content[1:])
                with patch.object(channel,'send_header',side_effect=mutate):
                    with self.assertRaises(ValueError):t.send(channel,self.source,NONCE,self.expected,'offer')
        finally:left.shutdown(socket.SHUT_WR);thread.join(2)

    def test_bounds_and_channel_type_admission(self):
        for expected in ({'bytes':True,'sha256':'a'*64},{'bytes':0,'sha256':'a'*64},
                         {'bytes':t.MAX_BYTES+1,'sha256':'a'*64}, {'bytes':1,'sha256':'A'*64}):
            with self.assertRaises(ValueError):t.binding(NONCE,expected)
        for timeout in (0,-1,301,float('nan')):
            with self.assertRaises(ValueError):t.Channel(0,1,timeout)
        with self.source.open('rb') as handle:
            with self.assertRaises(ValueError):
                with t.Channel(handle.fileno(),handle.fileno()):pass


if __name__=='__main__':unittest.main()
