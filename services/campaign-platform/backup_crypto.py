"""Versioned streaming envelope: RSA-OAEP/SHA256 + per-block AES-256-GCM.

Recovery uses the private key held by the administrator. The executor stores only
its public recipient; per-upload materials are sealed by the existing vault.
"""
import base64
import hashlib
import io
import json
import os
import struct
from pathlib import Path

from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import padding, rsa
from cryptography.hazmat.primitives.ciphers.aead import AESGCM

MAGIC = b'TDFCBK1\n'
BLOCK = 1024 * 1024


def b64(value):return base64.b64encode(value).decode('ascii')
def unb64(value):return base64.b64decode(value, validate=True)


def fingerprint(public_key):
    der = public_key.public_bytes(serialization.Encoding.DER,
                                 serialization.PublicFormat.SubjectPublicKeyInfo)
    return hashlib.sha256(der).hexdigest()


def recovery_key():
    key = rsa.generate_private_key(public_exponent=65537, key_size=3072)
    public = key.public_key().public_bytes(serialization.Encoding.PEM,
                                          serialization.PublicFormat.SubjectPublicKeyInfo).decode()
    private = key.private_bytes(serialization.Encoding.PEM,
                                serialization.PrivateFormat.PKCS8,
                                serialization.NoEncryption()).decode()
    return {'public_key': public, 'private_key': private,
            'fingerprint': fingerprint(key.public_key())}


class EncryptedSource:
    """Random-access encrypted bytes without a second full-size local copy.

    Each source block is hashed before first upload and rechecked on every read.
    Retrying therefore cannot reuse a GCM nonce with changed plaintext.
    """
    def __init__(self, source, public_pem, expected_sha, expected_size, material=None):
        self.source = Path(source)
        self.size = expected_size
        public = serialization.load_pem_public_key(public_pem.encode())
        if not isinstance(public, rsa.RSAPublicKey) or public.key_size < 3072:
            raise ValueError('Unsupported recovery recipient')
        if self.source.stat().st_size != self.size:
            raise ValueError('Source size changed')
        if material is None:
            digest = hashlib.sha256()
            blocks = []
            with self.source.open('rb') as stream:
                for block in iter(lambda: stream.read(BLOCK), b''):
                    digest.update(block)
                    blocks.append(hashlib.sha256(block).hexdigest())
            if digest.hexdigest() != expected_sha:
                raise ValueError('Source checksum changed')
            if not blocks:blocks.append(hashlib.sha256(b'').hexdigest())
            key = os.urandom(32)
            nonce = os.urandom(8)
            wrapped = public.encrypt(key, padding.OAEP(mgf=padding.MGF1(hashes.SHA256()),
                                                       algorithm=hashes.SHA256(), label=None))
            header = {'version': 1, 'size': self.size, 'sha256': expected_sha,
                      'block_size': BLOCK, 'recipient': fingerprint(public),
                      'wrapped_key': b64(wrapped), 'nonce_prefix': b64(nonce)}
            material = {'header': header, 'key': b64(key), 'blocks': blocks}
        self.material = material
        self.header = material['header']
        if (self.header['recipient'] != fingerprint(public) or self.header['size'] != self.size
                or self.header['sha256'] != expected_sha or self.header['block_size'] != BLOCK
                or len(material['blocks']) != max(1, (self.size + BLOCK - 1) // BLOCK)):
            raise ValueError('Upload material does not match this immutable object')
        encoded = json.dumps(self.header, sort_keys=True, separators=(',', ':')).encode()
        self.prefix = MAGIC + struct.pack('>I', len(encoded)) + encoded
        self.aad = hashlib.sha256(self.prefix).digest()
        self.aes = AESGCM(unb64(material['key']))
        self.nonce = unb64(self.header['nonce_prefix'])
        if len(self.nonce) != 8:raise ValueError('Invalid nonce prefix')
        self.total = len(self.prefix) + self.size + 20 * len(material['blocks'])

    def read_range(self, offset, length):
        if offset < 0 or length < 0 or offset + length > self.total:
            raise ValueError('Encrypted range outside the object')
        output = bytearray()
        if offset < len(self.prefix):
            part = self.prefix[offset:offset + length]
            output.extend(part)
            offset += len(part)
        with self.source.open('rb') as stream:
            while len(output) < length:
                position = offset - len(self.prefix)
                index, within = divmod(position, BLOCK + 20)
                stream.seek(index * BLOCK)
                plain = stream.read(min(BLOCK, self.size - index * BLOCK))
                if hashlib.sha256(plain).hexdigest() != self.material['blocks'][index]:
                    raise ValueError('Immutable source block changed; upload stopped')
                counter = struct.pack('>I', index)
                aad = self.aad + counter + struct.pack('>I', len(plain))
                cipher = self.aes.encrypt(self.nonce + counter, plain, aad)
                record = struct.pack('>I', len(cipher)) + cipher
                part = record[within:within + length - len(output)]
                if not part:raise ValueError('Invalid encrypted range')
                output.extend(part)
                offset += len(part)
        return bytes(output)


def read_exact(stream, count):
    result = bytearray()
    while len(result) < count:
        block = stream.read(count - len(result))
        if not block:raise ValueError('Truncated backup')
        result.extend(block)
    return bytes(result)


def decrypt_stream(stream, private_pem, sink=None):
    if read_exact(stream, len(MAGIC)) != MAGIC:raise ValueError('Unknown backup format')
    raw_length = read_exact(stream, 4)
    length = struct.unpack('>I', raw_length)[0]
    if not 1 <= length <= 8192:raise ValueError('Invalid backup header')
    encoded = read_exact(stream, length)
    header = json.loads(encoded)
    key = serialization.load_pem_private_key(private_pem, password=None)
    if (not isinstance(key, rsa.RSAPrivateKey) or header['version'] != 1
            or header['block_size'] != BLOCK or header['size'] < 0
            or fingerprint(key.public_key()) != header['recipient']):
        raise ValueError('Recovery key or format does not match')
    secret = key.decrypt(unb64(header['wrapped_key']),
                         padding.OAEP(mgf=padding.MGF1(hashes.SHA256()),
                                      algorithm=hashes.SHA256(), label=None))
    aes = AESGCM(secret)
    nonce = unb64(header['nonce_prefix'])
    if len(nonce) != 8:raise ValueError('Invalid nonce prefix')
    aad = hashlib.sha256(MAGIC + raw_length + encoded).digest()
    digest = hashlib.sha256()
    written = 0
    for index in range(max(1, (header['size'] + BLOCK - 1) // BLOCK)):
        expected = min(BLOCK, header['size'] - written)
        count = struct.unpack('>I', read_exact(stream, 4))[0]
        if count != expected + 16:raise ValueError('Invalid encrypted block length')
        counter = struct.pack('>I', index)
        plain = aes.decrypt(nonce + counter, read_exact(stream, count),
                            aad + counter + struct.pack('>I', expected))
        digest.update(plain)
        written += len(plain)
        if sink is not None:sink.write(plain)
    if stream.read(1) or written != header['size'] or digest.hexdigest() != header['sha256']:
        raise ValueError('Backup integrity mismatch')
    return {'sha256': digest.hexdigest(), 'size': written,
            'recipient': header['recipient'], 'authenticated': True}
