import assert from 'node:assert/strict';
import { EventEmitter } from 'node:events';
import { PassThrough } from 'node:stream';
import test from 'node:test';
import { downloadArtistImage, isPublicImageAddress, publicImageUrl } from '../artist-image-download.mjs';

test('rejects credentials, internal addresses, encoded local IPs and reserved IPv6', () => {
  for (const url of ['http://official.example/a.jpg', 'https://user:password@official.example/a.jpg',
    'https://127.1/a.jpg', 'https://2130706433/a.jpg', 'https://169.254.169.254/a.jpg',
    'https://[::ffff:127.0.0.1]/a.jpg', 'https://[2002:7f00:1::]/a.jpg', 'https://official.example:8443/a.jpg']) {
    assert.throws(() => publicImageUrl(url));
  }
  for (const address of ['10.0.0.1', '192.168.1.1', '172.16.0.1', '100.64.0.1', '::1', 'fe80::1', 'fc00::1', '2001:db8::1']) {
    assert.equal(isPublicImageAddress(address), false, address);
  }
  assert.equal(isPublicImageAddress('93.184.216.34'), true);
  assert.equal(isPublicImageAddress('2606:4700:4700::1111'), true);
});

function transport(replies, observed = [], addresses = [{ address: '93.184.216.34', family: 4 }]) {
  return {
    lookup: (_host, _options, callback) => callback(null, addresses),
    request: (url, options, respond) => {
      observed.push({ url: String(url), options });
      const connection = new EventEmitter();
      connection.end = () => options.lookup(url.hostname, { all: true }, (error) => {
        if (error) { queueMicrotask(() => connection.emit('error', error)); return; }
        const reply = replies.shift();
        const response = new PassThrough();
        response.socket = { remoteAddress: reply.remoteAddress ?? addresses[0].address };
        response.statusCode = reply.status ?? 200;
        response.headers = reply.headers ?? {};
        respond(response);
        if (!response.destroyed) { for (const chunk of reply.chunks ?? [Buffer.from('image')]) response.write(chunk); response.end(); }
      });
      return connection;
    },
  };
}

test('pins all DNS results to the connection and rejects a mixed public/private answer', async () => {
  await assert.rejects(downloadArtistImage('https://official.example/image.jpg', transport([], [], [
    { address: '93.184.216.34', family: 4 }, { address: '127.0.0.1', family: 4 },
  ])), /non-public/);
  await assert.rejects(downloadArtistImage('https://official.example/image.jpg', transport([{ remoteAddress: '1.1.1.1' }])), /did not match/);
});

test('validates every redirect before a request and never forwards credentials', async () => {
  const observed = [];
  await assert.rejects(downloadArtistImage('https://official.example/image.jpg', transport([
    { status: 302, headers: { location: 'https://127.0.0.1/private' } },
  ], observed)), /not public/);
  assert.equal(observed.length, 1);
  assert.equal(observed[0].options.agent, false);
  assert.equal(observed[0].options.headers.Authorization, undefined);
});

test('bounds redirects, declared size, streamed bytes and content encoding', async () => {
  const redirect = { status: 302, headers: { location: '/again.jpg' } };
  await assert.rejects(downloadArtistImage('https://official.example/a.jpg', transport([redirect, redirect, redirect, redirect])), /redirect limit/);
  await assert.rejects(downloadArtistImage('https://official.example/a.jpg', { ...transport([{ headers: { 'content-length': '20' } }]), maxBytes: 10 }), /size or encoding/);
  await assert.rejects(downloadArtistImage('https://official.example/a.jpg', { ...transport([{ chunks: [Buffer.alloc(6), Buffer.alloc(6)] }]), maxBytes: 10 }), /streaming byte limit/);
  await assert.rejects(downloadArtistImage('https://official.example/a.jpg', transport([{ headers: { 'content-encoding': 'gzip' } }])), /size or encoding/);
  assert.equal((await downloadArtistImage('https://official.example/a.jpg', transport([{}]))).toString(), 'image');
});
