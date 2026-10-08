// Native browser audio, using the production pipeline and only generated media.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { createReadStream, mkdtempSync, readFileSync, rmSync, statSync } from 'node:fs';
import { createServer } from 'node:http';
import { tmpdir } from 'node:os';
import { resolve, join } from 'node:path';

export default async function setup() {
  const runtime = mkdtempSync(join(tmpdir(), 'tdf-native-music-'));
  let server;
  const close = async () => {
    if (server?.listening) {
      server.closeAllConnections();
      await new Promise((done) => server.close(done));
    }
    rmSync(runtime, { recursive: true, force: true });
  };
  try {
    const env = { PATH: process.env.PATH, HOME: process.env.HOME, LANG: 'C', LC_ALL: 'C' };
    const run = (program, args) => execFileSync(program, args, { env, encoding: 'utf8', timeout: 180000 });
    run('ffmpeg', ['-nostdin', '-v', 'error', '-f', 'lavfi', '-i', 'sine=frequency=440:sample_rate=48000:duration=24',
      '-ac', '2', '-c:a', 'pcm_s24le', join(runtime, 'master.wav')]);
    console.log(run('bash', [resolve('scripts/process-music-release-audio.sh'), join(runtime, 'master.wav'), join(runtime, 'audio')]));
    const manifest = JSON.parse(readFileSync(join(runtime, 'audio/manifest.json'), 'utf8'));
    const files = {};
    for (const [quality, name, mediaType] of [
      ['low', 'stream-low.m4a', 'audio/mp4'], ['high', 'stream-high.m4a', 'audio/mp4'],
      ['lossless', 'stream-lossless.flac', 'audio/flac'],
    ]) {
      const path = join(runtime, 'audio', name);
      const sha256 = createHash('sha256').update(readFileSync(path)).digest('hex');
      assert.equal(manifest.derivatives.find((item) => item.path === name)?.sha256, sha256);
      files[quality] = { path, sha256, byteSize: statSync(path).size, mediaType };
    }
    server = createServer((req, res) => {
      const url = new URL(req.url, 'http://127.0.0.1');
      const match = url.pathname.match(/^\/media\/(first|second)\/(low|high|lossless)$/);
      const file = match && files[match[2]];
      if (!file || !['GET', 'HEAD'].includes(req.method)) { res.writeHead(404).end(); return; }
      const headers = { 'Content-Type': file.mediaType, 'Accept-Ranges': 'bytes', 'Cache-Control': 'no-store',
        'Access-Control-Allow-Origin': '*', 'Access-Control-Expose-Headers': 'Content-Range' };
      let start = 0, end = file.byteSize - 1;
      if (req.headers.range) {
        const range = /^bytes=(\d+)-(\d*)$/.exec(req.headers.range);
        start = range ? Number(range[1]) : -1;
        end = range?.[2] ? Math.min(Number(range[2]), end) : end;
        if (start < 0 || start > end || !Number.isSafeInteger(start) || !Number.isSafeInteger(end)) {
          res.writeHead(416, { ...headers, 'Content-Range': `bytes */${file.byteSize}` }).end(); return;
        }
        headers['Content-Range'] = `bytes ${start}-${end}/${file.byteSize}`;
      }
      res.writeHead(req.headers.range ? 206 : 200, { ...headers, 'Content-Length': end - start + 1 });
      if (req.method === 'HEAD') { res.end(); return; }
      const stream = createReadStream(file.path, { start, end });
      stream.on('error', () => res.destroy());
      res.on('close', () => stream.destroy());
      stream.pipe(res);
    });
    await new Promise((done, reject) => { server.once('error', reject); server.listen(0, '127.0.0.1', done); });
    process.env.TDF_NATIVE_MUSIC_FIXTURE = JSON.stringify({
      endpoint: `http://127.0.0.1:${server.address().port}`,
      files: Object.fromEntries(Object.entries(files).map(([quality, { path: _path, ...file }]) => [quality, file])),
      masterSha256: manifest.source.sha256,
    });
    console.log('Native audio fixture ready: verified AAC/FLAC from the real pipeline; loopback HTTP only.');
    return close;
  } catch (error) { await close(); throw error; }
}
